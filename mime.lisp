(in-package :mime)

(defstruct mime headers body)

(defconstant +crlfcrlf+ #.(format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed))

(defconstant +crlf+ #.(format nil "~C~C" #\Return #\Linefeed))

(defconstant +lwsp+ '(#\Space #\Tab #\Return #\Linefeed))

(defvar *decode-base-64-p* nil
  "Should we decode base64?")

(defvar *duplicate-headers* nil
  "List of header values which may be duplicated across multiple lines")

(defvar *crlf-is-lf-p* nil
  "Is our CRLF actually only a LF?")

(defconstant +extensions+
  '(("html" . "text/html") ("xhtml" . "text/html") ("css" . "text/css")
    ("pdf" . "application/pdf") ("csv" . "text/csv")
    ("js" . "text/javascript") ("json" . "application/json") ("map" . "application/json")
    ("ttf" . "application/font-ttf") ("woff" . "application/woff") ("woff2" . "application/woff")
    ("eot" . "application/vnd.ms-fontobject")
    ("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg") ("jfif" . "image/jpeg")
    ("tif" . "image/tiff") ("tiff" . "image/tiff") ("gif" . "image/gif") ("svg" . "image/svg+xml") ("ico" . "image/x-icon"))
  "alist mapping of common PATHNAME-TYPEs to mime")

(defun sniff-pathname-mime-type (pathname &optional (default "application/octet-stream"))
  "Determine the mime type of PATHNAME or DEFAULT otherwise"
  (or (cdr (assoc (pathname-type pathname) +extensions+ :test #'string-equal))
      (image-p pathname)
      default))

(defun sniff-extension-mime-type (type &optional (default "application/octet-stream"))
  (or (cdr (assoc type +extensions+ :test #'string-equal)) default))

(defun extension-by-mime-type (type)
  (or (car (assoc type +extensions+ :test #'string-equal)) "dat"))

(defconstant +text-types+ '("text/html" "text/css" "application/json" "text/plain")
  "mime types which should be treated as text")

(defparameter +image-headers+
  '((#(#xFF #xD8 #xFF #xE0 nil nil #x4A #x46 #x49 #x46) . ("image/jpeg" "JFIF"))
    (#(#xFF #xD8 #xFF #xE1 nil nil #x45 #x78 #x69 #x66) . ("image/jpeg" "Canon JFIF"))
    (#(#x49 #x49 #x2A) . ("image/tiff" "TIFF"))
    (#(#x4D #x4D #x2A) . ("image/tiff" "TIFF Mac"))
    (#(#x42 #x4D) . ("image/bmp" "BMP"))
    (#(#x47 #x49 #x46) . ("image/gif" "GIF"))
    (#(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A) . ("image/png" "PNG"))
    (#(#x42 #x50 #x47 #xFB) . ("application/octet-stream" "BPG"))
    (#(#x72 #x69 #x66 #x66) . ("image/webp" "WEBP")))
  "Precursor bytes for common image file formats")

(defparameter +max-image-header-bytes+ (reduce #'max +image-headers+ :initial-value 0 :key (lambda (c) (length (car c))))
  "Max number of bytes required to test +IMAGE-HEADERS+ fully")

(defun %test-image-header (bytes)
  (flet ((test-image-header-octetwise (one two)
           (when (>= (length one) (length two))
             (loop for a across one
                   for b across two
                   always (if (and a b) (eql a b) t)))))
    (values-list (cdr (assoc bytes +image-headers+ :test #'test-image-header-octetwise)))))

(defgeneric image-p (file)
  (:documentation "Returns (VALUES mime-type image-type-string) if PATHNAME points to an image file")

  (:method ((pathname pathname))
    (when (probe-file pathname)
      (with-open-file (image pathname :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (when image
          (let ((array (make-array (min +max-image-header-bytes+ (file-length image)) :element-type '(unsigned-byte))))
            (read-sequence array image)
            (%test-image-header array))))))

  (:method ((string string))
    (image-p (make-pathname string)))

  (:method ((vector array))
    (%test-image-header (subseq vector 0 (min +max-image-header-bytes+ (length vector))))))

(defun text-p (mime)
  "Should this mime type be treated as text?"
  (cond ((null mime) t)
        ((stringp mime) (find mime +text-types+ :test #'string-equal))
        (t t)))

(defun lwsp (c)
  (find c +lwsp+ :test #'char=))

(defun parse (source)
  (if (streamp source)
      (parse-stream source)
      (let* ((eoh (search #(13 10 13 10) source :test #'=))
             (headers (when eoh (parse-headers (ef:decode-external-string source :ascii :end eoh))))
             (body (if eoh (nsubseq source (+ eoh 4)) source)))
        (if headers
            (make-mime :headers headers :body (parse-body body headers))
            body))))

(defun parse-stream (stream)
  (labels ((read-until-crlf-crlf ()
             (loop with buffer = (make-array 512 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
                   for len from 0
                   for c = (read-byte stream nil nil)
                   do (cond ((null c) (return-from read-until-crlf-crlf buffer))
                            ((ends-with buffer #(13 10 13) :test #'=) (return-from read-until-crlf-crlf (nsubseq buffer 0 (- len 3))))
                            (t (vector-push-extend c buffer)))))

           (chunked-body-p (headers)
             (search "chunked" (header :transfer-encoding headers) :test #'char-equal))

           (skip-stream (num)
             (loop repeat num do (read-char stream nil nil)))

           (read-chunked-body ()
             (loop with vector = (make-array 512 :adjustable t :element-type '(unsigned-byte 8))
                   with start = 0
                   for len = (parse-integer (or (read-line stream nil nil) "0") :junk-allowed t :radix 16)
                   while (plusp len)
                   do (setf vector (adjust-array vector (+ (length vector) len)))
                      (read-sequence vector stream :start start :end (+ start len))
                      (incf start len)
                      (skip-stream 2)
                   finally (skip-stream 2) (return (nsubseq vector 0 start))))

           (read-content-length-body (len)
             (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
               (read-sequence buffer stream)
               buffer))

           (read-body-until-eof ()
             (loop with vector = (make-array 2048 :adjustable t :element-type '(unsigned-byte 8))
                   with start = 0
                   for end = (read-sequence vector stream :start start)
                   if (< end (length vector))
                     return (nsubseq vector 0 end)
                   else
                     do (setf vector (adjust-array vector (+ (length vector) 2048)) start end)))

           (read-body (headers)
             (let* ((lenstr (header :content-length headers)) (len (when lenstr (parse-integer lenstr :junk-allowed t))))
               (cond ((chunked-body-p headers) (read-chunked-body))
                     (len (read-content-length-body len))
                     (t (read-body-until-eof))))))

    (let ((headers (parse-headers (ef:decode-external-string (read-until-crlf-crlf) :ascii))))
      (make-mime :headers headers :body (parse-body (read-body headers) headers)))))

(defun header (key mime)
  (cdr (assoc key (if (mime-p mime) (mime-headers mime) mime) :test #'string-equal)))

(defun get-subheader-value (key string)
  (lw:when-let (pos (search key string :test #'char-equal))
    (let ((sow (+ pos (length key) 1)))
      (if (char= (char string sow) #\")
          (nsubseq string (1+ sow) (position #\" string :start (1+ sow)))
          (nsubseq string sow (position #\; string :start sow))))))

(defun subheader (key field mime)
  (lw:when-let (header (header key mime))
    (get-subheader-value (symbol-name field) header)))

(defun unfold-lines (lines)
  (let ((line (car lines)) (next (cadr lines)) (rest (cddr lines)))
    (cond ((null next) (cons line nil))
          ((lwsp (char next 0)) (unfold-lines (cons (concatenate 'string line " " (nsubseq next (position-if-not #'lwsp next))) rest)))
          (t (cons line (unfold-lines (cdr lines)))))))

(defun parse-headers (headers)
  (merge-headers (sort (mapcan #'make-header (unfold-lines (lw:split-sequence +crlf+ headers :test #'char= :coalesce-separators t))) #'string< :key #'car)))

(defconstant +headers+
  '(("accept" . :ACCEPT)
    ("accept-encoding" . :ACCEPT-ENCODING)
    ("accept-ranges" . :ACCEPT-RANGES)
    ("authorization" . :AUTHORIZATION)
    ("bcc" . :BCC)
    ("cache-cotrol" . :CACHE-CONTROL)
    ("content-disposition" . :CONTENT-DISPOSITION)
    ("content-encoding" . :CONTENT-ENCODING)
    ("content-length" . :CONTENT-LENGTH)
    ("content-transfer-encoding" . :CONTENT-TRANSFER-ENCODING)
    ("content-type" . :CONTENT-TYPE)
    ("cookie" . :COOKIE)
    ("date" . :DATE)
    ("expect" . :EXPECT)
    ("from" . :FROM)
    ("host" . :HOST)
    ("hx-request" . :HX-REQUEST)
    ("in-reply-to" . :IN-REPLY-TO)
    ("location" . :LOCATION)
    ("max-forwards" . :MAX-FORWARDS)
    ("message-id" . :MESSAGE-ID)
    ("pragma" . :PRAGMA)
    ("range" . :RANGE)
    ("received" . :RECEIVED)
    ("references" . :REFERENCES)
    ("referer" . :REFERER)
    ("request-method" . :REQUEST-METHOD)
    ("request-uri" . :REQUEST-URI)
    ("return-path" . :RETURN-PATH)
    ("server" . :SERVER)
    ("subject" . :SUBJECT)
    ("te" . :TE)
    ("to" . :TO)
    ("transfer-encoding" . :TRANSFER-ENCODING)
    ("user-agent" . :USER-AGENT)
    ("vary" . :VARY)))

(defun make-header (line)
  (let ((colon (position #\: line :test #'char=)))
    (lw:when-let (name (cdr (assoc (nsubseq line 0 colon) +headers+ :test #'string-equal)))
      (list (cons name (string-trim +lwsp+ (nsubseq line (1+ colon))))))))

(defun make-header* (name value)
  (lw:when-let (name (cdr (assoc name +headers+ :test #'string-equal)))
    (list (cons name value))))

(defun merge-headers (list)
  (cond ((null (cdr list)) list)
        ((string-equal (caar list) (caadr list)) (merge-headers (cons (cons (caar list) (append (mklist (cdar list)) (mklist (cdadr list)))) (cddr list))))
        (t (cons (car list) (merge-headers (cdr list))))))

(defun parse-body (body headers)
  (if (starts-with (header :content-type headers) "multipart")
      (mapcar (partial #'maybe-parse-part headers) (split-body-into-parts body (subheader :content-type :boundary headers)))
      (decode-body body headers)))

(defun maybe-parse-part (body headers)
  (if (mismatch #(13 10) body :end2 (min (length body) 2))
      (parse body)
      (decode-body body headers)))

(defun split-body-into-parts (array boundary)
  (if boundary
      (loop with split = (list* 13 10 45 45 (map 'list #'char-code boundary))
            with split-len = (+ 4 (length boundary))
            with start = 0
            for pos = (search split array :start2 start)
            nconc (trim-body-part array start pos)
            if pos
              do (setf start (+ pos split-len))
            else
              do (loop-finish))

      (list array)))

(defun trim-body-part (array start end)
  (unless (or (eql start end) (not (mismatch #(45 45 13 10) array :start2 start :end2 end)))
    ;; Remove the 13 10 from the start
    (list (nsubseq array (+ start 2) end))))

;;; ============================================================================
;;; DECODING BODY
;;; ============================================================================
(defconstant +encodings+
  '(("iso-8859-1" . :latin-1)
    ("ascii" . :ascii)
    ("us-ascii" . :ascii)
    ("utf8" . :utf-8)
    ("utf-8" . :utf-8)
    ("utf16" . :utf-16)
    ("utf-16" . :utf-16)
    ("utf32" . :utf-32)
    ("utf-32" . :utf-32)
    ("euc-jp" . :euc-jp)
    ("sjis" . :sjis)
    ("gbk" . :gbk)
    ("gb2312" . :gbk)
    ("gb18030" . :gbk)
    ("koi8-r" . :koi8-r))
  "Mapping of strings to supported encodings")

(defun decode-body (body headers)
  (let ((transfer-encoding (or (header :content-transfer-encoding headers) (header :transfer-encoding headers)))
        (content-type (header :content-type headers))
        (charset (subheader :content-type :charset headers)))
    (decode-content-type (decode-charset (decode-transfer body transfer-encoding) charset content-type) content-type)))

(defun decode-content-type (body content-type)
  (cond ((null content-type) body)
        ((starts-with content-type "application/json") (json:decode body))
        ((starts-with content-type "application/x-www-form-urlencoded") (decode-uri-query body))
        ((starts-with content-type "multipart/form-data") (decode-form-data body content-type))
        (t body)))

(defun decode-charset (body charset content-type)
  (if (or charset (some (partial* #'starts-with content-type) '("text/plain" "text/html" "application/csv" "application/json" "application/x-www-form-urlencoded")))
      (ef:decode-external-string body (or (cdr (assoc charset +encodings+ :test #'string-equal)) :latin-1-safe))
      body))

(defun decode-transfer (body transfer-encoding)
  (cond ((null transfer-encoding) body)
        ((starts-with transfer-encoding "base64") (crypto:decode-base-64 (ef:decode-external-string body :ascii)))
        ((starts-with transfer-encoding "quoted-printable") (decode-quoted-printable body))
        (t body)))

;;; ============================================================================
;;; QUOTED PRINTABLE
;;; ============================================================================

(defun decode-quoted-printable (array)
  "Convert a quoted-printable array"
  (flet ((hex (a b)
           (+ (* 16 (cond ((<= 48 a 57) (- a 48)) ((<= 65 a 70) (- a 55)) ((<= 97 a 102) (- a 87)) (t 0)))
              (cond ((<= 48 b 57) (- b 48)) ((<= 65 b 70) (- b 55)) ((<= 97 b 102) (- b 87)) (t 0)))))
    (loop with len = (length array)
          with out = 0
          for in below (length array)
          for val = (aref array in)
          do (cond ((and (/= val 61) (= in out)) (incf out))
                   ((/= val 61) (setf (aref array out) (aref array in) out (1+ out)))
                   ((and (< in (- len 2)) (= (aref array (1+ in)) 13) (= (aref array (+ 2 in)) 10)) (incf in 2))
                   ((and (< in (1- len)) (find (aref array (1+ in)) #(13 10))) (incf in 1))
                   (t (setf (aref array out) (hex (aref array (1+ in)) (aref array (+ in 2))) out (1+ out) in (+ 2 in))))
          finally (return (adjust-array array out)))))

(defun encode-quoted-printable (obj &rest additional-chars-to-encode)
  "Convert array/string OBJ into a string following quoted-printable encoding.
ADDITIONAL-CHARS-TO-ENCODE represents other chars which should be encoded (e.g. #\? in encoded-word headers)"
  ;; #\Tab 9 and #\Space 32 are themselves unless they're at the end of a line
  ;; #\= 61 must always be =3D
  ;; #\Return and #\Linefeed should always be represented by themselves for anything other than media content-types
  ;; Lines must be at most 76 characters long, a final #\= is a soft line break
  ;; ADDITIONAL-CHARS-TO-ENCODE represents other chars which should be encoded (e.g. #\? in encoded-word format)
  (flet ((soft-line-break (stream)
           (princ #\= stream) (princ +crlf+ stream)
           (file-position stream))

         (encode (code column)
           (if (or (find (code-char code) additional-chars-to-encode :test #'char=)
                   (and (or (= code 9) (= code 32)) (> column 74))
                   (= code 61) (< code 9) (= code 11) (= code 12) (< 14 code 32) (> code 126))
               (format nil "=~2,'0X" code)
               (code-char code))))

    (unless (stringp obj)
      (setf additional-chars-to-encode (append '(#\Return #\Linefeed) additional-chars-to-encode)))

    (with-output-to-string (stream)
      (loop with line-break = 0
            for char across obj
            for code = (if (numberp char) char (char-code char))
            for column = (- (file-position stream) line-break)
            for encoding = (encode code column)
            if (> (+ (if (characterp encoding) 1 3) column) 75)
              do (setf line-break (soft-line-break stream) encoding (encode code 0)) (princ encoding stream)
            else if (or (char= char #\Return) (char= char #\Linefeed)) do (princ encoding stream) (setf line-break (file-position stream))
                   else do (princ encoding stream)))))

;;; ============================================================================
;;; ENCODED WORD
;;; ============================================================================

(defun encode-encoded-word (string &key (encoding :quoted-printable) (charset :us-ascii))
  "Convert STRING to the 'encoded-word' format using ENCODING :quoted-printable or :base-64 and CHARSET"
  (format nil "=?~(~A~)?~C?~A?="
          charset
          (if (eq encoding :quoted-printable) #\Q #\B)
          (if (eq encoding :quoted-printable)
              (nsubstitute #\_ #\Space (encode-quoted-printable string #\?))
              (crypto:encode-base-64 string))))

(defun decode-encoded-word (string)
  "Convert STRING from 'encoded-word' format into a UTF-8 string"
  ;; =?charset?[bq]?text-to-decode?=[rest of string]
  (destructuring-bind (&optional prefix charset decoder text rest) (#~m/^([^=]*)=\?([^?]+)\?([bBqQ])\?([^?]*)\?=(.*)$/ string)
    (lw:if-let (encoding (and charset decoder text (gethash charset +encodings+)))
               (strcat (or prefix "")
                       (ef:decode-external-string (if (char-equal (char decoder 0) #\q)
                                                      (decode-quoted-printable text)
                                                      (crypto:decode-base-64 text))
                                                  encoding)
                       (or (and rest (maybe-decode-encoded-word (delete-if #'lwsp rest))) ""))
               string)))

(defun maybe-decode-encoded-word (string)
  "Decode if our string is in 'encoded-word' format"
  (if (search "=?" string :test #'char=)
      (decode-encoded-word string)
      string))

;;; ============================================================================
;;; PERCENTAGE HEX
;;; ============================================================================

(defun encode-percentage-hex (string)
  "Encode STRING '/Apple Bob' to '/Apple%20Bob'"
  (flet ((translate (char)
           (if (or (alphanumericp char) (not (find char " :/?#[]@!$&'/()*+,;=%" :test #'char=)))
               char
               (format nil "%~X" (char-code char)))))
    (format nil "~{~A~}" (map 'list #'translate string))))

(defun decode-percentage-hex (string &optional (start 0) (end (length string)))
  "Decode STRING '/Apple%20Bob' to '/Apple Bob'"
  (flet ((decode (pos)
           (if (< pos (- end 2))
               (code-char (+ (* 16 (digit-char-p (char string (1+ pos)) 16)) (digit-char-p (char string (+ 2 pos)) 16)))
               "")))
    (reduce #'strcat
            (loop for pct = (position #\% string :test #'char= :start start :end end)
                  collect (subseq string start pct)
                  if (and pct (< pct end)) collect (string (decode pct)) and do (setf start (min end (+ pct 3)))
                    else do (loop-finish))
            :initial-value "")))

;;; ============================================================================
;;; URLENCODED
;;; ============================================================================

(defun decode-urlencoded (string &optional (start 0) (end (length string)))
  "Decode URI string using percentage-hex and substituting #\Space for #\+"
  (decode-percentage-hex (substitute #\Space #\+ string :test #'char=) start end))

(defun decode-uri-query (string)
  "Convert the URI query string into an alist"
  (flet ((make-params (part)
           (let ((parts (lw:split-sequence '(#\=) part :test #'char=)))
             (cons (decode-urlencoded (car parts)) (decode-urlencoded (cadr parts))))))
    (mapcar #'make-params (lw:split-sequence '(#\&) string :test #'char=))))

(defun encode-uri-query (alist)
  (format nil "~{~{~A=~A~}~^&~}" (mapcar (lambda (cons) (list (encode-percentage-hex (car cons)) (encode-percentage-hex (cdr cons)))) alist)))

;;; ============================================================================
;;; FORM DATA
;;; ============================================================================

(defun decode-form-data (body content-type)
  (lw:if-let (boundary (get-subheader-value "boundary" content-type))
             (mapcar #'make-form-data-kv (mapcar #'parse (split-body-into-parts body boundary)))
             (list (cons "value" body))))

(defun make-form-data-kv (mime)
  (let ((filename (subheader :content-disposition :filename mime)))
    (cons (subheader :content-disposition :name mime)
          (if filename
              (list (cons "filename" filename) (cons "value" (mime-body mime)))
              (ef:decode-external-string (mime-body mime) :utf-8)))))
