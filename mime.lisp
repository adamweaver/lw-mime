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
    (get-subheader-value (if (symbolp field) (symbol-name field) field) header)))

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

