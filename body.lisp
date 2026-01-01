(in-package :mime)

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
        ((starts-with transfer-encoding "base64") (base64:decode-base-64 (ef:decode-external-string body :ascii)))
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
              (base64:encode-base-64 string))))

(defun decode-encoded-word (string)
  "Convert STRING from 'encoded-word' format into a UTF-8 string"
  ;; =?charset?[bq]?text-to-decode?=[rest of string]
  (destructuring-bind (&optional prefix charset decoder text rest) (#~m/^([^=]*)=\?([^?]+)\?([bBqQ])\?([^?]*)\?=(.*)$/ string)
    (lw:if-let (encoding (and charset decoder text (gethash charset +encodings+)))
               (strcat (or prefix "")
                       (ef:decode-external-string (if (char-equal (char decoder 0) #\q)
                                                      (decode-quoted-printable text)
                                                      (base64:decode-base-64 text))
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
