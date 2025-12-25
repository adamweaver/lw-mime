(define-lw-system lw-mime ()
  (:system "lw-string")
  (:system "lw-json")
  (:file "body" :depends-on "mime")
  (:file "mime" :depends-on "package")
  (:file "package"))

