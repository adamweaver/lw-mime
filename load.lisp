(define-lw-system lw-mime ()
  (:system "lw-string")
  (:system "lw-json")
  (:system "lw-crypto")
  (:file "body" :depends-on "mime")
  (:file "mime" :depends-on "package")
  (:file "package"))

