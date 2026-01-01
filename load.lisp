(define-lw-system lw-mime ()
  (:system "lw-crypto")
  (:system "lw-json")
  (:system "lw-utils")
  (:file "body" :depends-on "mime")
  (:file "mime" :depends-on "package")
  (:file "package"))

