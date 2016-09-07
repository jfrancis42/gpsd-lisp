;;;; gpsd.asd

(asdf:defsystem #:gpsd
  :description "A Common Lisp client for reading GPSD data."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:spot
               #:cl-json
	       #:usocket
               #:aviation-formulary
	       #:local-time)
  :serial t
  :components ((:file "package")
               (:file "gpsd")))

