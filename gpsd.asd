;;;; gpsd.asd

(asdf:defsystem #:gpsd
  :description "Describe gpsd-lisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:spot
               #:aviation-formulary)
  :serial t
  :components ((:file "package")
               (:file "gpsd")))

