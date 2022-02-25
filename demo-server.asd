;;;; demo-server.asd

(asdf:defsystem #:demo-server
  :description "Describe demo-server here"
  :author "Aniket Narvekar"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:alexandria #:s-sql #:postmodern #:cl-ppcre)
  :components ((:file "package")
               (:file "demo-server")))
