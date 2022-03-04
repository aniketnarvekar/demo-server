;;;; package.lisp

(defpackage #:demo-server
  (:use #:cl #:hunchentoot)
  (:export #:server-start
	   #:server-stop))
