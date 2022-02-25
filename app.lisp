(push #p"/app/" asdf:*central-registry*)

(ql:quickload :demo-server)

(hunchentoot:define-easy-handler (greeting :uri "/yo") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (print "greeting")
  "hello World!!!")
