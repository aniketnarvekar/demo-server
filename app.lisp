(push #p"/app/" asdf:*central-registry*)

(ql:quickload :demo-server)

(hunchentoot:define-easy-handler (greeting :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  "hello World!!!")
