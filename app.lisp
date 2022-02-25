(push #p"/app/" asdf:*central-registry*)

(ql:quickload :demo-server)

(demo-server::clackup
 (lambda (env)
   (declare (ignore env))
   '(200 (:content-type "text/plain") ("hello, Clack!"))))
