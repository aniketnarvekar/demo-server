;;;; demo-server.lisp

(in-package #:demo-server)

;;; Subclass ACCEPTOR
(defclass vhost (acceptor)
  ;; slots
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))) ; because ACCEPTOR uses it

;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
(defmethod acceptor-dispatch-request ((vhost vhost) request)
  ;; try REQUEST on each dispatcher in turn
  (mapc (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler               ; Handler found. FUNCALL it and return result
	      (return-from acceptor-dispatch-request (funcall handler)))))
	(dispatch-table vhost))
  (call-next-method))

;;; ======================================================================
;;; Now all we need to do is test it

;;; Define handlers
(defun foo1 () "Hello")


(defun create-server (&key port)
  (let ((s (make-instance 'vhost :port port)))
    (send-prefix-dispatcher s)
    s))

(let ((servers nil))
  (defun server-start (&key port)
    (let ((s (create-server :port port)))
      (push (start s) servers)
      (format t "Server started with address ~S and port ~D"
	      (acceptor-address s)
	      (acceptor-port s))
      (values)))
  (defun server-stop (&key port)
    (flet ((filter (s)
	     (= (acceptor-port s) port))
	   (vhost-stop (s)
	     (unless s
	       (return-from server-stop (values)))
	     (stop s)
	     (format t "Server stoped for address ~S and port ~D"
		     (acceptor-address s)
		     (acceptor-port s))
	     (values)))
      (vhost-stop
       (car (remove-if-not #'filter servers)))
      (setq vhost1
	    (remove-if #'filter servers))
      (values))))

(let ((funs nil))
  (defun push-prefix-dispatcher (prefix func)
    (push (create-prefix-dispatcher prefix func) funs))
  (defmethod send-prefix-dispatcher ((acceptor vhost))
    (map (values) (lambda (func)
		    (push func (dispatch-table acceptor)))
	 funs)))

(push-prefix-dispatcher "/foo" 'foo1)
