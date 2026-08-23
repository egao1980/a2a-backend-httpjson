(in-package #:a2a-backend-httpjson)

(defclass httpjson-a2a-backend (a2a-protocol:a2a-backend) ())

(defun make-httpjson-a2a-backend ()
  (make-instance 'httpjson-a2a-backend))

(defun use-httpjson-a2a-backend ()
  (setf a2a-protocol:*a2a-backend* (make-httpjson-a2a-backend)))
