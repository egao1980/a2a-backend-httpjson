(in-package #:a2a-backend-httpjson/tests)

(deftest backend-class
  (ok (typep (a2a-backend-httpjson:make-httpjson-a2a-backend) 'a2a-backend-httpjson:httpjson-a2a-backend)))
