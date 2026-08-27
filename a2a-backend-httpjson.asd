(defsystem "a2a-backend-httpjson"
  :version "0.2.0"
  :description "HTTP+JSON REST binding for a2a-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("a2a-protocol" "http-protocol" "http-server-protocol"
               "sse-protocol" "babel")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "a2a-backend-httpjson/tests"))))

(defsystem "a2a-backend-httpjson/tests"
  :depends-on ("a2a-backend-httpjson" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
