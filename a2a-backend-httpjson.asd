(defsystem "a2a-backend-httpjson"
  :version "0.2.1"
  :description "HTTP+JSON REST binding for a2a-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("a2a-protocol" "rpc-protocol" "rpc-backend-http"
               "http-protocol" "http-server-protocol"
               "sse-protocol" "babel")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "a2a-backend-httpjson/tests"))))

(defsystem "a2a-backend-httpjson/tests"
  :depends-on ("a2a-backend-httpjson"
               "http-server-backend-hunchentoot"
               "http-backend-async"
               "http-backend-dexador"
               "event-protocol"
               "usocket"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
