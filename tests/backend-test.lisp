(in-package #:a2a-backend-httpjson/tests)

(defun %agent ()
  (a2a-protocol:make-a2a-agent :name "echo"))

(defun %send-body (&optional (text "hi"))
  (a2a-protocol:encode-json
   (a2a-protocol:json-object
    "message" (a2a-protocol:encode-message
               (a2a-protocol:make-a2a-message :text text)))))

(defun %headers ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "a2a-version" h) "1.0")
    h))

(deftest backend-class
  (ok (typep (a2a-backend-httpjson:make-httpjson-a2a-backend)
             'a2a-backend-httpjson:httpjson-a2a-backend)))

(deftest well-known-card
  (let* ((app (a2a-backend-httpjson:make-a2a-app (%agent)))
         (res (funcall app (list :request-method :get
                                 :path-info "/.well-known/agent-card.json"))))
    (ok (eql 200 (first res)))
    (let ((card (a2a-protocol:decode-json (first (third res)))))
      (ok (equal "echo" (gethash "name" card)))
      (ok (null (gethash "protocolVersion" card)))
      (ok (plusp (length (gethash "supportedInterfaces" card)))))))

(deftest rest-send-and-get
  (let* ((agent (%agent))
         (app (a2a-backend-httpjson:make-a2a-app agent))
         (sent (funcall app (list :request-method :post
                                  :path-info "/message:send"
                                  :raw-body (%send-body "pong")
                                  :headers (%headers)))))
    (ok (eql 200 (first sent)))
    (let* ((body (a2a-protocol:decode-json (first (third sent))))
           (task (a2a-protocol:decode-send-result body)))
      (ok (eq :completed (a2a-protocol:a2a-task-state task)))
      (let* ((id (a2a-protocol:a2a-task-id task))
             (got (funcall app (list :request-method :get
                                     :path-info (format nil "/tasks/~a" id)
                                     :headers (%headers))))
             (got-task (a2a-protocol:decode-task
                        (a2a-protocol:decode-json (first (third got))))))
        (ok (eql 200 (first got)))
        (ok (equal id (a2a-protocol:a2a-task-id got-task)))))))

(deftest rest-list-and-cancel
  (let* ((agent (%agent))
         (app (a2a-backend-httpjson:make-a2a-app agent)))
    (funcall app (list :request-method :post
                       :path-info "/message:send"
                       :raw-body (%send-body "a")
                       :headers (%headers)))
    (let* ((listed (funcall app (list :request-method :get
                                      :path-info "/tasks"
                                      :query-string "pageSize=10"
                                      :headers (%headers))))
           (body (a2a-protocol:decode-json (first (third listed)))))
      (ok (eql 200 (first listed)))
      (ok (eql 1 (gethash "totalSize" body)))
      (ok (equal "" (gethash "nextPageToken" body))))))

(deftest rest-stream
  (let* ((app (a2a-backend-httpjson:make-a2a-app (%agent)))
         (res (funcall app (list :request-method :post
                                 :path-info "/message:stream"
                                 :raw-body (%send-body "stream")
                                 :headers (%headers)))))
    (ok (eql 200 (first res)))
    (ok (search "text/event-stream" (getf (second res) :content-type)))
    (let ((events (with-input-from-string (s (first (third res)))
                    (sse-protocol:collect-sse-events s))))
      (ok (= 3 (length events))))))

(deftest rest-unknown-task-404
  (let* ((app (a2a-backend-httpjson:make-a2a-app (%agent)))
         (res (funcall app (list :request-method :get
                                 :path-info "/tasks/missing"
                                 :headers (%headers))))
         (body (a2a-protocol:decode-json (first (third res)))))
    (ok (eql 404 (first res)))
    (ok (eql a2a-protocol:+a2a-error-task-not-found+
             (gethash "code" (gethash "error" body))))))

(deftest missing-http-backend-signals
  (let ((http-protocol:*http-backend* nil))
    (ok (signals (a2a-protocol:send-message
                  (a2a-backend-httpjson:make-httpjson-a2a-backend
                   :url "http://127.0.0.1:9/")
                  (a2a-protocol:make-a2a-message :text "x"))
                 'a2a-protocol:a2a-error))))

(deftest resubscribe-is-unsupported
  (ok (signals (a2a-protocol:resubscribe-task
                (a2a-backend-httpjson:make-httpjson-a2a-backend
                 :url "http://127.0.0.1:9/")
                "t1")
               'a2a-protocol:a2a-unsupported)))
