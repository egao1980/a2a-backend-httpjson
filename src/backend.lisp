(in-package #:a2a-backend-httpjson)

(defclass httpjson-a2a-backend (a2a-protocol:a2a-backend)
  ((url :initarg :url :accessor backend-url :initform nil)
   (card :initarg :card :accessor backend-card :initform nil)
   (protocol-version :initarg :protocol-version
                     :accessor backend-protocol-version
                     :initform a2a-protocol:+a2a-protocol-version+)))

(defun make-httpjson-a2a-backend
    (&key url card (protocol-version a2a-protocol:+a2a-protocol-version+))
  (make-instance 'httpjson-a2a-backend
                 :url url :card card :protocol-version protocol-version))

(defun use-httpjson-a2a-backend (&rest args &key &allow-other-keys)
  (setf a2a-protocol:*a2a-backend* (apply #'make-httpjson-a2a-backend args)))

(defun well-known-card-path-p (path)
  (member path '("/.well-known/agent-card.json" "/.well-known/agent.json")
          :test #'string=))

(defun %header (env name)
  (let ((headers (getf env :headers)))
    (cond
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash (string-downcase name) headers)))
      ((listp headers)
       (cdr (assoc name headers :test #'string-equal)))
      (t nil))))

(defun %octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun %slurp-stream (stream)
  (if (and (open-stream-p stream)
           (ignore-errors
             (let ((et (stream-element-type stream)))
               (and et (subtypep et 'character)))))
      (with-output-to-string (out)
        (loop for c = (read-char stream nil :eof)
              until (eq c :eof)
              do (write-char c out)))
      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
        (loop for b = (read-byte stream nil :eof)
              until (eq b :eof)
              do (vector-push-extend b bytes))
        (%octets-to-string bytes))))

(defun slurp-env-body (env)
  (let ((raw (getf env :raw-body)))
    (cond
      ((null raw) "")
      ((stringp raw) raw)
      ((and (vectorp raw) (not (stringp raw)))
       (%octets-to-string raw))
      ((streamp raw) (%slurp-stream raw))
      (t (princ-to-string raw)))))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (%octets-to-string b))
      (t ""))))

(defun %split (string delimiter)
  (let ((out nil)
        (start 0)
        (len (length string)))
    (loop for i from 0 below len
          when (char= (char string i) delimiter)
            do (push (subseq string start i) out)
               (setf start (1+ i)))
    (push (subseq string start) out)
    (nreverse out)))

(defun %query (env)
  (let ((qs (or (getf env :query-string) ""))
        (h (make-hash-table :test 'equal)))
    (unless (zerop (length qs))
      (dolist (pair (%split qs #\&))
        (let* ((eq (position #\= pair))
               (k (if eq (subseq pair 0 eq) pair))
               (v (if eq (subseq pair (1+ eq)) "")))
          (setf (gethash k h) v))))
    h))

(defun %q (query key &optional default)
  (or (gethash key query) default))

(defun %json (status obj)
  (list status
        '(:content-type "application/json; charset=utf-8")
        (list (a2a-protocol:encode-json obj))))

(defun %error-status (code)
  (cond
    ((eql code a2a-protocol:+a2a-error-task-not-found+) 404)
    ((eql code rpc-protocol:+invalid-params+) 400)
    ((or (eql code a2a-protocol:+a2a-error-unsupported-operation+)
         (eql code a2a-protocol:+a2a-error-push-not-supported+))
     400)
    (t 400)))

(defun %error-json (c)
  (%json (%error-status (or (a2a-protocol:a2a-error-code c) 400))
         (a2a-protocol:json-object
          "error" (a2a-protocol:json-object
                   "code" (a2a-protocol:a2a-error-code c)
                   "message" (a2a-protocol:a2a-error-message c)
                   "data" (or (a2a-protocol:a2a-error-data c) :omit)))))

(defun %card-response (card)
  (%json 200 (a2a-protocol:encode-agent-card card)))

(defun %decode-body (env)
  (let ((raw (slurp-env-body env)))
    (if (plusp (length raw))
        (a2a-protocol:decode-json raw)
        (a2a-protocol:json-object))))

(defun %sse (events)
  (list 200
        '(:content-type "text/event-stream; charset=utf-8"
          :cache-control "no-cache")
        (list (apply #'concatenate 'string
                     (mapcar (lambda (ev)
                               (sse-protocol:encode-sse-event
                                (sse-protocol:make-sse-event
                                 :data (a2a-protocol:encode-json ev))))
                             events)))))

(defun %run (agent method params env)
  (let ((ver (or (%header env "a2a-version") (%header env "A2A-Version"))))
    (handler-case
        (let ((result (a2a-protocol:dispatch-a2a-method
                       agent method params :protocol-version ver)))
          (if (typep result 'a2a-protocol:a2a-stream-result)
              (%sse (a2a-protocol:a2a-stream-events result))
              (%json 200 result)))
      (a2a-protocol:a2a-error (c)
        (%error-json c))
      (rpc-protocol:rpc-error (c)
        (%error-json
         (make-condition 'a2a-protocol:a2a-error
                         :message (rpc-protocol:rpc-error-message c)
                         :code (rpc-protocol:rpc-error-code c)
                         :data (rpc-protocol:rpc-error-data c)))))))

(defun %ends-with (s suffix)
  (and (stringp s) (stringp suffix)
       (>= (length s) (length suffix))
       (string= s suffix :start1 (- (length s) (length suffix)))))

(defun %task-id-from-path (path)
  (cond
    ((and (string= path "/tasks/" :end1 (min (length path) 7))
          (%ends-with path ":cancel"))
     (subseq path 7 (- (length path) (length ":cancel"))))
    ((and (> (length path) 7) (string= path "/tasks/" :end1 7)
          (not (find #\/ path :start 7)))
     (subseq path 7))
    (t nil)))

(defun make-a2a-app (agent &key card)
  "Clack HTTP+JSON REST app (A2A 1.0).
   POST /message:send, POST /message:stream, GET /tasks, GET /tasks/{id},
   POST /tasks/{id}:cancel, GET /extendedAgentCard, GET well-known card."
  (lambda (env)
    (block app
      (let ((path (or (getf env :path-info) "/"))
            (method (getf env :request-method))
            (query (%query env)))
        (when (and (eq method :get) (well-known-card-path-p path))
          (return-from app
            (%card-response (or card (a2a-protocol:a2a-agent-card agent)))))
        (cond
          ((and (eq method :post) (string= path "/message:send"))
           (%run agent "SendMessage" (%decode-body env) env))
          ((and (eq method :post) (string= path "/message:stream"))
           (%run agent "SendStreamingMessage" (%decode-body env) env))
          ((and (eq method :get) (string= path "/tasks"))
           (%run agent "ListTasks"
                 (a2a-protocol:json-object
                  "contextId" (or (%q query "contextId") :omit)
                  "status" (or (%q query "status") :omit)
                  "pageSize" (let ((n (%q query "pageSize")))
                               (if n (parse-integer n :junk-allowed t) :omit))
                  "pageToken" (or (%q query "pageToken") :omit)
                  "historyLength" (let ((n (%q query "historyLength")))
                                    (if n (parse-integer n :junk-allowed t) :omit))
                  "includeArtifacts" (if (equal (%q query "includeArtifacts") "true")
                                         t :omit)
                  "statusTimestampAfter" (or (%q query "statusTimestampAfter") :omit))
                 env))
          ((and (eq method :get) (string= path "/extendedAgentCard"))
           (%run agent "GetExtendedAgentCard" (a2a-protocol:json-object) env))
          ((and (eq method :post) (%ends-with path ":cancel")
                (let ((id (%task-id-from-path path)))
                  (when id
                    (return-from app
                      (%run agent "CancelTask"
                            (a2a-protocol:json-object "id" id) env)))))
           '(404 (:content-type "text/plain") ("not found")))
          ((and (eq method :get)
                (let ((id (%task-id-from-path path)))
                  (when (and id (not (%ends-with path ":cancel")))
                    (return-from app
                      (%run agent "GetTask"
                            (a2a-protocol:json-object
                             "id" id
                             "historyLength"
                             (let ((n (%q query "historyLength")))
                               (if n (parse-integer n :junk-allowed t) :omit)))
                            env)))))
           '(404 (:content-type "text/plain") ("not found")))
          (t
           '(404 (:content-type "text/plain") ("not found"))))))))

(defun %ensure-http ()
  (unless http-protocol:*http-backend*
    (error 'a2a-protocol:a2a-error
           :message "*http-backend* is nil — bind an http-protocol backend")))

(defun %ensure-url (backend)
  (or (backend-url backend)
      (error 'a2a-protocol:a2a-error
             :message "httpjson backend has no :url")))

(defun %join (base path &optional query)
  (let ((url (format nil "~a~a" (string-right-trim "/" base) path)))
    (if (and query (plusp (length query)))
        (format nil "~a?~a" url query)
        url)))

(defun %headers (backend)
  `(("content-type" . "application/json")
    ("accept" . "application/json, text/event-stream")
    ("A2A-Version" . ,(backend-protocol-version backend))))

(defun %raise-http (res)
  (let* ((status (http-protocol:response-status res))
         (text (%body-string res))
         (obj (ignore-errors (a2a-protocol:decode-json text))))
    (cond
      ((and obj (hash-table-p obj) (gethash "error" obj))
       (let ((err (gethash "error" obj)))
         (error 'a2a-protocol:a2a-error
                :code (gethash "code" err)
                :message (gethash "message" err)
                :data (gethash "data" err))))
      ((<= 200 status 299) obj)
      (t
       (error 'a2a-protocol:a2a-error
              :message (format nil "HTTP ~a~@[ ~a~]" status
                               (and (plusp (length text)) text)))))))

(defun %card-url (url)
  (if (search "/.well-known/" url)
      url
      (format nil "~a/.well-known/agent-card.json" (string-right-trim "/" url))))

(defmethod a2a-protocol:fetch-agent-card ((backend httpjson-a2a-backend) url &key)
  (%ensure-http)
  (let* ((res (http:get (%card-url url)
                        :headers `(("accept" . "application/json")
                                   ("A2A-Version" . ,(backend-protocol-version backend)))))
         (card (a2a-protocol:decode-agent-card (%raise-http res))))
    (setf (backend-card backend) card)
    card))

(defmethod a2a-protocol:serve-agent-card ((backend httpjson-a2a-backend) card &key)
  (setf (backend-card backend) card))

(defmethod a2a-protocol:send-message ((backend httpjson-a2a-backend) message
                                      &key task-id (blocking t))
  (%ensure-http)
  (when task-id
    (setf (a2a-protocol:a2a-message-task-id message) task-id))
  (let ((res (http:post (%join (%ensure-url backend) "/message:send")
                        :content (a2a-protocol:encode-json
                                  (a2a-protocol:json-object
                                   "message" (a2a-protocol:encode-message message)
                                   "configuration"
                                   (if blocking
                                       :omit
                                       (a2a-protocol:json-object
                                        "returnImmediately" t))))
                        :headers (%headers backend))))
    (a2a-protocol:decode-send-result (%raise-http res))))

(defmethod a2a-protocol:stream-message ((backend httpjson-a2a-backend) message
                                        &key on-event)
  (%ensure-http)
  (let* ((res (http:post (%join (%ensure-url backend) "/message:stream")
                         :content (a2a-protocol:encode-json
                                   (a2a-protocol:json-object
                                    "message" (a2a-protocol:encode-message message)))
                         :headers (%headers backend)))
         (text (%body-string res))
         (events (mapcar (lambda (ev)
                           (a2a-protocol:decode-json (sse-protocol:sse-event-data ev)))
                         (with-input-from-string (s text)
                           (sse-protocol:collect-sse-events s)))))
    (when on-event
      (mapc on-event events))
    (a2a-protocol:make-a2a-stream-result events)))

(defmethod a2a-protocol:get-task ((backend httpjson-a2a-backend) task-id
                                  &key history-length)
  (%ensure-http)
  (let ((qs (when history-length
              (format nil "historyLength=~a" history-length))))
    (a2a-protocol:decode-task
     (%raise-http
      (http:get (%join (%ensure-url backend) (format nil "/tasks/~a" task-id) qs)
                :headers (%headers backend))))))

(defmethod a2a-protocol:list-tasks ((backend httpjson-a2a-backend)
                                    &key context-id status page-size page-token
                                      history-length include-artifacts
                                      status-timestamp-after)
  (%ensure-http)
  (let ((qs (with-output-to-string (s)
              (let ((first t))
                (flet ((add (k v)
                         (when v
                           (format s "~a~a=~a" (if first "" "&") k v)
                           (setf first nil))))
                  (add "contextId" context-id)
                  (add "status" (and status (a2a-protocol:task-state-to-wire status)))
                  (add "pageSize" page-size)
                  (add "pageToken" page-token)
                  (add "historyLength" history-length)
                  (add "includeArtifacts" (and include-artifacts "true"))
                  (add "statusTimestampAfter" status-timestamp-after))))))
    (%raise-http
     (http:get (%join (%ensure-url backend) "/tasks"
                      (and (plusp (length qs)) qs))
               :headers (%headers backend)))))

(defmethod a2a-protocol:cancel-task ((backend httpjson-a2a-backend) task-id &key)
  (%ensure-http)
  (a2a-protocol:decode-task
   (%raise-http
    (http:post (%join (%ensure-url backend) (format nil "/tasks/~a:cancel" task-id))
               :content "{}"
               :headers (%headers backend)))))

(defmethod a2a-protocol:resubscribe-task ((backend httpjson-a2a-backend) task-id
                                          &key on-event)
  (declare (ignore task-id on-event))
  (error 'a2a-protocol:a2a-error
         :message "SubscribeToTask is JSON-RPC only in this binding"
         :code a2a-protocol:+a2a-error-unsupported-operation+))

(defun %ensure-http-server ()
  (or http-server-protocol:*http-server-backend*
      (progn
        (asdf:load-system "http-server-backend-hunchentoot")
        (funcall (find-symbol "USE-HUNCHENTOOT-BACKEND"
                              :http-server-backend-hunchentoot)))))

(defun serve-a2a-httpjson (agent &key (host "127.0.0.1") (port 8080) card)
  (%ensure-http-server)
  (http-server-protocol:serve (make-a2a-app agent :card card)
                              :host host :port port))

(use-httpjson-a2a-backend)
