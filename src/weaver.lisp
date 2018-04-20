(in-package :weaver)

(defun handle-request (stream)
  (let ((first-line (read-initial-request-line stream)))
    (when first-line
      (let* ( ;; extract request info here
             (split-line (cl-ppcre:split "\\s+" first-line :limit 3))
             (method (chunga:as-keyword (first split-line)))
             (url-string (second split-line))
             (match-start (position #\? url-string))
             (uri (if match-start
                      (subseq url-string 0 match-start)
                      url-string))
             (args (if match-start
                       (parse-args-string (subseq url-string (1+ match-start)))
                       (makehash))) ;; params arg
             (headers (p-r (chunga:read-http-headers stream)))
             (content-length (make-sure-number (p-r (cdr (assoc :CONTENT-LENGTH headers)))))
             (raw-body (when content-length
                         (read-http-body stream content-length)))
             (request* (make-instance 'request
                                      :headers-in headers
                                      :method method
                                      :uri uri
                                      :args args
                                      :raw-body raw-body
                                      ))
             (r (dispatch request*)))
        (make-response stream r)))))

;; output content from client
;; plus some mark then return to client
(defun handle-connection (socket)
  (let* ((connection (usocket:socket-accept socket))
         (stream (usocket:socket-stream connection)))
    (handle-request stream)
    (force-format *standard-output* "Conection ends.~%")
    (usocket:socket-close connection) ;;need it?
    ;; (close stream)
    ))

(defun accept-connection (socket)
  (handler-case (bt:make-thread
                 (lambda () (handle-connection socket)))
    (error (c)
      (format t "error happens when make-thread")
      (error c))))

(defun start (host port)
  (usocket:with-socket-listener
      (socket host port :reuse-address t :element-type '(unsigned-byte 8))
    (format t "Listening for connections...~%")
    (loop when (usocket:wait-for-input socket :ready-only t) do
         (accept-connection socket))))



;; ;; test
;; (defun test-client (host port)
;;   (usocket:with-client-socket (socket stream host port)
;;     (loop for i below 10 do
;;          (progn
;;            (force-format stream "Request once: ~a ~%" i)
;;            (sleep 1)
;;            (force-format *standard-output* "From server:~a~%" (read-line stream nil 'the-end))))
;;     (force-format *standard-output* "Outof loop")))


;; ;; ;; test case
(add-route :get "/case1/:name"
           #'(lambda (req name) (format nil "hello ~a, header: ~a" name (headers-in req))))

(add-route :get "/case2/:name"
           #'(lambda (req name) (format nil "goodbye ~a" name)))

(add-route :get "/case3/:name"
           #'(lambda (req name) (make-instance 'response
                                          :content (format nil "welcome ~a" name)
                                          :status-code 200
                                          :content-type "plain/text"
                                          :headers (makehash :xxx "yyy"))))

(add-route :get "/case4/:name"
           #'(lambda (req name)
               (format nil "hello ~a, req.args: x1:  ~a" name (gethash "x1" (request-args req) ))))


(add-route :post "/case6/:name"
           #'(lambda (req name)
               (format nil "hello ~a, req.args: x1:  ~a, req.content.length:~a, req.body: ~a"
                       name
                       (gethash "x1" (request-args req))
                       (cdr (assoc :content-length (headers-in req)))
                       (raw-body req))))

;; (weaver::start #(127 0 0 1) 8000)
