(in-package :weaver)


;;; route

;; (ql:quickload '(:usocket :bordeaux-threads :chunga :cl-ppcre :flexi-streams))

(defun send-response (stream headers content)
  (declare (ignore headers))
  (destructuring-bind (status-code content reason-phrase)
      (if (equal content 'no-match)
          (list 510 "<html><p>No match</p></html>" "Internal1 Server Error")
          (list 200 (concatenate 'string "<html><p>" content "</p></html>") "OK"))
    (let ((h-stream (flex:make-flexi-stream stream)))
      (format h-stream "HTTP/1.1 ~D ~A~C~C" status-code reason-phrase #\Return #\Linefeed)
      (format h-stream "~C~C" #\Return #\Linefeed)
      (write-sequence content h-stream)
      (finish-output stream)))
  )


(defun make-response (response* stream)
  (send-response stream nil (response-content response*)))



(defun handle-request (stream)
  (let ((first-line (read-initial-request-line stream)))
    (when first-line
      (let* ((split-line (cl-ppcre:split "\\s+" first-line :limit 3))
             (method (chunga:as-keyword (first split-line)))
             (url-string (second split-line))
             (uri (let ((match-start (position #\? url-string)))
                    (if match-start
                        (subseq url-string 0 match-start)  ;; drop args info here
                        url-string)))

             (headers (chunga:read-http-headers stream))
             (request* (make-instance 'request
                                      :headers-in headers
                                      :method method
                                      :uri uri))
             (result (dispatch request*))
             (response* (make-instance 'response
                                       :content result)))
        (make-response response* stream)))))

;; output content from client
;; plus some mark then return to client
(defun handle-connection (socket)
  (let* ((connection (usocket:socket-accept socket))
         (stream (usocket:socket-stream connection)))
    (handle-request stream)
    (force-format *standard-output* "Conection ends.~%")
    (usocket:socket-close connection) ;;need it?
    ;;(close stream)
    ))

(defun accept-connection (socket)
  (bt:make-thread
   (lambda () (handle-connection socket))))

(defun start (host port)
  (usocket:with-socket-listener
      (socket host port :reuse-address t :element-type '(unsigned-byte 8))
    (format t "Listening for connections...~%")
    (loop when (usocket:wait-for-input socket) do
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
;; (add-route :get "/hello/:name"
;;            #'(lambda (req name) (format nil "hello ~a, header: ~a" name (headers-in req))))

;; (add-route :get "/goodbye/:name"
;;            #'(lambda (req name) (format nil "goodbye ~a" name)))

;; (weaver::start #(127 0 0 1) 8000)
