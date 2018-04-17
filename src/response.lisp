(in-package :weaver)

(defclass response ()
  ((content-type :reader content-type :initarg :content-type)
   (headers :reader headers :initarg :headers)
   (status-code :reader status-code :initarg :status-code)
   (content :reader response-content :initarg :content)))


(defgeneric make-response (stream response)
  (:documentation "make response to stream with response"))

(defmethod make-response (stream (response response))
  (send-response stream response))

(defun make-default-headers () ())

(defmethod make-response (stream (response t))
  (send-response stream (make-instance 'response
                                       :content-type "application/json" ;; need to guess with response
                                       :headers (make-default-headers)
                                       :status-code 200
                                       :content response)))

(defvar +code-to-phrase+ (makehash 200 "ok"
                                   500 "Internal Server Error"))

(defgeneric send-response (stream response)
  (:documentation "send response to stream")
  (:method (stream response)
    (let* ((h-stream (flex:make-flexi-stream stream))
           (status-code (status-code response))
           (reason-phrase (gethash status-code +code-to-phrase+))
           (content (response-content response)))
      (format h-stream "HTTP/1.1 ~D ~A~C~C"  status-code reason-phrase #\Return #\Linefeed)
      (format h-stream "~C~C" #\Return #\Linefeed)
      (write-sequence content h-stream)
      (finish-output stream))))
