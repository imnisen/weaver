(in-package :weaver)

(defclass response ()
  ((content-type :reader content-type :initarg :content-type)
   (headers :reader response-headers :initarg :headers :initform nil)
   (status-code :reader status-code :initarg :status-code)
   (content :reader response-content :initarg :content :initform nil)))


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
