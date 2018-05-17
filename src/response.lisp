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

(defvar +code-to-phrase+ (makehash 200 "ok"
                                   500 "Internal Server Error"
                                   404 "Not Found"))

(defgeneric send-response (stream response)
  (:documentation "send response to stream"))

(defmethod send-response (stream response)
  (let* ((h-stream (flex:make-flexi-stream stream))
         (status-code (status-code response))
         (reason-phrase (gethash status-code +code-to-phrase+))
         (headers (response-headers response))
         (content (response-content response)))
    (format h-stream "HTTP/1.1 ~D ~A~C~C"  status-code reason-phrase #\Return #\Linefeed)
    ;; write headers
    ;; tmp add content-type, latter move it to headers, todo fix latter
    (write-header-line "Content-Type" (content-type response) h-stream)
    (when headers
      (loop for key being the hash-keys in headers using (hash-value value) do
           (write-header-line (chunga:as-capitalized-string key) value h-stream)))

    (format h-stream "~C~C" #\Return #\Linefeed)

    (log:debug "writing ~a to stream~%" content)
    (write-sequence content h-stream)
    (finish-output stream)))

;; copy from hunchentoot, TODO 稍后研究
(defgeneric write-header-line (key value stream)
  (:documentation "Accepts a string KEY and a Lisp object VALUE and
writes them directly to the client as an HTTP header line.")
  (:method (key (string string) stream)
    (write-string key stream)
    (write-char #\: stream)
    (write-char #\Space stream)
    (let ((start 0))
      (loop
         (let ((end (or (position #\Newline string :start start)
                        (length string))))
           ;; skip empty lines, as they confuse certain HTTP clients
           (unless (eql start end)
             (unless (zerop start)
               (write-char #\Tab stream))
             (write-string string stream :start start :end end)
             (write-char #\Return stream)
             (write-char #\Linefeed stream))
           (setf start (1+ end))
           (when (<= (length string) start)
             (return))))))
  (:method (key (number number) stream)
    (write-header-line key (write-to-string number :escape nil :readably nil :base 10) stream))
  (:method (key value stream)
    (write-header-line key (princ-to-string value) stream)))
