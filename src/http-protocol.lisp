(in-package :weaver)


;;; request

(defun parse-request (stream)
  "this function get content from stream to meaningful http content
   "
  (alexandria:when-let
      (first-line (read-initial-request-line stream))
    (let* ((split-line (cl-ppcre:split "\\s+" first-line :limit 3))
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
                       (read-http-body stream content-length))))
      (make-instance 'request
                     :headers-in headers
                     :method method
                     :uri uri
                     :args args
                     :raw-body raw-body
                     ))))



;;; response

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

    (format t "writing ~a to stream~%" content)
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
