(in-package :weaver)

;; TODO remove request-* prefix
(defclass request ()
  ((headers-in :initarg :headers-in
               :reader headers-in)
   (method :initarg :method
           :reader request-method)
   (uri :initarg :uri
        :reader request-uri)
   (args :initarg :args
         :reader request-args)
   (raw-body :initarg :raw-body
             :reader raw-body)
   (body :initarg :body
         :reader request-body))
  )

;; make it strong,when (parse-args-string "a=1&b=2,1&c=3&c=4&d")
(defun parse-args-string (string)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for pair-string in (cl-ppcre:split "\\&" string)
       do (let* ((pair (cl-ppcre:split "\\=" pair-string))
                 (key (first pair))
                 (value (second pair)))
            (setf (gethash key hash) value)))
    hash))


(defun read-http-body (stream content-length)
  (with-output-to-string (s)
    (do ((length 0 (1+ length)))
        ((>= length content-length) s)
      ;; (format t "~%~a~%" length)
      (write-char (chunga:read-char* stream nil nil) s))))

;; parse raw-body to structed body according to content-type
;; TODO handle parse error
(defun parse-body (headers raw-body)
  (handler-case
      (alexandria:when-let
          (content-type (p-r (alist-get :content-type headers)))
        (cond ((upcase-equal content-type "application/json")
               (yason:parse raw-body))
              (t nil) ;; not implented
              ))
    (error (e)
      (log:error e)
      nil)))

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
                       (read-http-body stream content-length)))
           (parsed-body (parse-body headers raw-body))
           )
      (make-instance 'request
                     :headers-in headers
                     :method method
                     :uri uri
                     :args args
                     :raw-body raw-body
                     :body parsed-body
                     ))))
