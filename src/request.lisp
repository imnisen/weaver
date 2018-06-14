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

(define-condition request-header-error (error) ())
(defvar +buffer-length+ 8192)
(deftype octet ()
  "A shortcut for \(UNSIGNED-BYTE 8)."
  '(unsigned-byte 8))

(defun read-http-body (stream content-length chunkedp)
  (format t "-----read-http-body----~%")
  (cond ((and content-length chunkedp) (error 'request-header-error))
        (content-length
         (with-output-to-string (s)
           (do ((length 0 (1+ length)))
               ((>= length content-length) s)
             ;; (format t "~%~a~%" length)
             (write-char (chunga:read-char* stream nil nil) s))))
        (chunkedp
         (p-r
          (flexi-streams:octets-to-string
           (let ((s (chunga:make-chunked-stream stream)))
             (setf (chunga:chunked-stream-input-chunking-p s) t)
             (loop with buffer = (make-array +buffer-length+ :element-type 'octet)
                with content = (make-array 0 :element-type 'octet :adjustable t)
                for index = 0 then (+ index pos)
                for pos = (read-sequence buffer s)
                do (adjust-array content (+ index pos))
                  (replace content buffer :start1 index :end2 pos)
                while (= pos +buffer-length+)
                finally (return content)))
           )))
        (t (error 'request-header-error))))

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
(defun make-sure-bool (x)
  (if x t nil))
(defun parse-request (stream)
  "this function get content from stream to meaningful http content
   "
  (format t "---parse-request----")
  (handler-case
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

               (transfer-encodings (cdr (assoc :transfer-encoding headers)))

               (chunkedp (make-sure-bool
                          (and transfer-encodings
                               (member "chunked" (cl-ppcre:split "\\s*,\\s*" transfer-encodings) :test #'equalp)))
                 )

               (content-length (make-sure-number (cdr (assoc :CONTENT-LENGTH headers))))

               (raw-body (read-http-body stream content-length chunkedp))
               (parsed-body (parse-body headers raw-body)))


          (p-r (make-instance 'request
                              :headers-in headers
                              :method method
                              :uri uri
                              :args args
                              :raw-body raw-body
                              :body parsed-body
                              ))))
    (error (e)
      (log:error "Error occurs when parse request" e)
      (error e))))
