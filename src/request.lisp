(in-package :weaver)

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
             :reader raw-body))
  )

;; make it strong,when (parse-args-string "a=1&b=2,1&c=3&c=4&d")
(defun parse-args-string (string)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for pair-string in (cl-ppcre:split "\\&" string)
       do (progn (format t "~a~%" pair-string)
                 (let* ((pair (cl-ppcre:split "\\=" pair-string))
                        (key (first pair))
                        (value (second pair)))
                   (setf (gethash key hash) value))))
    hash))


(defun read-http-body (stream content-length)
  (format t "content-length: ~a" content-length)
  (p-r
   (with-output-to-string (s)
     (do ((length 0 (1+ length)))
         ((>= length content-length) s)
       ;; (format t "~%~a~%" length)
       (write-char (chunga:read-char* stream nil nil) s)))))
