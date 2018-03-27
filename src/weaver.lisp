(in-package :cl-user)
(defpackage weaver
  (:use :cl))
(in-package :weaver)


;;; route

;; (ql:quickload '(:usocket :bordeaux-threads :chunga :cl-ppcre :flexi-streams))

(defvar *route-list* nil)

(defun add-route (method uri handler)
  "method-> :get :post
   url-> /hello/:name
   handle-> a function"
  (let* ((urilist (cl-ppcre:split "/" uri))
         (urilistclean (remove "" urilist :test #'equal))
         (uriregstr "")
         (arglist nil))
    (loop for each in urilistclean do
         (multiple-value-bind (column-start? the-symbol) (start-with-column each)
           (if column-start?
               (progn (setf uriregstr (concatenate 'string uriregstr "/" "([^/]*)/")) (push the-symbol arglist))
               (setf uriregstr (concatenate 'string uriregstr "/" each "/"))
               )))
    (setf *route-list*
          (append *route-list* (list (list method uriregstr arglist handler))))))

(defun start-with-column (st)
  "return two values: t or nil, symbol"
  (if (char-equal #\: (aref st 0))
      (values t (intern (string-upcase (subseq st 1))))
      (values nil nil)))

(defun make-sure-with-slash (st)
  (if (string-equal "/" (subseq st (1- (length st))))
      st
      (concatenate 'string st "/")))

(defun match (method uri route-compile)
  "=> match-or-not, arg-list, handler"
  (format t "match:  ~a ~a ~a ~%" method uri route-compile)
  (let (arg-list handler)
    (if (and (equal method (first route-compile))
             (multiple-value-bind (match regs)
                 (cl-ppcre:scan-to-strings (second route-compile) uri)
               (setf arg-list  regs)
               (setf handler (fourth route-compile))
               (string-equal match uri)))
        (values t arg-list handler)
        (values nil nil nil))))

;; 1d array -> list
(defun 1d-array-to-list (a-array)
  (loop for i below (array-dimension a-array 0)
     collect (aref a-array i)))

(defun dispatch (method uri)
  (format t "dispatch:  ~a ~a ~a ~%" method uri *route-list*)
  (let ((uri-withslash (make-sure-with-slash uri)))
    (loop for route-compile in *route-list* do
         (multiple-value-bind (match? arg-list handler)
             (match method uri-withslash route-compile)
           (when match?
             (return (progn
                       (format t "call handler")
                       (apply handler (1d-array-to-list arg-list))))))
       finally (return 'no-match))))



;;; web server



(defmacro force-format (stream &rest args)
  `(progn
     (funcall #'format ,stream ,@args)
     (force-output ,stream)))


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

(defun read-initial-request-line (stream)
  (chunga:with-character-stream-semantics
    (handler-case
        (usocket:with-mapped-conditions ()
          (chunga:read-line* stream))
      ((or end-of-file usocket:timeout-error) ()))))

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
             (result (dispatch method uri)))
        (send-response stream headers result)))))

;; output content from client
;; plus some mark then return to client
(defun handle-connection (socket)
  (let* ((connection (usocket:socket-accept socket))
         (stream (usocket:socket-stream connection)))
    (handle-request stream)
    (force-format *standard-output* "Conection ends.~%")
    (usocket:socket-close connection)))

(defun accept-connection (socket)
  (bt:make-thread
   (lambda () (handle-connection socket))))

(defun start (host port)
  (usocket:with-socket-listener (socket host port :reuse-address t :element-type '(unsigned-byte 8))
    (loop when (usocket:wait-for-input socket) do
         (accept-connection socket))))


(defun test-client (host port)
  (usocket:with-client-socket (socket stream host port)
    (loop for i below 10 do
         (progn
           (force-format stream "Request once: ~a ~%" i)
           (sleep 1)
           (force-format *standard-output* "From server:~a~%" (read-line stream nil 'the-end))))
    (force-format *standard-output* "Outof loop")))
