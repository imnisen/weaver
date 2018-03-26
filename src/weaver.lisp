(in-package :cl-user)
(defpackage weaver
  (:use :cl))
(in-package :weaver)


;;; route

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
               (setf uriregstr (concatenate 'string uriregstr "/" each))
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
  (let ((uri-withslash (make-sure-with-slash uri)))
    (loop for route-compile in *route-list* do
         (multiple-value-bind (match? arg-list handler)
             (match method uri-withslash route-compile)
           (when match?
             (return (apply handler (1d-array-to-list arg-list)))))
       finally (return "sorry no match"))))



;;; web server

(ql:quickload :usocket :bordeaux-threads)
(defmacro force-format (stream &rest args)
  `(progn
     (funcall #'format ,stream ,@args)
     (force-output ,stream)))

;; output content from client
;; plus some mark then return to client
(defun handle-connection (socket)
  (let* ((connection (usocket:socket-accept socket))
         (stream (usocket:socket-stream connection)))
    (loop for line = (read-line stream nil nil)
       while line do (progn
                       (force-format *standard-output* "Client send: ~a~%" line)
                       (force-format stream "~a +1~%" line)))
    (force-format *standard-output* "Conection ends.~%")))

(defun accept-connection (socket)
  (bt:make-thread
   (lambda () (handle-connection socket))))

(defun start (host port)
  (usocket:with-socket-listener (socket host port :reuse-address t)
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
