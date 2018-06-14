(in-package :weaver)

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
               (progn (setf uriregstr (concatenate 'string uriregstr "([^/]*)/")) (push the-symbol arglist))
               (setf uriregstr (concatenate 'string uriregstr "/" each "/"))
               )))
    (setf *route-list*
          (append *route-list* (list (list method uriregstr arglist handler))))))

(defun match (method uri route-compile)
  "=> match-or-not, arg-list, handler"
  ;; (format t "~%  match:  ~a ~a ~a ~%" method uri route-compile)
  (let (arg-list handler)
    (if (and (equal method (first route-compile))
             (multiple-value-bind (match regs)
                 (cl-ppcre:scan-to-strings (second route-compile) uri)
               (setf arg-list  regs)
               (setf handler (fourth route-compile))
               ;; (format t "--->match:~a  uri:~a ~%" match uri)
               (string-equal match uri)))
        (values t arg-list handler)
        (values nil nil nil))))


(defun join-list (x y)
  (if y
      (append (list x) y)
      (list x)))

(defun dispatch (request*)
  (format t "~%---Enter funcation dispatch---~%")
  (handler-case
      (let ((method (request-method request*))
            (uri (request-uri request*)))
        ;; (format t "dispatch:  ~a ~a ~a ~%" method uri *route-list*)
        (let ((uri-withslash (make-sure-with-slash uri)))
          (loop for route-compile in *route-list* do
               (multiple-value-bind (match? arg-list handler)
                   (match method uri-withslash route-compile)
                 (when match?
                   (return
                     (apply handler (join-list
                                     request*
                                     (1d-array-to-list arg-list))))))
             finally (return (make-instance 'response
                                            :status-code 404
                                            :content-type "text/plain")))))
    (error (e)
      (log:error "Error happens when dispatch" e)
      (invoke-debugger e)
      ;; (error e)
      )))
