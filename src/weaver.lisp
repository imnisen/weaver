(in-package :weaver)

(defun handle-request (stream)
  (alexandria:when-let (r (parse-request stream))
    (make-response stream (dispatch r))))

;; output content from client
;; plus some mark then return to client
(defun handle-connection (socket)
  (handler-case
      (let* ((connection (usocket:socket-accept socket))
             (stream (usocket:socket-stream connection)))
        (handle-request stream)
        (force-format *standard-output* "Conection ends.~%")
        ;; (usocket:socket-close connection) ;;need it?
        (close stream)
        )
    (error (c)
      (format t "error happens when handle-connection->: ~A~%" C))))

(DEFUN ACCEPT-CONNECTION (SOCKET)
  (HANDLER-CASE (BT:MAKE-THREAD
                 (LAMBDA () (HANDLE-CONNECTION SOCKET)))
    (ERROR (C)
      (FORMAT T "ERROR HAPPENS WHEN MAKE-THREAD")
      (ERROR C))))


;; (defvar *keep-going* t)
(defvar *running-socket* nil)

(defun serve (host port)
  (handler-case
      (usocket:with-socket-listener
          (socket host port :reuse-address t :element-type '(unsigned-byte 8))
        (format t "Listening for connections...~%")
        (setf *running-socket* socket)
        (loop while (usocket:wait-for-input socket :ready-only t)
           do (accept-connection socket))
        (format t "Server stop"))
    (error (e)
      (format t "error occurs:  ~a" e))))

(defun start (host port &key ((:foreground foreground) nil))
  (if foreground
      (serve host port)
      (handler-case (bt:make-thread
                     (lambda () (serve host port)))
        (error (c)
          (format t "error happens when make-thread")
          (error c)))))

;; TODO fix it , now close rudely
(defun stop ()
  ;; (setf *keep-going* nil)
  (usocket:socket-close *running-socket*))
