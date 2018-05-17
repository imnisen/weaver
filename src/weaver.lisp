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
        (log:info "Connection ends")
        ;; (usocket:socket-close connection) ;;need it?
        (close stream)
        )
    (error (e)
      (log:error e))))

(defun accept-connection (socket)
  (handler-case (bt:make-thread
                 (lambda () (handle-connection socket)))
    (error (e)
      (log:error e)
      (error e))))


;; (defvar *keep-going* t)
(defvar *running-socket* nil)

(defun serve (host port)
  (handler-case
      (usocket:with-socket-listener
          (socket host port :reuse-address t :element-type '(unsigned-byte 8))
        (log:info "Listening for connections...")
        (setf *running-socket* socket)
        (loop while (usocket:wait-for-input socket :ready-only t)
           do (accept-connection socket))
        (log:info "Server stop.")
        )
    (error (e)
      (log:error e))))

(defun start (host port &key ((:foreground foreground) nil))
  (if foreground
      (serve host port)
      (handler-case (bt:make-thread
                     (lambda () (serve host port)))
        (error (e)
          (log:error e)
          (error e)))))

;; TODO fix it , now close rudely
(defun stop ()
  ;; (setf *keep-going* nil)
  (usocket:socket-close *running-socket*))
