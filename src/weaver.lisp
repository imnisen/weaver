(in-package :weaver)

(defun handle-request (stream)
  (format t "----handle-request---")
  (alexandria:when-let (request (parse-request stream))
    (make-response stream (dispatch request))))

(defparameter *finish-processing-socket* nil)

(defun handle-connection (connection)
  (handler-case
      (let* ((stream (usocket:socket-stream connection))
             (*finish-processing-socket* t))


        (unwind-protect
             (loop
                (handle-request stream)
                (when *finish-processing-socket*
                  (return)))
          (close stream)
          (usocket:socket-close connection)
          (log:info "Connection ends")
          ))
    (error (e)
      (log:error e))))

(defun accept-socket (socket)
  (let ((connection (handler-case
                        (usocket:socket-accept socket)
                      ;; ignore condition here?
                      (usocket:connection-aborted-error ()))))
    (handler-case (bt:make-thread
                   (lambda () (handle-connection connection)))
      (error (e)
        (usocket:socket-close connection) ;; 考虑这个是否妥当
        (log:error "Error while creating worker thread for new incoming connection")
        ))))

(defparameter *keep-going* t)
;; (defvar *running-socket* nil)
;; (defvar *host* nil)
;; (defvar *port* nil)

(defun serve (host port)
  (handler-case
      (usocket:with-socket-listener
          (socket host port :reuse-address t :element-type '(unsigned-byte 8))
        (log:info "Listening for connections...")

        ;; (setf *running-socket* socket)
        (loop
           (if (not *keep-going*) (return))

           (when (usocket:wait-for-input socket :ready-only t :timeout 2)
             (accept-socket socket)))

        (log:info "Server stop...")
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
  (setf *keep-going* nil)
  (sleep 1)
  t
  ;; (usocket:socket-close *running-socket*)
  )
