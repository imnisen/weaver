(in-package :weaver)

(defun handle-request (stream)
  (format t "----handle-request---")
  ;; (make-response stream nil))
  (let ((request (handler-case
                     (parse-request stream)
                   (error (e)
                     (log:warn "Error occours while parsing request" e)
                     (make-response stream "parse-request-error"))))) ;; temp behavior
    (make-response stream (dispatch request)))) ;; here function interface not beautiful

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
      (log:error "Error occurs when handle connection" e))))

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

(defun serve (address port)
  (handler-case
      (usocket:with-socket-listener
          (socket address port :reuse-address t :element-type '(unsigned-byte 8))
        (log:info "Listening for connections...")

        ;; (setf *running-socket* socket)
        (loop
           (if (not *keep-going*) (return))

           (when (usocket:wait-for-input socket :ready-only t :timeout 2)
             (accept-socket socket)))

        (log:info "Server stop...")
        )
    (error (e)
      (log:error "Error occous when in serve" e))))

(defun start (address port &key ((:foreground foreground) nil))
  (let ((*keep-going* t))
    (if foreground
        (serve address port)
        (handler-case (bt:make-thread
                       (lambda () (serve address port)))
          (error (e)
            (log:error "Error occours when in start" e)
            (error e))))))

;; TODO fix it , now close rudely
(defun stop ()
  (setf *keep-going* nil)
  (sleep 1)
  t
  ;; (usocket:socket-close *running-socket*)
  )
