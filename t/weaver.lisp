(in-package :cl-user)
(defpackage weaver-test
  (:use :cl
        :weaver
        :prove))
(in-package :weaver-test)

;; NOTE: To run this test file, execute `(asdf:test-system :weaver)' in your Lisp.



;; define some test constant
(defvar +address+ "127.0.0.1")
(defvar +port+ 8001)
(defvar +host+ (format nil "http://~a:~a" +address+ +port+))
(defvar +user-agent+ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
(defvar +accept+ "*/*")
(defvar +origin+ +address+)
(defvar +status-ok+ 200)
(defvar +status-not-found+ 404)

(defvar +x-header-key1+ "x-header-key1")
(defvar +x-header-key2+ "x-header-key2")
(defvar +x-header-value1+ "x-header-value1")
(defvar +x-header-value2+ "x-header-value2")

(defvar +x-arg-key1+ "x-arg-key1")
(defvar +x-arg-key2+ "x-arg-key2")
(defvar +x-arg-value1+ "x-arg-value1")
(defvar +x-arg-value2+ "x-arg-value2")

(defvar +x-content-key1+ "x-content-key1")
(defvar +x-content-key2+ "x-content-key2")
(defvar +x-content-value1+ "x-content-value1")
(defvar +x-content-value2+ "x-content-value2")

;; add some route
(defun convert-to-symbol (x)
  (intern (string-upcase x) "KEYWORD"))


;; helper function
(defun uri-to-url (uri &rest args)
  (let ((url (concatenate 'string +host+ uri))
        (arg-string nil))
    (if args
        (concatenate
         'string
         url
         "?"
         (format nil "~{~a~^&~}"
                 (loop for x on args by #'cddr
                    collect (concatenate
                             'string
                             arg-string
                             (first x)
                             "="
                             (second x)))))
        url)))

;; test case
(defun print-hash (h)
  (format t "~%----hashtable begin-----~%")
  (loop for k being the hash-keys of h
     using (hash-value v)
     do (format t "~& ~a => ~a~&" k v))
  (format t "~%----hashtable end -----~%"))


(plan nil)

(ok (weaver:start +address+ +port+) "Server started ?")

(sleep 0.5)

;; test general functions
(weaver:add-route :post "/hello/:name"
                  #'(lambda (req name)
                      (let ((hash (make-hash-table :test 'equal)))
                        (setf (gethash "name" hash) name
                              (gethash "headers" hash) (let ((h (make-hash-table :test 'equal)))
                                                         (setf (gethash +x-header-key1+ h)
                                                               (cdr (assoc (convert-to-symbol +x-header-key1+) (headers-in req)))
                                                               (gethash +x-header-key2+ h)
                                                               (cdr (assoc (convert-to-symbol +x-header-key2+) (headers-in req))))
                                                         h)
                              (gethash "args" hash ) (let ((h (make-hash-table :test 'equal)))
                                                       (setf (gethash +x-arg-key1+ h) (gethash +x-arg-key1+ (request-args req)))
                                                       (setf (gethash +x-arg-key2+ h) (gethash +x-arg-key2+ (request-args req)))
                                                       h)
                              (gethash "content-length" hash ) (cdr (assoc :content-length (headers-in req)))
                              (gethash "raw-body" hash ) (raw-body req)
                              (gethash "body" hash) (let ((h (make-hash-table :test 'equal)))
                                                      (setf (gethash +x-content-key1+ h)
                                                            (gethash +x-content-key1+ (weaver::request-body req))
                                                            (gethash +x-content-key2+ h)
                                                            (gethash +x-content-key2+ (weaver::request-body req)))
                                                      h))
                        (json:encode-json-to-string hash))))

(multiple-value-bind (body status)
    (drakma:http-request (uri-to-url "/hello/weaver" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                         :method :post
                         :content-type "application/json"
                         :additional-headers `((,+x-header-key1+ . ,+x-header-value1+)
                                               (,+x-header-key2+ . ,+x-header-value2+))
                         :content (weaver::make-json-string +x-content-key1+ +x-content-value1+
                                                            +x-content-key2+ +x-content-value2+)
                         )
  (is status +status-ok+)
  (when (> (length body) 0)
    (let* ((body-json (yason:parse body))) ;; base on body type , octets ot string?
      (is "weaver" (gethash "name" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json)))
      (is +x-content-value1+ (gethash +x-content-key1+ (gethash "body" body-json)))
      (is +x-content-value2+ (gethash +x-content-key2+ (gethash "body" body-json)))
      ))
  )


;; test request chunking
(weaver:add-route :post "/chunk"
                  #'(lambda (request)
                      (let ((hash (make-hash-table :test 'equal)))
                        (setf (gethash "content" hash) (raw-body request))
                        (json:encode-json-to-string hash)))
                  )


(multiple-value-bind (body status)
    (let ((continuation (drakma:http-request (uri-to-url "/chunk")
                                             :method :post
                                             :content :continuation)))
      (funcall continuation "foo" t)
      (funcall continuation (list (char-code #\z) (char-code #\a)) t)
      (funcall continuation (lambda (stream)
                              (write-char #\p stream))))
  (is status +status-ok+)
  (when (> (length body) 0)
    (let* ((body-json (yason:parse body)))
      (is "foozap" (gethash "content" body-json))
      ))
  )

;; test Accept-encoding & Content-encoding
(weaver:add-route :get "/accept-encoding"
                  #'(lambda (request)
                      (declare (ignore request))
                      (let ((hash (make-hash-table :test 'equal)))
                        (setf (gethash "hello" hash) "you success!")
                        (json:encode-json-to-string hash))))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))

  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (chipz:make-decompressing-stream :gzip body)
         (chipz:decompress nil (chipz:make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (chipz:make-decompressing-stream :zlib body)
         (chipz:decompress nil (chipz:make-dstate :zlib) body)))
    (T body)))

(multiple-value-bind (body status)
    (drakma:http-request (uri-to-url "/accept-encoding")
                         :method :get
                         :additional-headers `(("accept-encoding" . "deflate"))
                         )
  (is status +status-ok+)
  (when (> (length body) 0)
    (let ((body-json (yason:parse (flex:octets-to-string (decompress-body "deflate" (flex:string-to-octets body))))))
      (is "you success!" (gethash "hello" body-json))
      ))
  )

(multiple-value-bind (body status)
    (drakma:http-request (uri-to-url "/accept-encoding")
                         :method :get
                         :additional-headers `(("accept-encoding" . "gzip"))
                         )
  (is status +status-ok+)
  (when (> (length body) 0)
    (let ((body-json (yason:parse (flex:octets-to-string (decompress-body "gzip" (flex:string-to-octets body))))))
      (is "you success!" (gethash "hello" body-json))
      ))
  )


(ok (weaver:stop) "Server Stopped ?")


(finalize)
