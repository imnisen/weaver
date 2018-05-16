(in-package :cl-user)
(defpackage weaver-test
  (:use :cl
        :weaver
        :prove))
(in-package :weaver-test)

;; NOTE: To run this test file, execute `(asdf:test-system :weaver)' in your Lisp.

(plan nil)

(ok (weaver:start "127.0.0.1" 8888) "Server started ?")

(sleep 0.5)


;; define some test constant
(defvar +user-agent+ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
(defvar +accept+ "*/*")
(defvar +origin+ "127.0.0.1")
(defvar +host+ "127.0.0.1:8888")
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

(defmacro p-r (x)
  (let* ((g (gensym)))
    `(let ((,g ,x))
       (format t "~%----p-r begin origin form ~a -----~%" ',x)
       (format t "~a" ,g)
       (format t "~%----p-r end -----~%")
       ,g)))

;; add some route
(defun convert-to-symbol (x)
  (intern (string-upcase x) "KEYWORD"))
(weaver:add-route :post "/hello/:name"
                  #'(lambda (req name)
                      (let ((hash (make-hash-table :test 'equal)))
                        (p-r (headers-in req))
                        (setf (gethash "name" hash) name
                              (gethash "headers" hash) (let ((h (make-hash-table :test 'equal)))
                                                         (setf (gethash +x-header-key1+ h)
                                                               (cdr (assoc (convert-to-symbol +x-header-key1+) (headers-in req)))
                                                               (gethash +x-header-key2+ h)
                                                               (cdr (p-r (assoc (p-r (convert-to-symbol +x-header-key2+)) (headers-in req)))))
                                                         h)
                              (gethash "args" hash ) (let ((h (make-hash-table :test 'equal)))
                                                       (setf (gethash +x-arg-key1+ h) (gethash +x-arg-key1+ (request-args req)))
                                                       (setf (gethash +x-arg-key2+ h) (gethash +x-arg-key2+ (request-args req)) )
                                                       h)
                              (gethash "content-length" hash ) (cdr (assoc :content-length (headers-in req)))
                              (gethash "raw-body" hash ) (raw-body req))
                        (json:encode-json-to-string hash))))
;; helper function
(defun uri-to-url (uri &rest args)
  (let ((url (format nil "http://127.0.0.1:8888~a" uri))
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

(multiple-value-bind (body status)
    (drakma:http-request (uri-to-url "/hello/weaver" +x-arg-key1+ +x-arg-value1+ +x-arg-key2+ +x-arg-value2+)
                         :method :post
                         :additional-headers `((,+x-header-key1+ . ,+x-header-value1+)
                                               (,+x-header-key2+ . ,+x-header-value2+))
                         ;; :content `((,+x-content-key1+ . ,+x-content-value1+)
                         ;;            (,+x-content-key2+ . ,+x-content-value2+))
                         )
  (format t "~a~%" status)
  (format t "---> bodyis : ~a~%" (yason:parse (flexi-streams:octets-to-string body)))
  (print-hash (yason:parse (flexi-streams:octets-to-string body)))
  (print-hash (gethash "headers" (yason:parse (flexi-streams:octets-to-string body))))
  (print-hash (gethash "args" (yason:parse (flexi-streams:octets-to-string body))))
  (is status +status-ok+)
  (when body
    (let* ((body-json (yason:parse (flexi-streams:octets-to-string body))))
      (is "weaver" (gethash "name" body-json))
      (is +x-arg-value1+ (gethash +x-arg-key1+ (gethash "args" body-json)))
      (is +x-arg-value2+ (gethash +x-arg-key2+ (gethash "args" body-json)))
      (is +x-header-value1+ (gethash +x-header-key1+ (gethash "headers" body-json)))
      (is +x-header-value2+ (gethash +x-header-key2+ (gethash "headers" body-json))) ))
  )



(ok (weaver:stop) "Server Stopped ?")


(finalize)
