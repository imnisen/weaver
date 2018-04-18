(in-package :weaver)


(defun start-with-column (st)
  "return two values: t or nil, symbol"
  (if (char-equal #\: (aref st 0))
      (values t (intern (string-upcase (subseq st 1))))
      (values nil nil)))

(defun make-sure-with-slash (st)
  (if (string-equal "/" (subseq st (1- (length st))))
      st
      (concatenate 'string st "/")))

;; 1d array -> list
(defun 1d-array-to-list (a-array)
  (loop for i below (array-dimension a-array 0)
     collect (aref a-array i)))

(defmacro force-format (stream &rest args)
  `(progn
     (funcall #'format ,stream ,@args)
     (force-output ,stream)))


(defun read-initial-request-line (stream)
  (chunga:with-character-stream-semantics
    (handler-case
        (usocket:with-mapped-conditions ()
          (chunga:read-line* stream))
      ((or end-of-file usocket:timeout-error) ()))))


;; demo:
;; (makehash :data "Fuck you, pay me!"
;;           :headers (my-makehash "x-more-info" "http://vimeo.com/22053820"))
(defun makehash (&rest init-values)
  (let ((h (make-hash-table :test 'equal)))
    (loop for x on init-values by #'cddr
       do (setf (gethash (first x) h) (second x)) )
    h))


;;; demo
;;; (sethash h :a nil)
;;; (sethash h :a nil :allow-nil nil)  :a 的 值nil 不会被设置
;; need 打磨
(defun sethash (h &rest set-values &key (allow-nil t supplied-p) &allow-other-keys)
  "这个方法会直接改变传入的hashtable ;如果传入了allow-nil 参数，那么更新hashtable时把rest参数里的allow nil和值剔除掉"
  (if supplied-p
      (if allow-nil
          (loop for x on set-values by #'cddr
             do (if (not (equal (first x) :allow-nil))
                    (setf (gethash (first x) h) (second x))))
          (loop for x on set-values by #'cddr
             do (if (and (not (equal (first x) :allow-nil))
                         (second x))
                    (setf (gethash (first x) h) (second x)))))
      (loop for x on set-values by #'cddr
         do (setf (gethash (first x) h) (second x))))
  h)

;; helper-fun
;; (in-package :cl)
(defun print-hash (h)
  (format t "~%----hashtable begin-----~%")
  (loop for k being the hash-keys of h
     using (hash-value v)
     do (format t "~& ~a => ~a~&" k v))
  (format t "~%----hashtable end -----~%"))

(defun p-r (x)
  (format t "~%----p-r begin-----~%")
  (format t "~a" x)
  (format t "~%----p-r end -----~%")
  x)


(defun make-sure-number (x)
  (cond ((stringp x) (parse-integer x :junk-allowed t))
        ((numberp x) x)
        (t nil)))
