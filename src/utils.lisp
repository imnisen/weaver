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
