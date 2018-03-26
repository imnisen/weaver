#|
  This file is a part of weaver project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

#|
  Author: Nisen (imnisen@gmail.com)
|#

(in-package :cl-user)
(defpackage weaver-asd
  (:use :cl :asdf))
(in-package :weaver-asd)

(defsystem weaver
  :version "0.1"
  :author "Nisen"
  :license ""
  :depends-on (:cl-ppcre
               :usocket
               :bordeaux-threads)
  :components ((:module "src"
                        :components
                        ((:file "weaver"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op weaver-test))))
