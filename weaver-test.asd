#|
  This file is a part of weaver project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

(in-package :cl-user)
(defpackage weaver-test-asd
  (:use :cl :asdf))
(in-package :weaver-test-asd)

(defsystem weaver-test
  :author "Nisen"
  :license ""
  :depends-on (:weaver
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "weaver"))))
  :description "Test system for weaver"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
