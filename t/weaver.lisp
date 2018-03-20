(in-package :cl-user)
(defpackage weaver-test
  (:use :cl
        :weaver
        :prove))
(in-package :weaver-test)

;; NOTE: To run this test file, execute `(asdf:test-system :weaver)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
