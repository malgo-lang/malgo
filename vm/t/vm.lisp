(in-package :cl-user)
(defpackage vm-test
  (:use :cl
        :vm
        :prove))
(in-package :vm-test)

;; NOTE: To run this test file, execute `(asdf:test-system :vm)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
