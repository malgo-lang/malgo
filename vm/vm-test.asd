#|
  This file is a part of vm project.
  Copyright (c) 2017 Yuya Kono (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage vm-test-asd
  (:use :cl :asdf))
(in-package :vm-test-asd)

(defsystem vm-test
  :author "Yuya Kono"
  :license "BSD3"
  :depends-on (:vm
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "vm"))))
  :description "Test system for vm"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
