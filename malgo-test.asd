#|
  This file is a part of malgo project.
  Copyright (c) 2016 Yuya Kono (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage malgo-test-asd
  (:use :cl :asdf))
(in-package :malgo-test-asd)

(defsystem malgo-test
  :author "Yuya Kono"
  :license "MIT"
  :depends-on (:malgo
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "malgo"))))
  :description "Test system for malgo"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
