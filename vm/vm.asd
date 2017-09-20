#|
  This file is a part of vm project.
  Copyright (c) 2017 Yuya Kono (takohati0821@gmail.com)
|#

#|
  Author: Yuya Kono (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage vm-asd
  (:use :cl :asdf))
(in-package :vm-asd)

(defsystem vm
  :version "0.1"
  :author "Yuya Kono"
  :license "BSD3"
  :depends-on (:trivia
               :serapeum)
  :components ((:module "src"
                :components
                ((:file "vm"))))
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
  :in-order-to ((test-op (test-op vm-test))))
