#|
  This file is a part of malgo project.
  Copyright (c) 2016 Yuya Kono (takohati0821@gmail.com)
|#

#|
  Author: Yuya Kono (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage malgo-asd
  (:use :cl :asdf))
(in-package :malgo-asd)

(defsystem malgo
  :version "0.1"
  :author "Yuya Kono"
  :license "MIT"
  :depends-on (:alexandria
               :fare-quasiquote
               :named-readtables
               :esrap
               :parser.common-rules
               :proc-parse
               :serapeum
               :trivia
               :trivia.level2
               :trivia.ppcre)
  :components ((:module "src"
                :components
                (
                 (:file "malgo")
                 )))
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
  :in-order-to ((test-op (test-op malgo-test))))
