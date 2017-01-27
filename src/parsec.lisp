(in-package :cl-user)
(defpackage :malgo.parsec
  (:use :cl :trivia.level2 :trivia.ppcre)
  (:import-from :serapeum
                :mvlet
                :mvlet*))
(in-package :malgo.parsec)

;; sample
;; (let* ((s1 (source "abc123"))
;;        (s1a (many alpha s1))
;;        (s1b (many digit s1)))
;;   (format t "~a,~a" s1a s1b))
;; > abc,123

(defun tail (seq)
  (declare (type sequence seq))
  (the sequence (if (= (length seq) 0)
                    seq
                    (subseq seq 1))))

(defstruct (source
             (:constructor source (seq &aux (start 0) (end (length seq)))))
  (seq nil :type sequence)
  (start 0 :type (integer 0))
  (end 0 :type (integer 0)))

(defun current (source)
  (declare (type source source))
  (if (finishedp source)
      nil
      (elt (source-seq source) (source-start source))))

(defun next (source)
  (declare (type source source))
  (if (finishedp source)
      nil
      (setf (source-start source) (1+ (source-start source)))))

(defun finishedp (source)
  (declare (type source source))
  (= (source-start source) (source-end source)))

(defun parse-test (p s)
  (handler-case (funcall p s)
    (error (c) c)))

(defun any (s)
  (declare (type source s))
  (if (finishedp s)
      (failure)
      (success s)))

(defun satisfy (f s)
  (if (funcall f (current s))
      (success s)
      (failure)))

(defun test (s)
  )
