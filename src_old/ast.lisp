(in-package :cl-user)
(defpackage malgo.ast
  (:use :cl :trivia.level2)
  (:export :malgo-type-p
           :malgo-term-p
           :valp
           :name2index
           :index2name
           :show-term
           :show-type))
(in-package :malgo.ast)

(named-readtables:in-readtable :fare-quasiquote)

(defun malgo-type-p (type)
  (match type
    (:tybool t)
    (:tyunit t)
    (`(:tyarr t1 t2) (and (malgo-type-p t1)
                          (malgo-type-p t2)))
    (_ nil)))

(defun valp (term)
  (match term
    ((list* :tmvar _) t)
    ((or :tmtrue :tmfalse) t)
    (:tmunit t)
    (_ nil)))

(defun malgo-term-p (term)
  (match term
    ((guard v (valp v)) term)
    (`(:tmabs _ _ _) term)
    (`(:tmapp _ _) term)
    (`(:tmif _ _ _) term)
    (_ nil)))

(defun name2index (name name-ctx)
  (position name name-ctx :test #'equal))

(defun index2name (index name-ctx)
  (elt name-ctx index))

(defparameter *newline-char* #\newline)

(defun show-term% (ctx term)
  (match term
    ((list :tmabs x ty t1)
     (list "(lambda " x ":" (show-type ty) ". " (show-term% (cons x ctx) t1) ")"))
    (`(:tmapp (:tmabs :ignore :tyunit ,t1) ,t2)
      (list (show-term% ctx t1) ";" *newline-char* (show-term% ctx t2)))
    ((list :tmapp t1 t2) (list "(" (show-term% ctx t1) " " (show-term% ctx t2) ")"))
    ((list :tmvar k n) (if (= n (length ctx))
                           (index2name k ctx)
                           (error "bad index")))
    (:tmtrue "true")
    (:tmfalse "false")
    ((list :tmif t1 t2 t3) (list "(if " (show-term% ctx t1) " " (show-term% ctx t2) " " (show-term% ctx t3) ")"))
    (:tmunit "unit")
    (_ (error "bad term"))))

(defun show-type (type)
  (match type
    (:tybool "Bool")
    (:tyunit "Unit")
    ((list :tyarr (list :tyarr ty11 ty12) ty2)
     (format nil "(~A -> ~A) -> ~A"
             (show-type ty11) (show-type ty12)
             (show-type ty2)))
    ((list :tyarr ty1 ty2) (format nil "~A -> ~A"
                                   (show-type ty1)
                                   (show-type ty2)))))

(defun show-term (term)
  (esrap:text (show-term% nil term)))
