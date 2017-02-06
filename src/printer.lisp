(in-package :cl-user)
(defpackage malgo.printer
  (:use :cl :trivia.level2))

(defun index2name (index name-ctx)
  (elt name-ctx index))

(defun show-term% (ctx term)
  (match term
    ((list :tmabs x ty t1)
     (list "(lambda " x ":" (show-type ty) ". " (show-term% (cons x ctx) t1) ")"))
    ((list :tmapp t1 t2) (list "(" (show-term% ctx t1) " " (show-term% ctx t2) ")"))
    ((list :tmvar k n) (if (= n (length ctx))
                           (index2name k ctx)
                           (error "bad index")))
    (:tmtrue "true")
    (:tmfalse "false")
    ((list :tmif t1 t2 t3) (list "(if " (show-term% ctx t1) " " (show-term% ctx t2) " " (show-term% ctx t3) ")"))
    (_ (error "bad term"))))

(defun show-type (type)
  (match type
    (:tybool "Bool")
    ((list :tyarr (list :tyarr ty11 ty12) ty2)
     (format nil "(~A -> ~A) -> ~A"
             (show-type ty11) (show-type ty12)
             (show-type ty2)))
    ((list :tyarr ty1 ty2) (format nil "~A -> ~A"
                                   (show-type ty1)
                                   (show-type ty2)))))

(defun show-term (term)
  (text (show-term% nil term)))
