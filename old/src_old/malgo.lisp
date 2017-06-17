(in-package :cl-user)
(defpackage malgo
  (:use :cl :trivia.level2 :malgo.ast)
  (:export :evaluate))
(in-package :malgo)

(named-readtables:in-readtable :fare-quasiquote)

(defun map-indexed-term (onvar c term)
  (labels ((walk (c term)
             (match term
               ((list :tmvar x n) (funcall onvar c x n))
               ((list :tmabs x ty t1) (list :tmabs x ty (walk (1+ c) t1)))
               ((list :tmapp t1 t2) (list :tmapp (walk c t1) (walk c t2)))
               (:tmtrue :tmtrue)
               (:tmfalse :tmfalse)
               (:tmunit :tmunit)
               ((list :tmif t1 t2 t3) (list :tmif (walk c t1) (walk c t2) (walk c t3))))))
    (walk c term)))

(defun term-shift-above (d c term)
  (map-indexed-term
   (lambda (c x n) (if (>= x c)
                       (list :tmvar (+ x d) (+ n d))
                       (list :tmvar x (+ n d))))
   c term))

(defun term-shift (d term)
  (term-shift-above d 0 term))

(defun term-subst (j s term)
  (map-indexed-term
   (lambda (c x n) (if (= x (+ j c))
                       (term-shift c s)
                       (list :tmvar x n)))
   0 term))

(defun term-subst-top (s term)
  (term-shift -1 (term-subst 0 (term-shift 1 s) term)))

(define-condition no-rule-applies (error) ())

(defun eval1 (ctx term)
  (match term
    ((list :tmapp (list :tmabs _ _ t12) (guard v2 (valp v2)))
     (term-subst-top v2 t12))
    ((list :tmapp (guard v1 (valp v1)) t2)
     (list :tmapp v1 (eval1 ctx t2)))
    ((list :tmapp t1 t2)
     (list :tmapp (eval1 ctx t1) t2))
    ((list :tmif (guard c (valp c)) then else)
     (if (eq c :tmtrue)
         then
         else))
    ((list :tmif c then else)
     (list :tmif (eval1 ctx c) then else))
    (_ (error 'no-rule-applies))))

(defun eval-lambda (ctx term)
  (handler-case (let ((v (eval1 ctx term)))
                  (eval-lambda ctx v))
    (no-rule-applies () term)))

(defun show-term-and-type (term)
  (concatenate 'string (show-term term) " : " (show-type (malgo.typing:typeof term))))

(defun evaluate (src)
  (let ((result (eval-lambda nil (malgo.parser:parse src))))
    (format t "~A~%" (show-term-and-type result))
    (values result (malgo.typing:typeof result))))
