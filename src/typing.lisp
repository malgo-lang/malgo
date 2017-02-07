(in-package :cl-user)
(defpackage malgo.typing
  (:use :cl :trivia.level2)
  (:export :typeof))
(in-package :malgo.typing)

(named-readtables:in-readtable :fare-quasiquote)

(defun get-type-from-context (i ctx)
  (cdr (nth i ctx)))

(defun add-binding (name type ctx)
  (cons (cons name type) ctx))

(defun typeof (term)
  (labels ((acc (tr ctx)
             (match tr
               ((list :tmvar i _) (get-type-from-context i ctx))
               ((list :tmabs x tyt1 t2)
                (let* ((new-ctx (add-binding x tyt1 ctx))
                       (tyt2 (acc t2 new-ctx)))
                  (list :tyarr tyt1 tyt2)))
               ((list :tmapp t1 t2)
                (let* ((tyt1 (acc t1 ctx))
                       (tyt2 (acc t2 ctx)))
                  (match tyt1
                    ((list :tyarr tyt11 tyt12)
                     (if (equal tyt2 tyt11)
                         tyt12
                         (error "parameter type mismatch: ~S ~S" tr ctx)))
                    (_ (error "arrow type expected: ~S ~S" tr ctx)))))
               (:tmtrue :tybool)
               (:tmfalse :tybool)
               ((list :tmif t1 t2 t3)
                (if (equal (acc t1 ctx) :tybool)
                    (let ((tyt2 (acc t2 ctx)))
                      (if (equal tyt2 (acc t3 ctx))
                          tyt2
                          (error "arms of condtional have different types: ~S ~S" tr ctx)))
                    (error "guard of conditional not a boolean: ~S ~S" tr ctx)))
               (:tmunit :tyunit))))
    (acc term nil)))
