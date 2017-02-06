(in-package :cl-user)
(defpackage malgo.lambda
  (:use :cl :esrap :trivia.level2 :trivia.ppcre)
  (:import-from :serapeum
                :mvlet)
  (:import-from :parser.common-rules
                :defrule/s))
(in-package :malgo.lambda)

(defrule ws+ (+ (or parser.common-rules:shell-style-comment/trimmed
                    parser.common-rules:whitespace+))
  (:constant nil))
(defrule ws* (* (or parser.common-rules:shell-style-comment/trimmed
                    parser.common-rules:whitespace+))
  (:constant nil))

(defrule skippable ws+)
(defrule skippable? ws*)

(defrule alphanumeric (alphanumericp character))
(defrule alpha (alpha-char-p character))
(defrule lower (lower-case-p character))
(defrule upper (upper-case-p character))

(defrule reserved (or "if ""lambda" "λ" "true" "false")
  (:constant nil))

(defrule/s variable (and  lower (* alphanumeric))
  (:lambda (v) (list :tmvar (text v))))

(defrule/s type (or arrow-type)
  (:lambda (v) v))

(defrule/s factor-type (or (and "(" ws* arrow-type/?s ")")
                           "Bool")
  (:lambda (v)
    (match v
      ((list "(" _ type ")") type)
      ("Bool" :tybool))))

(defrule/s arrow-type (or (and factor-type/?s "->" ws* arrow-type)
                          factor-type)
  (:lambda (v)
    (match v
      ((list ty1 "->" _ ty2) (list :tyarr ty1 ty2))
      (x x))))

(defrule/s typed-var (and variable/?s ":" ws* type/?s)
  (:lambda (v)
    (cons (first v) (fourth v))))

(defrule/s lambda (and (or "lambda" "λ") ws* (+ typed-var/?s) "." ws* term)
  (:destructure
   (l ws1 vars d ws2 term)
   (declare (ignore l ws1 d ws2))
   (let ((result term))
     (dolist (var (reverse vars))
       (setf result (list :tmabs (car var) (cdr var) result)))
     result)))

(defrule/s if (and "if" ws* value/?s value/?s value)
  (:lambda (v) (cons :tmif (cddr v))))

(defun singlep (list)
  (if (null list)
      nil
      (not (cdr list))))

(defrule/s apply (and value/?s (+ value/?s))
  (:lambda (v)
    ;; (list (car v) (cadr v))
    (reduce (lambda (acc c)
              (list :tmapp acc c))
            (cons (car v) (cadr v)))))

(defrule/s bool parser.common-rules:boolean-literal/lower-case
  (:lambda (v)
    (if v :tmtrue :tmfalse)))

(defrule/s value (or parens if bool lambda variable))

(defrule/s parens (and "(" ws* term/?s ")")
  (:function third))

(defrule/s term (or apply value parens))

(defun pick-fresh-name (x ctx)
  (labels ((gen-name (y ctx)
             (if (member y ctx :key #'car :test #'equal)
                 (gen-name (concatenate 'string y "'") ctx)
                 (values y (cons (cons x y) ctx)))))
    (gen-name x ctx)))

(defun get-alpha-name (x ctx)
  (cdr (assoc x ctx :test #'equal)))

(defun alpha-conv (term &optional (ctx nil))
  (match term
    ((list :tmabs (list :tmvar x) ty t1)
     (mvlet ((new-x new-ctx (pick-fresh-name x ctx)))
       (list :tmabs (list :tmvar new-x) ty
             (alpha-conv t1 new-ctx))))
    ((list :tmapp t1 t2)
     (list :tmapp (alpha-conv t1 ctx) (alpha-conv t2 ctx )))
    ((list :tmvar x)
     (list :tmvar (get-alpha-name x ctx)))
    (x x)))

(defun valp (term)
  (match term
    ((list* :tmvar _) t)
    ((or :tmtrue :tmfalse) t)
    (_ nil)))

(defun make-type-context (term)
  "For debugging"
  (let ((ctx nil))
    (labels ((acc (tr)
               (match tr
                 ((list :tmabs (list :tmvar x) ty t1)
                  (progn (setf ctx (cons (cons x ty) ctx))
                         (acc t1)))
                 ((list :tmapp t1 t2) (acc t1) (acc t2))
                 ((guard v (valp v)) ctx)
                 (_ (error "~S is not a term" tr)))))
      (acc term))
    ctx))

(defun name2index (name name-ctx)
  (position name name-ctx :test #'equal))

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

(defun remove-names (term)
  (labels ((acc (tr ctx)
             (match tr
               ((list :tmvar x)
                (list :tmvar (name2index x ctx) (length ctx)))
               ((list :tmabs (list :tmvar x) ty t1)
                (list :tmabs x ty (acc t1 (cons x ctx))))
               ((list :tmapp t1 t2)
                (list :tmapp (acc t1 ctx) (acc t2 ctx)))
               ((list :tmif t1 t2 t3)
                (list :tmif (acc t1 ctx) (acc t2 ctx) (acc t3 ctx)))
               ((guard v (valp v)) v)
               (_ (error "~S is not a term" tr)))))
    (acc term nil)))

(defun parse-lambda (src)
  (remove-names (alpha-conv (parse 'term src))))

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
                    (error "guard of conditional not a boolean: ~S ~S" tr ctx))))))
    (acc term nil)))


(defun map-indexed-term (onvar c term)
  (labels ((walk (c term)
             (match term
               ((list :tmvar x n) (funcall onvar c x n))
               ((list :tmabs x ty t1) (list :tmabs x ty (walk (1+ c) t1)))
               ((list :tmapp t1 t2) (list :tmapp (walk c t1) (walk c t2)))
               (:tmtrue :tmtrue)
               (:tmfalse :tmfalse)
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

(defun rep (src)
  (let ((result (eval-lambda nil (parse-lambda src))))
    (format t "~A : ~A~%" (show-term result) (show-type (typeof result)))
    result))
