(in-package :cl-user)
(defpackage malgo.parser
  (:use :cl :esrap :trivia.level2 :malgo.ast)
  (:shadow :parse)
  (:import-from :serapeum
                :mvlet)
  (:import-from :parser.common-rules
                :defrule/s)
  (:export :parse))
(in-package :malgo.parser)

(named-readtables:in-readtable :fare-quasiquote)

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

(defrule reserved (or "if ""lambda" "λ" "unit" "true" "false")
  (:constant nil))

(defrule/s variable (or "_"
                        (and (! reserved) lower (* alphanumeric)))
  (:lambda (v) (if (equal "_" v)
                   (list :tmvar :ignore)
                   (list :tmvar (text v)))))

(defrule/s type (or arrow-type)
  (:lambda (v) v))

(defrule/s factor-type (or (and "(" ws* arrow-type/?s ")")
                           "Bool"
                           "Unit")
  (:lambda (v)
    (match v
      (`("(" ,_ ,type ")") type)
      ("Bool" :tybool)
      ("Unit" :tyunit))))

(defrule/s arrow-type (or (and factor-type/?s "->" ws* arrow-type)
                          factor-type)
  (:lambda (v)
    (match v
      (`(,ty1 "->" ,_ ,ty2) (list :tyarr ty1 ty2))
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

(defrule/s seq (and term/?s ";" ws* term/?s)
  (:destructure
   (t1 colon ws t2)
   (declare (ignore colon ws))
   `(:tmapp (:tmabs (:tmvar :ignore) :tyunit ,t1)
            ,t2)))

(defrule/s if (and "if" ws* value/?s value/?s value)
  (:lambda (v) (cons :tmif (cddr v))))

(defun singlep (list) (if (null list)
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

(defrule/s unit "unit"
  (:constant :tmunit))

(defrule/s value (or parens if bool unit lambda variable))

(defrule/s parens (and "(" ws* term/?s ")")
  (:function third))

(defrule/s term (or apply seq value parens))

(defun pick-fresh-name (x ctx)
  (if (eq x :ignore)
      x
      (labels ((gen-name (y ctx)
                 (if (member y ctx :key #'car :test #'equal)
                     (gen-name (concatenate 'string y "'") ctx)
                     (values y (cons (cons x y) ctx)))))
        (gen-name x ctx))))

(defun get-alpha-name (x ctx)
  (if (eq x :ignore)
      x
      (cdr (assoc x ctx :test #'equal))))

(defun alpha-conv (term &optional (ctx nil))
  (match term
    (`(:tmabs (:tmvar ,x) ,ty ,t1)
     (mvlet ((new-x new-ctx (pick-fresh-name x ctx)))
       `(:tmabs (:tmvar ,new-x) ,ty
                ,(alpha-conv t1 new-ctx))))
    ((list :tmapp t1 t2)
     `(:tmapp ,(alpha-conv t1 ctx) ,(alpha-conv t2 ctx )))
    ((list :tmvar x)
     `(:tmvar ,(get-alpha-name x ctx)))
    (x x)))

(defun remove-names (term)
  (labels ((acc (tr ctx)
             (match tr
               (`(:tmvar ,x)
                `(:tmvar ,(name2index x ctx) ,(length ctx)))
               (`(:tmabs (:tmvar ,x) ,ty ,t1)
                `(:tmabs ,x ,ty ,(acc t1 (cons x ctx))))
               (`(:tmapp ,t1 ,t2)
                `(:tmapp ,(acc t1 ctx) ,(acc t2 ctx)))
               (`(:tmif ,t1 ,t2 ,t3)
                `(:tmif ,(acc t1 ctx) ,(acc t2 ctx) ,(acc t3 ctx)))
               ((guard v (valp v)) v)
               (_ (error "~S is not a term" tr)))))
    (acc term nil)))

(defun parse (src)
  (remove-names (alpha-conv (esrap:parse 'term src))))
