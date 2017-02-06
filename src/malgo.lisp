(in-package :cl-user)
(defpackage malgo
  (:use :cl :esrap :trivia.level2 :trivia.ppcre)
  (:import-from :parser.common-rules
                :defrule/s))
(in-package :malgo)

(defstruct (id (:constructor id (name))) name)

(defrule ws+ (+ (or parser.common-rules:shell-style-comment/trimmed
                    parser.common-rules:whitespace+))
  (:constant nil))
(defrule ws* (* (or parser.common-rules:shell-style-comment/trimmed
                    parser.common-rules:whitespace+))
  (:constant nil))

(defrule skippable ws+)
(defrule skippable? ws*)

(defrule bool-literal
    parser.common-rules:boolean-literal/lower-case
  (:lambda (b) (list :bool b)))

(defrule string-literal
    (or parser.common-rules:string-literal/double-quotes
        parser.common-rules:string-literal/single-quotes)
  (:lambda (s) (list :string s)))

(defrule float-literal
    parser.common-rules:float-literal
  (:lambda (f) (list :float f)))

(defrule integer-literal
    parser.common-rules:integer-literal
  (:lambda (i) (list :integer i)))

(defrule alphanumeric (alphanumericp character))
(defrule lower (lower-case-p character))

(defrule/s ident (and (! "let") (! "in") (! "def") (alpha-char-p character) (* alphanumeric)))
(defrule/s variable (and lower (? ident))
  (:lambda (v) (list :var (text (car v) (cdr v)))))

(defrule/s let
    (and "let" ws+ variable ws* "=" ws* expr/?s)
  (:destructure
   (let ws1 var-name ws2 eq ws3 e1)
   (declare (ignore let ws1 ws2 eq ws3))
   (list :let var-name e1)))


(defrule/s parens
    (and "(" ws* expr ws* ")")
  (:destructure
   (lparen ws1 inner ws2 rparen)
   (declare (ignore lparen ws1 ws2 rparen))
   inner))

(defrule/s simple-expr
    (or parens
        bool-literal
        string-literal
        float-literal
        integer-literal
        variable))

(defun make-multiply (parsed rest)
  (if (null rest)
      parsed
      (let ((next (car rest)))
        (make-multiply (list (if (equal "*" (first next))
                            :mul
                            :div)
                        parsed
                        (third next))
                  (cdr rest)))))

(defrule/s multiply
    (and add/?s (* (and (or "*" "/") ws* add/?s)))
  (:destructure (x rest)
                (make-multiply x rest)))

(defun make-add (parsed rest)
  (if (null rest)
      parsed
      (let ((next (car rest)))
        (make-add (list (if (equal "+" (first next))
                            :add
                            :sub)
                        parsed
                        (third next))
                  (cdr rest)))))

(defrule/s add
    (and simple-expr/?s (* (and (or "+" "-") ws* simple-expr/?s)))
  (:destructure (x rest)
                (make-add x rest)))

(defrule/s funcall
    (and variable/?s (+ simple-expr/?s))
  (:destructure (func args)
                (list :funcall func args)))


(defrule/s expr
    (or funcall/?s
        multiply/?s))

(defrule/s stat
    (or let/?s funcall/?s ";"))

(defrule/s stats
    (+ stat/?s)
  (:lambda (ast)
    (remove ";" ast :test #'equal)))
