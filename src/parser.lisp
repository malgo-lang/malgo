(in-package :cl-user)
(defpackage :malgo.parser
  (:use :cl))

;; Lexer
(cl-lex:define-string-lexer malgo-lexer
  ("\"([^\\\"]|\\.)*?\"" (return (values :string (string-trim "\"" $@))))
  ("0|[1-9][0-9]*(\\.[0-9]*)?([e|E][\\+-]?[0-9]+)?" (return (values :number (read-from-string $@))))
  ("[a-zA-Z][a-zA-Z0-9]*" (return (values :symbol $@)))
  ("\\(" (return (values :lparen $@)))
  ("\\)" (return (values :rparen $@)))
  ("," (return (values :colon $@)))
  (";" (return (values :semicolon $@)))
  ("\\+" (return (values :plus $@)))
  ("-" (return (values :minus $@)))
  ("\\*" (return (values :asterisk $@)))
  ("/" (return (values :slash $@)))
  ("%" (return (values :percent $@)))
  ("=" (return (values :equal $@))))
