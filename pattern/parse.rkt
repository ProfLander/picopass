#lang racket/base

; Pattern parsing pipeline
;
; Parses definition syntax and produces pattern IR

(require racket/function

         syntax/parse

         picopass/pattern/ir)

(provide (all-defined-out))

; Descriptive predicate for parsing patterns with no statically-known literals
(define no-literal? (const #f))

(define-syntax-class (parse-pattern literal?)
  #:description "pattern"
  (pattern (~datum ...)
           #:attr struct (p-repeat this-syntax 0))
  (pattern (~datum ...+)
           #:attr struct (p-repeat this-syntax 1))
  (pattern ident:id
           #:attr struct (if (literal? this-syntax)
                             (p-literal this-syntax)
                             (p-ident this-syntax)))
  (pattern ((~var pat (parse-pattern literal?)) ...)
           #:attr struct (p-list this-syntax (attribute pat.struct))))

