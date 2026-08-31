#lang picopass/impl

; Language error handling machinery

(require racket/string

         picopass/pass/ir/pass
         picopass/pass/ir/processor
         picopass/pass/ir/processor-clause)

(provide (all-defined-out))

; Pass

(define (raise-pass-error self
                          message
                          [sub-expr #f]
                          [extra-sources null])
  (->* [pass? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error (pass-name self)
   message
   (pass-stx self)
   sub-expr
   extra-sources])

(define (raise-pass-input-predicate-error pass-name pred-name)
  (-> symbol? symbol? none/c)
  (error
    (format "~a: input predicate ~a returned false"
            pass-name
            pred-name)))

(define (raise-pass-output-predicate-error pass-name pred-name value)
  (-> symbol? symbol? any/c none/c)
  (error
    (format "~a: output predicate ~a returned false\n  value: ~a"
            pass-name
            pred-name
            value)))

(define (raise-pass-dispatch-error pass-name in)
  (-> symbol? any/c none/c)
  (error
    (format "~a: no predicates matched input:\n~a"
            pass-name
            in)))

; Processor

(define (raise-processor-error self
                               message
                               [sub-expr #f]
                               [extra-sources null])
  (->* [processor? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error (processor-name self)
   message
   (processor-stx self)
   sub-expr
   extra-sources])

(define (raise-processor-output-predicate-error pass-name
                                                pred-name
                                                pattern
                                                value)
  (-> symbol? symbol? string? any/c none/c)
  (error
    (format (string-join
              (list "~a: output predicate ~a returned false"
                    "  clause: ~a"
                    "  value: ~a")
              "\n")
            pass-name
            pred-name
            pattern
            value)))

; Processor Clause

(define (raise-processor-clause-error self
                                      message
                                      [sub-expr #f]
                                      [extra-sources null])
  (->* [processor-clause? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error 'processor-clause
   message
   (processor-clause-stx self)
   sub-expr
   extra-sources])

(define (raise-processor-clause-invalid-pattern-error self
                                                      lang-name
                                                      pattern)
  (-> processor-clause? symbol? syntax? none/c)

  [raise-processor-clause-error self
   (format "not a non-terminal production of ~a"
           lang-name)
   pattern])

(define (raise-processor-clause-terminal-rec-error self pattern)
  (-> processor-clause? syntax? none/c)
  [raise-processor-clause-error self
   "~rec may not be called on a terminal"
   pattern])

