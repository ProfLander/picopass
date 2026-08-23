#lang picopass/impl

; Language error handling machinery

(require picopass/pattern/ir
         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

; Language

(define (raise-language-error self
                              message
                              [sub-expr #f]
                              [extra-sources null])
  (->* [language? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error (language-name self)
   message
   (language-stx self)
   sub-expr
   extra-sources])

(define (raise-language-entry-point-error lang entry-point)
  (-> language? syntax? none/c)

  [raise-language-error lang
   "entry-point is not a valid non-terminal"
   #f
   (list entry-point)])

(define (raise-language-duplicate-symbol-error lang duplicate)
  (-> language? syntax? none/c)

  [raise-language-error lang
   "duplicate symbol name"
   duplicate])

; Terminal

(define (raise-terminal-error self
                              message
                              [sub-expr #f]
                              [extra-sources null])
  (->* [terminal? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error (terminal-name self)
   message
   (terminal-stx self)
   sub-expr
   extra-sources])

(define (raise-terminal-unbound-class-error self class)
  (-> terminal? syntax? none/c)
  [raise-terminal-error self
   "unbound terminal class"
   class])

; Non-Terminal

(define (raise-non-terminal-error self
                                  message
                                  [sub-expr #f]
                                  [extra-sources null])
  (->* [non-terminal? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error (non-terminal-name self)
   message
   (non-terminal-stx self)
   sub-expr
   extra-sources])

(define (raise-non-terminal-duplicate-literal-error non-terminal 
                                                    duplicate)
  (-> language? syntax? none/c)
  [raise-non-terminal-error non-terminal
   "duplicate literal"
   duplicate])

(define (raise-non-terminal-duplicate-production-error non-terminal 
                                                       duplicate)
  (-> non-terminal? syntax? none/c)
  [raise-non-terminal-error non-terminal
   "duplicate production"
   (pattern-stx duplicate)])

; Production

(define (raise-production-error self
                                message
                                [sub-expr #f]
                                [extra-sources null])
  (->* [pattern? string?]
       [(or/c syntax? #f)
        (or/c (listof syntax?) #f)]
       none/c)
  "raise a syntax error in the context of SELF,
   with the given MESSAGE, SUB-EXPR, and EXTRA-SOURCES"

  [raise-syntax-error 'production
   message
   (pattern-stx self)
   sub-expr
   extra-sources])

(define (raise-production-invalid-ident-error prod lang ident)
  (-> pattern? language? syntax? none/c)
  [raise-production-error prod
   (format "not a literal, datum-literal, terminal, or non-terminal of ~a"
           (language-name lang))
   ident])

; Language Delta

(define ((raise-missing-removed-terminal-error name) _target missing)
  (-> symbol? (-> (listof any/c) (listof any/c) none/c))

  [raise-syntax-error name
   "removed terminals missing from base language"
   #f
   #f
   (map terminal-stx missing)])

; Non-Terminal Delta

(define ((raise-missing-removed-literal-error lctx) _target missing)
  (-> syntax? (-> (listof any/c) (listof any/c) none/c))

  (raise-syntax-error 'extend-non-terminal
                      "removed literal missing from base language"
                      lctx
                      #f
                      missing))

(define ((raise-missing-removed-datum-literal-error lctx) _target missing)
  (-> syntax? (-> (listof any/c) (listof any/c) none/c))

  (raise-syntax-error 'extend-non-terminal
                      "removed datum literal missing from base language"
                      lctx
                      #f
                      missing))

(define ((raise-missing-removed-production-error lctx) _target missing)
  (-> syntax? (-> (listof any/c) (listof any/c) none/c))

  (printf "missing: ~a\n" missing)
  (raise-syntax-error 'extend-non-terminal
                      "removed production missing from base language"
                      lctx
                      #f
                      (map pattern-stx missing)))
