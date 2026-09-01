#lang picopass/impl

; Language IR
;
; Represents a named language with atomic terminal forms,
; non-terminal productions, an entrypoint, and a private definition scope

(require (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out)
         (struct-out language))

(struct language [stx
                  ident
                  entry-point-ident
                  description
                  terminals
                  non-terminals
                  scope]
  #:methods gen:custom-write
  ([%define (write-proc self port _mode)
    (display (list 'language
                   (list 'name (syntax->datum (language-ident self)))
                   (list 'entry-point
                         (let ([entry-point
                                (language-entry-point-ident self)])
                           (and entry-point
                                (syntax->datum entry-point))))
                   (list 'description (language-description self))
                   (cons 'terminals (language-terminals self))
                   (cons 'non-terminals (language-non-terminals self)))
             port)]))

(define (language-name self)
  (-> language? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (language-ident self)))

(define (language-context self)
  (-> language? syntax?)
  #:trace #f
  "return the syntactic context of SELF"

  (language-ident self))

(define (language-introduce self stx)
  (-> language? syntax? syntax?)
  #:trace #f
  "add the private scope of SELF to the context of STX"

  ((language-scope self) stx))

