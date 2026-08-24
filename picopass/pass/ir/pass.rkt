#lang picopass/impl

; Pass IR
;
; Represents a named transformation between two languages,
; with a series of processors defining translation between
; specific language forms, and a private definition scope

(require (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out)
         (struct-out pass))

(struct pass [stx
              ident
              input
              output
              processors
              self-ref
              scope]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'pass
                           (list 'name (syntax->datum (pass-ident self)))
                           (list 'input (let ([input (pass-input self)])
                                          (if (syntax? input)
                                              (syntax->datum input)
                                              input)))
                           (list 'output (let ([output (pass-output self)])
                                           (if (syntax? output)
                                               (syntax->datum output)
                                               output)))
                           (cons 'processors (pass-processors self)))
                     port))])

(define (pass-name self)
  (-> pass? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (pass-ident self)))

(define (pass-context self)
  (-> pass? syntax?)
  #:trace #f
  "return the syntactic context of SELF"

  (pass-ident self))

(define (datum->pass-syntax self datum)
  (-> pass? any/c syntax?)
  #:trace #f
  "convert DATUM to syntax in the context of SELF"

  (datum->syntax (pass-context self) datum))

(define (pass-introduce self stx)
  (-> pass? syntax? syntax?)
  #:trace #f
  "add the private scope of SELF to the context of STX"

  ((pass-scope self) stx))

(define (pass-introduce-datum self datum)
  (-> pass? any/c syntax?)
  #:trace #f
  "convert DATUM to syntax in the context of SELF,
   and add the private scope of SELF to it"

  ((pass-scope self) (datum->pass-syntax self datum)))

; Scoped multi-definition form of datum->pass-syntax
(define-syntax (with-pass-syntax stx)
  (syntax-parse stx
    [(_ pass ([key val] ...) body ...)
     #'(with-syntax ([key (datum->pass-syntax pass val)]
                     ...)
         body
         ...)]))

; Scoped multi-definition form of pass-introduce-datum
(define-syntax (with-pass-bindings stx)
  (syntax-parse stx
    [(_ pass ([key val] ...) body ...)
     #'(with-syntax ([key (pass-introduce-datum pass val)]
                     ...)
         body
         ...)]))

