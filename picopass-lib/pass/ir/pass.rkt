#lang picopass/impl

; Pass IR
;
; Represents a named transformation between two languages,
; with a series of processors defining translation between
; specific language forms, and a private definition scope

(require (for-syntax racket/base
                     syntax/parse)

         racket/function

         picopass/language/ir/language)

(provide (all-defined-out)
         (struct-out pass))

(struct pass [stx
              ident
              input-ident
              output-ident
              processors
              self-ref
              scope]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'pass
                           (list 'name (syntax->datum (pass-ident self)))
                           (list 'input (syntax->datum (pass-input-ident self)))
                           (list 'output (syntax->datum (pass-output-ident self)))
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

(define (pass-input self)
  (-> pass? (or/c language? syntax?))
  (let ([input-ident (pass-input-ident self)])
    (syntax-local-value input-ident (thunk input-ident))))

(define (pass-output self)
  (-> pass? (or/c language? syntax?))
  (let ([output-ident (pass-output-ident self)])
    (syntax-local-value output-ident (thunk output-ident))))

(define (pass-introduce self stx)
  (-> pass? syntax? syntax?)
  #:trace #f
  "add the private scope of SELF to the context of STX"

  ((pass-scope self) stx))

