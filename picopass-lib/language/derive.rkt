#lang racket/base

; Language derivation macro

(require (for-syntax racket/base

                     syntax/parse

                     picopass/language/derive/define-language-spec
                     picopass/language/derive/syntax-spec))

(provide (all-defined-out))

(define-for-syntax (derive-language/dispatch define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [syntax-spec]
    [(syntax-spec _ ...)
     (syntax-spec->define-language-spec define-language-spec this-syntax)]))

(define-syntax derive-language
  (syntax-parser
    [(_ name:id
        (~seq #:entry-point entry-point:id)
        (~optional (~seq #:description description:string))
        target)

     (with-syntax ([define-language
                    (derive-language/dispatch
                      (make-define-language-spec #'name
                                                 #'entry-point
                                                 (attribute description))
                      #'target)])

       #`(begin
           (begin-for-syntax
             define-language)
           target))]))

