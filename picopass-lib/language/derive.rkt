#lang racket/base

; Language derivation macro

(require (for-syntax racket/base
                     racket/syntax

                     syntax/parse

                     picopass/language/derive/define-language-spec
                     picopass/language/derive/syntax-spec))

(provide (all-defined-out))

(define-for-syntax (derive-language/dispatch define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [syntax-spec]
    [(syntax-spec _ ...)
     (syntax-spec->define-language-spec define-language-spec
                                        this-syntax)]))

(define-syntax derive-language
  (syntax-parser
    [(_ name:id
        (~seq #:entry-point entry-point:id)
        (~optional (~seq #:description description:string))
        (~optional (~and #:for-syntax (~bind [for-syntax #t])))
        target)

     (with-syntax* ([def-lang
                      (derive-language/dispatch
                       (make-define-language-spec #'name
                                                  #'entry-point
                                                  (attribute description))
                       #'target)]
                    [def-lang (if (attribute for-syntax)
                                  #'(begin-for-syntax def-lang)
                                  #'def-lang)])

       #`(begin
           def-lang
           target))]))

