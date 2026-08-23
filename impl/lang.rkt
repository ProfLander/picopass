#lang racket/base

; Picopass implementation language
;
; Extends racket/base with racket/contract,
; and conditionally enables contracts based on picopass/config

(require (for-syntax racket/base
                     syntax/parse/lib/function-header
                     picopass/config)

         (rename-in racket/base
                    [define %define])

         (rename-in racket/contract
                    [define/contract %define/contract])

         syntax/parse/define

         picopass/logger)

(provide (all-from-out racket/base)
         (all-from-out racket/contract)
         (all-defined-out))

; Choose define or define/contract based on the value of `use-contracts`
[define-syntax-parser define
 #:datum-literals [-> ->*]

 [(_ ident:id exp:expr)

  #'(%define ident exp)]

 [(_ header:function-header
     (~and ((~or -> ->*) _ ...)
           ctr)
     body:expr ...)

  (with-syntax ([ident-str (symbol->string (syntax-e #'header.name))]
                [define (if use-contracts
                            #'%define/contract
                            #'%define)])
    (syntax/loc this-syntax
      [define header
        ctr
        (log-picopass-debug ident-str)
        body ...]))]]

