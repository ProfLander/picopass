#lang racket/base

; Picopass implementation language
;
; Extends racket/base with racket/contract,
; and conditionally enables contracts based on picopass/config

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/lib/function-header
                     picopass/config)

         (rename-in racket/base
                    [define %define])

         (rename-in racket/contract
                    [define/contract %define/contract])

         racket/string
         racket/pretty

         syntax/parse/define

         picopass/logger)

(provide (all-from-out racket/base)
         (all-from-out racket/contract)
         (all-from-out racket/pretty)
         (all-defined-out))

; Choose define or define/contract based on the value of `use-contracts`
[define-syntax-parser define
 #:datum-literals [-> ->*]

 [(_ ident:id exp:expr)

  #'(%define ident exp)]

 [(_ header:function-header

     (~and ((~or -> ->*) _ ...)
           ctr)

     (~optional (~seq #:trace (~or (~and #t (~bind [trace #t]))
                                   (~and #f (~bind [trace #f]))))
                #:defaults ([trace #t]))

     (~optional (~seq #:trace-depth trace-depth:number)
                #:defaults ([trace-depth #'4]))


     body:expr ...)

  (with-syntax* ([define (if use-contracts
                             #'%define/contract
                             #'%define)]
                 [(arg ...) (syntax->list #'header.params)]
                 [(trace ...)
                  (if (attribute trace)
                      #'((parameterize ([pretty-print-depth trace-depth]
                                        [pretty-print-columns 80])
                           (log-picopass-debug
                             "~a:\n~a\n"
                             'header.name
                             (string-join
                               (list
                                 (let ([arg-str (symbol->string 'arg)])
                                   (format " ~a: ~a"
                                           arg-str
                                           (string-replace
                                             (pretty-format arg #:mode 'write)
                                             "\n"
                                             [string-append "\n"
                                              (make-string (+ 3 (string-length arg-str))
                                                           #\space)])))
                                 ...)
                               "\n"))))
                      #'())])
    (syntax/loc this-syntax
      [define header
        ctr
        trace ...
        body 
        ...]))]]

