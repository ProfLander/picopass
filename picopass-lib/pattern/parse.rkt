#lang racket/base

; Pattern parsing pipeline
;
; Parses definition syntax and produces pattern IR

(require racket/function
         racket/pretty

         syntax/parse

         picopass/logger

         picopass/pattern/ir)

(provide (all-defined-out))

; Descriptive predicate for parsing patterns with no statically-known literals
(define no-literal? (const #f))

(define-syntax-class (parse-pattern literal?)
  #:description "pattern"

  (pattern (~or #f
                #t
                num:number
                str:string)

           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]

           #:attr struct (p-literal this-syntax))

  (pattern (~datum ...)
           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]
           #:attr struct (p-repeat this-syntax 0))

  (pattern (~datum ...+)
           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]
           #:attr struct (p-repeat this-syntax 1))
  
  (pattern kw:keyword

           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]

           #:attr struct (p-keyword this-syntax))

  (pattern ident:id

           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]

           #:attr struct (if (literal? this-syntax)
                             (p-literal this-syntax)
                             (p-ident this-syntax)))

  (pattern ((~var pat (parse-pattern literal?)) ...)
           
           #:do [(log-picopass-debug "parse-pattern:\n~a"
                                     (pretty-format
                                       (syntax->datum this-syntax)))]

           #:attr struct (p-list this-syntax (attribute pat.struct))))

