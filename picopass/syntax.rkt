#lang picopass/impl

; Syntax utilities

(require racket/string
         racket/function
         syntax/parse)

(provide (all-defined-out))

(define (datum=? a b)
  (-> syntax? syntax? boolean?)
  "datum equality over syntax objects"

  (equal? (syntax->datum a)
          (syntax->datum b)))

; Identifier-class pair used by syntax/parse,
; such as ident:id, num:number, or str:string
(define-syntax-class ident+class
  (pattern ident+class:id
           #:with [ident:id class:id]
           (map (compose (curry datum->syntax #'ident+class)
                         string->symbol)
                (string-split (symbol->string (syntax-e #'ident+class))
                              ":"))))

; Split an ident+class syntax object into its ident and class parts,
; and return them as multi-values
(define split-ident+class
  (syntax-parser
    [ident+class:ident+class
     (values #'ident+class.ident #'ident+class.class)]))
