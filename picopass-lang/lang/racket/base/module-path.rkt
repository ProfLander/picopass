#lang picopass

(provide (all-defined-out))

(define-syntax-class minor-vers
  #:datum-literals [= + -]
  (pattern nat:exact-integer)
  (pattern (min:exact-integer patch:exact-integer))
  (pattern (= nat:exact-integer))
  (pattern (+ nat:exact-integer))
  (pattern (- nat:exact-integer)))

(define-splicing-syntax-class vers
  (pattern nat:exact-integer)
  (pattern (~seq nat:exact-integer vers:minor-vers)))

(define-syntax-class planet
  (pattern (planet lib:id))
  (pattern (planet lib:string))
  (pattern (planet package:string
                   (user:string pkg:string vers:vers)
                   collection:string ...)))

[define-language module-path
 #:entry-point module-path
 #:terminals ([id id]
              [string string]
              [rel-string string]
              [planet planet])

 (module-path
   #:datum-literals [submod]

   root-module-path
   (submod root-module-path submod-path-element ...)
   (submod "." submod-path-element ...)
   (submod ".." submod-path-element ...))

 (root-module-path
   #:datum-literals [quote
                     lib
                     file]
   (quote id)
   rel-string
   (lib rel-string ...+)
   id
   (file string)
   planet)

 (submod-path-element
   id
   "..")]

(define-language-classes module-path [module-path-class module-path])

