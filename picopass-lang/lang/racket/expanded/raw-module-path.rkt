#lang picopass

(require picopass/lang/racket/datum
         picopass/lang/racket/base/module-path)

(provide (all-defined-out))

(define-syntax-class path
  (pattern path:string
           #:when (path-string? (syntax-e #'path))))

(define-syntax-class planet
  (pattern (planet rel:string
                   (user:string pkg:string vers:vers ...))))

[define-language raw-module-path
 #:entry-point raw-module-path
 #:terminals ([id id]
              [string string]
              [rel-string string]
              [literal-path path]
              [planet planet])

 (raw-module-path
   #:literals [submod]

   raw-root-module-path
   (submod raw-root-module-path id ...+)
   (submod "." id ...+))

 (raw-root-module-path
   #:literals [quote
               lib
               file
               planet]

   (quote id)
   rel-string
   (lib rel-string ...)
   id
   (file string)
   planet
   literal-path)]

[define-language-classes raw-module-path
 [raw-module-path-class raw-module-path]]

