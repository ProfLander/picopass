#lang picopass

(provide (all-defined-out))

[define-language racket/module-path
 #:entry-point module-path
 #:terminals ([id id]
              [string string]
              [rel-string string]
              [user-string string]
              [pkg-string string]
              [nat exact-nonnegative-integer])

 (module-path
   #:datum-literals [submod]

   root-module-path
   (submod root-module-path submod-path-element ...)
   (submod "." submod-path-element ...)
   (submod ".." submod-path-element ...))

 (root-module-path
   #:datum-literals [quote
                     lib
                     file
                     planet]
   (quote id)
   rel-string
   (lib rel-string ...+)
   id
   (file string)
   (planet id)
   (planet string)
   (planet rel-string
           #; (user-string pkg-string vers)
           (user-string pkg-string nat)
           (user-string pkg-string nat minor-vers)
           rel-string ...))

 (submod-path-element
   id
   "..")

 #; (vers
      nat
      (~seq nat minor-vers))

 (minor-vers
   #:datum-literals [= + -]
   nat
   (nat nat)
   (= nat)
   (+ nat)
   (- nat))]

(define-language-classes racket/module-path [module-path module-path])
