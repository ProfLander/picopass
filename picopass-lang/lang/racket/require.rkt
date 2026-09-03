#lang picopass

(provide (all-defined-out))

[define-language racket/require
 #:entry-point top-level
 #:terminals ([ident id]
              [exact-integer exact-integer]
              [nat exact-nonnegative-integer]
              [string string]
              [rel-string string]
              [user-string string]
              [pkg-string string])

 (top-level
   #:datum-literals [require]
   (require require-spec ...))

 (require-spec
   #:datum-literals [only-in
                     except-in
                     prefix-in
                     rename-in
                     combine-in
                     relative-in
                     only-meta-in
                     only-space-in
                     for-syntax
                     for-template
                     for-label
                     for-meta
                     for-space]

   module-path
   (only-in require-spec ident-maybe-renamed ...)
   (except-in require-spec ident ...)
   (prefix-in ident ident ...)
   (rename-in require-spec [ident ident] ...)
   (combine-in require-spec ...)
   (relative-in module-path require-spec ...)
   (only-meta-in phase-level require-spec ...)
   (only-space-in space require-spec ...)
   (for-syntax require-spec ...)
   (for-template require-spec ...)
   (for-label require-spec ...)
   (for-meta phase-level require-spec ...)
   (for-space space require-spec ...)
   #;derived-require-spec)

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

   (quote ident)
   rel-string
   (lib rel-string ...+)
   ident
   (file string)
   (planet ident)
   (planet string)
   (planet rel-string
           #;(user-string pkg-string vers)
           (user-string pkg-string nat)
           (user-string pkg-string nat minor-vers)
           rel-string ...))

 (submod-path-element
   ident
   "..")

 (ident-maybe-renamed
   ident
   [ident ident])

 (phase-level
   exact-integer
   #f)

 (space
   ident
   #f)

 #;(vers
   nat
   (~seq nat minor-vers))

 (minor-vers
   #:datum-literals [= + -]
   nat
   (nat nat)
   (= nat)
   (+ nat)
   (- nat))]

(define-language-parser parse-require racket/require)

