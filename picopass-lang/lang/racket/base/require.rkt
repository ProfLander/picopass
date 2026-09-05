#lang picopass

(require picopass/lang/racket/base/module-path)

(provide (all-defined-out))

[define-language racket/require
 #:entry-point top-level
 #:terminals ([id id]
              [prefix-id id]
              [orig-id id]
              [bind-id id]
              [exact-integer exact-integer]
              [module-path module-path-class])

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
   (only-in require-spec id-maybe-renamed ...)
   (except-in require-spec id ...)
   (prefix-in prefix-id require-spec)
   (rename-in require-spec [orig-id bind-id] ...)
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

 (id-maybe-renamed
   id
   [id id])

 (phase-level
   exact-integer
   #f)

 (space
   id
   #f) ]

(define-language-parser parse-require racket/require)

