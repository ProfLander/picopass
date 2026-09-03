#lang picopass

(require picopass/lang/racket/module-path)

(provide (all-defined-out))

[define-language racket/provide
 #:entry-point top-level
 #:terminals ([id id]
              [orig-id id]
              [export-id id]
              [prefix-id id]
              [exact-integer exact-integer]
              [module-path module-path])

 (top-level
   #:datum-literals [provide]

   (provide provide-spec ...))

 (provide-spec
   #:datum-literals [all-defined-out
                     all-from-out
                     rename-out
                     except-out
                     prefix-out
                     struct-out
                     combine-out
                     protect-out
                     for-meta
                     for-syntax
                     for-template
                     for-label
                     for-space]

   id
   (all-defined-out)
   (all-from-out module-path ...)
   (rename-out [orig-id export-id] ...)
   (except-out provide-spec provide-spec ...)
   (prefix-out prefix-id provide-spec)
   (struct-out id)
   (combine-out provide-spec ...)
   (protect-out provide-spec ...)
   (for-meta phase-level provide-spec ...)
   (for-syntax provide-spec ...)
   (for-template provide-spec ...)
   (for-label provide-spec ...)
   (for-space space provide-spec ...)
   #; derived-provide-spec)

 (phase-level
   exact-integer
   #f)

 (space
   id
   #f)]

