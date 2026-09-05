#lang picopass

(require picopass/lang/racket/datum
         picopass/lang/racket/expanded/raw-module-path)

(provide (all-defined-out))

[define-language raw-require-spec
 #:entry-point raw-require-spec
 #:terminals ([id id]
              [portal-id id]
              [prefix-id id]
              [local-id id]
              [exported-id id]
              [content datum]
              [exact-integer exact-integer]
              [raw-module-path raw-module-path-class])

 (raw-require-spec
   #:datum-literals [for-meta
                     for-syntax
                     for-template
                     for-label
                     just-meta
                     portal]

   phaseless-spec
   (for-meta phase-level raw-require-spec ...)
   (for-syntax raw-require-spec ...)
   (for-template raw-require-spec ...)
   (for-label raw-require-spec ...)
   (just-meta phase-level raw-require-spec ...)
   (portal portal-id content))

 (phase-level
   exact-integer
   #f)

 (phaseless-spec
   #:datum-literals [for-space
                     just-space]

   spaceless-spec
   (for-space space phaseless-spec ...)
   (just-space space spaceless-spec ...))

 (space
   id
   #f)

 (spaceless-spec
   #:datum-literals [only
                     prefix
                     all-except
                     prefix-all-except
                     rename]

   raw-module-path
   (only raw-module-path id ...)
   (prefix prefix-id raw-module-path)
   (all-except raw-module-path id ...)
   (prefix-all-except prefix-id
                      raw-module-path id ...)
   (rename raw-module-path local-id exported-id))]

[define-language-classes raw-require-spec
 [raw-require-spec-class raw-require-spec]]

