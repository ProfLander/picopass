#lang picopass

(require picopass/lang/racket/datum
         picopass/lang/racket/expanded/raw-module-path)

(provide (all-defined-out))

[define-language raw-provide-spec
 #:entry-point raw-provide-spec
 #:terminals ([id id]
              [local-id id]
              [export-id id]
              [struct-id id]
              [field-id id]
              [prefix-id id]
              [exact-integer exact-integer]
              [raw-module-path raw-module-path-class]
              [datum datum]
              [orig-form datum])

 (raw-provide-spec
   #:datum-literals [for-meta
                     for-syntax
                     for-label
                     protect]

   phaseless-spec
   (for-meta phase-level phaseless-spec ...)
   (for-syntax phaseless-spec ...)
   (for-label phaseless-spec ...)
   (protect raw-provide-spec ...))

 (phase-level
   exact-integer
   #f)

 (phaseless-spec
   #:datum-literals [for-space
                     protect]

   spaceless-spec
   (for-space space spaceless-spec ...)
   (protect phaseless-spec ...))

 (space
   id
   #f)

 (spaceless-spec
   #:datum-literals [rename
                     struct
                     all-from
                     all-from-except
                     all-defined
                     all-defined-except
                     prefix-all-defined
                     prefix-all-defined-except
                     protect
                     expand]

   id
   (rename local-id export-id)
   (struct struct-id (field-id ...))
   (all-from raw-module-path)
   (all-from-except raw-module-path id ...)
   (all-defined)
   (all-defined-except id ...)
   (prefix-all-defined prefix-id)
   (prefix-all-defined-except prefix-id id ...)
   (protect spaceless-spec ...)
   (expand (id . datum))
   (expand (id . datum) orig-form))]

[define-language-classes raw-provide-spec
 [raw-provide-spec-class raw-provide-spec]]

