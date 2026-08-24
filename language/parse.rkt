#lang racket/base

; Language parsing pipeline
;
; Parses definition syntax and produces language IR

(require racket/pretty

         syntax/parse

         picopass/syntax
         picopass/delta
         picopass/logger

         picopass/pattern/ir
         picopass/pattern/parse

         picopass/language/ir/language
         picopass/language/ir/language-delta
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal
         picopass/language/ir/non-terminal-delta)

(provide (all-defined-out))

(define-syntax-class parse-language
  #:description "language"
  (pattern (_ name:id
              #:entry-point entry-point:id
              (~seq #:terminals [terminal:parse-terminal
                                 ...+])
              non-terminal:parse-non-terminal
              ...+)
           #:do [(log-picopass-debug "parse-language:\n~a"
                                     (pretty-format this-syntax))]
           #:attr struct (language this-syntax
                                   (attribute name)
                                   (attribute entry-point)
                                   (attribute terminal.struct)
                                   (attribute non-terminal.struct)
                                   (make-syntax-introducer))))

(define-syntax-class parse-terminal
  #:description "terminal"
  (pattern [name:id class:id]
           #:attr struct
           (terminal this-syntax
                     #'name
                     #'class)))

(define-syntax-class parse-non-terminal
  #:description "non-terminal"
  (pattern ((~and name:id
                  (~fail #:when (datum=? #'terminals #'name)))
            (~optional (~seq #:literals [literal:id ...])
                       #:defaults ([[literal 1] null]))
            (~optional (~seq #:datum-literals [datum-literal:id ...])
                       #:defaults ([[datum-literal 1] null]))
            (~var production (parse-pattern no-literal?))
            ...+)
           #:attr struct
           (non-terminal this-syntax
                         #'name
                         (attribute literal)
                         (attribute datum-literal)
                         (attribute production.struct))))

(define-syntax-class parse-language-delta
  #:description "language extension"
  (pattern (_ name:id
              #:extends extends:id
              (~alt (~optional (~seq #:entry-point entry-point:id))
                    (~optional (~seq #:terminals- [terminal-:parse-terminal
                                                   ...])
                               #:defaults ([[terminal-.struct 1] null]))
                    (~optional (~seq #:terminals+ [terminal+:parse-terminal
                                                   ...])
                               #:defaults ([[terminal+.struct 1] null])))
              ...
              non-terminal:parse-non-terminal-delta
              ...)
           #:attr struct
           (language-delta this-syntax
                           (attribute name)
                           (attribute entry-point)
                           (make-delta #:remove (attribute terminal-.struct)
                                       #:add (attribute terminal+.struct))
                           (attribute non-terminal.struct))))

(define-syntax-class parse-non-terminal-delta
  #:description "non-terminal extension"

  (pattern ((~and name:id
                  (~fail #:when (datum=? #'terminals #'name)))

            (~optional (~seq #:literals- [literal-:id ...])
                       #:defaults ([[literal- 1] null]))
            (~optional (~seq #:literals+ [literal+:id ...])
                       #:defaults ([[literal+ 1] null]))

            (~optional (~seq #:datum-literals- [datum-literal-:id ...])
                       #:defaults ([[datum-literal- 1] null]))
            (~optional (~seq #:datum-literals+ [datum-literal+:id ...])
                       #:defaults ([[datum-literal+ 1] null]))

            (~optional (~and ((~datum -)
                              ~!
                              (~var production-
                                    (parse-pattern no-literal?))
                              ...+)
                             to-remove)
                       #:defaults ([[production-.struct 1] null]))
            (~optional (~and ((~datum +)
                              ~!
                              (~var production+
                                    (parse-pattern no-literal?))
                              ...+)
                             to-add)
                       #:defaults ([[production+.struct 1] null])))

           #:fail-unless (or (attribute to-remove)
                             (attribute to-add))
           "expected at least one removal or addition"

           #:attr struct
           (non-terminal-delta this-syntax
                               #'name
                               (make-delta #:remove (attribute literal-)
                                           #:add (attribute literal+))
                               (make-delta #:remove (attribute datum-literal-)
                                           #:add (attribute datum-literal+))
                               (make-delta #:remove (attribute production-.struct)
                                           #:add (attribute production+.struct)))))

