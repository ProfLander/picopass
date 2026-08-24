#lang picopass/impl

; Language validation pipeline
;
; Asserts invariants over a language to produce useful syntax errors

(require racket/list
         racket/function

         threading

         picopass/syntax

         picopass/pattern/ir

         picopass/language/error
         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

; Language

(define (validate-language self)
  (-> language? (or/c language? none/c))
  "ensure SELF is a valid language"

  (~> self
      (validate-language/entry-point)
      (validate-language/unique-idents)
      (validate-language/valid-terminals)
      (validate-language/valid-non-terminals)))

(define (validate-language/entry-point self)
  (-> language? (or/c language? none/c))
  "ensure the entry-point of SELF points to a valid non-terminal"

  (let* ([entry-point (language-entry-point-ident self)]
         [non-terminal-idents (map non-terminal-ident
                                   (language-non-terminals self))])
    (unless (member entry-point non-terminal-idents datum=?)
      (raise-language-entry-point-error self entry-point)))

  self)

(define (validate-language/unique-idents self)
  (-> language? (or/c language? none/c))
  "ensure terminal and non-terminal identifiers in SELF are unique"

  (let* ([terminal-idents (map terminal-ident/name
                               (language-terminals self))]
         [non-terminal-idents (map non-terminal-ident
                                   (language-non-terminals self))]
         [symbol-namespace (append terminal-idents
                                   non-terminal-idents)]
         [duplicate (check-duplicates symbol-namespace datum=?)])

    (when duplicate
      (raise-language-duplicate-symbol-error self duplicate)))

  self)

(define (validate-language/valid-terminals self)
  (-> language? (or/c language? none/c))
  "ensure all the terminals in SELF are valid"

  (map validate-terminal (language-terminals self))

  self)

; Terminal

(define (validate-terminal self)
  (-> terminal? (or/c terminal? none/c))
  "ensure SELF is a valid terminal"

  (~> self
      (validate-terminal/valid-class)))

(define (validate-terminal/valid-class self)
  (-> terminal? (or/c terminal? none/c))
  "ensure the class in SELF points to a valid binding"

  (let* ([class (terminal-ident/class self)]
         [binding (identifier-binding class)])
    (unless binding
      (raise-terminal-unbound-class-error (terminal-ident/class self))))

  self)

(define (validate-language/valid-non-terminals self)
  (-> language? (or/c language? none/c))
  "ensure all non-terminals in SELF are valid"

  (map (curryr validate-non-terminal self)
       (language-non-terminals self))

  self)

; Non-Terminal

(define (validate-non-terminal self lang)
  (-> non-terminal? language? (or/c non-terminal? none/c))
  "ensure SELF is a valid non-terminal"

  (~> self
      (validate-non-terminal/unique-literals _)
      (validate-non-terminal/unique-productions _)
      (validate-non-terminal/valid-productions _ lang)))

(define (validate-non-terminal/unique-literals self)
  (-> non-terminal? (or/c non-terminal? none/c))
  "ensure literals and datum-literals in SELF are unique"

  (let* ([literals (non-terminal-literals self)]
         [datum-literals (non-terminal-datum-literals self)]
         [literal-namespace (append literals datum-literals)]
         [duplicate (check-duplicates literal-namespace datum=?)])
    (when duplicate
      (raise-non-terminal-duplicate-literal-error self duplicate)))

  self)

(define (validate-non-terminal/unique-productions self)
  (-> non-terminal? (or/c non-terminal? none/c))
  "ensure productions in SELF are unique"

  (let* ([productions (non-terminal-productions self)]
         [duplicate (check-duplicates productions pattern=?)])
    (when duplicate
      (raise-non-terminal-duplicate-production-error self duplicate)))

  self)

(define (validate-non-terminal/valid-productions self lang)
  (-> non-terminal? language? (or/c non-terminal? none/c))
  "ensure productions in SELF are valid"

  (map (curryr validate-production lang self)
       (non-terminal-productions self))

  self)

; Production

(define (validate-production self lang non-terminal)
  (-> pattern? language? (or/c non-terminal? void?) pattern?)
  "ensure SELF is a valid production"

  (~> self
      (validate-production/valid-idents _ lang non-terminal)))

(define (validate-production/valid-idents self lang non-terminal)
  (-> pattern?
      language?
      (or/c non-terminal? void?)
      pattern?)
  "ensure identifiers in SELF are valid"

  (define (rec self prod lang non-terminal)
    (-> pattern?
        pattern?
        language?
        (or/c non-terminal? void?)
        pattern?)
    "ensure SELF references a valid literal, datum-literal, terminal, or non-terminal"
    (cond
      [(p-ident? self)
       (let ([ident (p-ident-ident self)]
             [literals (non-terminal-literals non-terminal)]
             [datum-literals (non-terminal-datum-literals non-terminal)]
             [terminal-idents (map terminal-ident/name
                                   (language-terminals lang))]
             [non-terminal-idents (map non-terminal-ident
                                       (language-non-terminals lang))])
         (unless (or (member ident literals datum=?)
                     (member ident datum-literals datum=?)
                     (member ident terminal-idents datum=?)
                     (member ident non-terminal-idents datum=?))
           (raise-production-invalid-ident-error prod lang ident)))
       self]
      [(p-list? self)
       (p-list (p-list-stx self)
               (map (curryr rec prod lang non-terminal)
                    (p-list-list self)))]
      [(or (p-literal? self)
           (p-repeat? self))
       self]
      [else (error "not a production:" self)]))

  (rec self self lang non-terminal))

