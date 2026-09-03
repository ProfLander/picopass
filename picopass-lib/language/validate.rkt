#lang picopass/impl

; Language validation pipeline
;
; Asserts invariants over a language to produce useful syntax errors

(require racket/list
         racket/match

         threading

         picopass/syntax

         picopass/pattern/ir

         picopass/language/error
         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

; Language

(define (validate-language language)
  (-> language? (or/c language? none/c))
  "ensure LANGUAGE is a valid language"

  (~> language
      (validate-language/entry-point)
      (validate-language/unique-idents)
      (validate-language/valid-terminals)
      (validate-language/valid-non-terminals)))

(define (validate-language/entry-point language)
  (-> language? (or/c language? none/c))
  "ensure the entry-point of LANGUAGE points to a valid non-terminal"

  (let* ([entry-point (language-entry-point-ident language)]
         [non-terminal-idents (map non-terminal-ident
                                   (language-non-terminals language))])
    (unless (member entry-point non-terminal-idents datum=?)
      (raise-language-entry-point-error language entry-point)))

  language)

(define (validate-language/unique-idents language)
  (-> language? (or/c language? none/c))
  "ensure terminal and non-terminal identifiers in LANGUAGE are unique"

  (let* ([terminal-idents (map terminal-ident/name
                               (language-terminals language))]
         [non-terminal-idents (map non-terminal-ident
                                   (language-non-terminals language))]
         [symbol-namespace (append terminal-idents
                                   non-terminal-idents)]
         [duplicate (check-duplicates symbol-namespace datum=?)])

    (when duplicate
      (raise-language-duplicate-symbol-error language duplicate)))

  language)

(define (validate-language/valid-terminals language)
  (-> language? (or/c language? none/c))
  "ensure all the terminals in LANGUAGE are valid"

  (for ([terminal (in-list (language-terminals language))])
    (validate-terminal terminal))

  language)

(define (validate-language/valid-non-terminals language)
  (-> language? (or/c language? none/c))
  "ensure all non-terminals in LANGUAGE are valid"

  (for ([non-terminal (in-list (language-non-terminals language))])
    (validate-non-terminal language non-terminal))

  language)

; Terminal

(define (validate-terminal terminal)
  (-> terminal? (or/c terminal? none/c))
  "ensure TERMINAL is a valid terminal"

  (~> terminal
      (validate-terminal/valid-class)))

(define (validate-terminal/valid-class terminal)
  (-> terminal? (or/c terminal? none/c))
  "ensure the class in TERMINAL points to a valid binding"

  (let* ([class (terminal-ident/class terminal)]
         [binding (identifier-binding class)])
    (unless binding
      [raise-terminal-unbound-class-error terminal
       (terminal-ident/class terminal)]))

  terminal)

; Non-Terminal

(define (validate-non-terminal language non-terminal)
  (-> language? non-terminal? (or/c non-terminal? none/c))
  "ensure NON-TERMINAL is a valid non-terminal in the context of LANGUAGE"

  (~> non-terminal
      (validate-non-terminal/unique-literals _)
      (validate-non-terminal/unique-productions _)
      (validate-non-terminal/valid-productions language _)))

(define (validate-non-terminal/unique-literals non-terminal)
  (-> non-terminal? (or/c non-terminal? none/c))
  "ensure literals and datum-literals in NON-TERMINAL are unique"

  (let* ([literals (non-terminal-literals non-terminal)]
         [datum-literals (non-terminal-datum-literals non-terminal)]
         [literal-namespace (append literals datum-literals)]
         [duplicate (check-duplicates literal-namespace datum=?)])
    (when duplicate
      (raise-non-terminal-duplicate-literal-error non-terminal duplicate)))

  non-terminal)

(define (validate-non-terminal/unique-productions non-terminal)
  (-> non-terminal? (or/c non-terminal? none/c))
  "ensure productions in NON-TERMINAL are unique"

  (let* ([productions (non-terminal-productions non-terminal)]
         [duplicate (check-duplicates productions pattern=?)])
    (when duplicate
      (raise-non-terminal-duplicate-production-error non-terminal duplicate)))

  non-terminal)

(define (validate-non-terminal/valid-productions language non-terminal)
  (-> language? non-terminal? (or/c non-terminal? none/c))
  "ensure the productions of NON-TERMINAL are valid
   in the context of LANGUAGE"

  (for ([production (in-list (non-terminal-productions non-terminal))])
    (validate-production language non-terminal production))

  non-terminal)

; Production

(define (validate-production language non-terminal production)
  (-> language? (or/c non-terminal? void?) pattern? pattern?)
  "ensure PRODUCTION is a valid production
   in the context of NON-TERMINAL and LANGUAGE"

  (~> production
      (validate-production/valid-idents language non-terminal _)))

(define (validate-production/valid-idents language non-terminal production)
  (-> language?
      non-terminal?
      pattern?
      (or/c pattern? none/c))
  "ensure identifiers in PRODUCTION are valid
   in the context of NON-TERMINAL and LANGUAGE"

  (define (rec language non-terminal production pattern)
    (-> language?
        non-terminal?
        pattern?
        pattern?
        pattern?)
    "ensure SELF references a valid literal, datum-literal, terminal, or non-terminal"
    (cond
      [(p-ident? pattern)
       (let ([ident (p-ident-ident pattern)]
             [literals (non-terminal-literals non-terminal)]
             [datum-literals (non-terminal-datum-literals non-terminal)]
             [terminal-idents (map terminal-ident/name
                                   (language-terminals language))]
             [non-terminal-idents (map non-terminal-ident
                                       (language-non-terminals language))])
         (unless (or (member ident literals datum=?)
                     (member ident datum-literals datum=?)
                     (member ident terminal-idents datum=?)
                     (member ident non-terminal-idents datum=?))
           (raise-production-invalid-ident-error production language ident)))
       pattern]
      [(p-list? pattern)
       (p-list (p-list-stx pattern)
               (for/list ([pattern* (in-list (p-list-list pattern))])
                 (rec language non-terminal production pattern*)))]
      [(or (p-literal? pattern)
           (p-keyword? pattern)
           (p-repeat? pattern))
       pattern]
      [else (error "not a production:" pattern)]))

  (rec language non-terminal production production))

