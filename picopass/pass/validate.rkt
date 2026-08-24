#lang picopass/impl

; Pass validation pipeline
;
; Asserts invariants over a pass to produce useful syntax errors

(require racket/list
         racket/function

         threading

         picopass/syntax
         picopass/pattern/ir

         picopass/language/ir/language
         picopass/language/ir/non-terminal

         picopass/pass/error
         picopass/pass/ir/pass
         picopass/pass/ir/processor
         picopass/pass/ir/processor-clause)

(provide (all-defined-out))

(define (validate-pass self)
  (-> pass? (or/c pass? none/c))
  "ensure SELF is a valid pass"

  (~> self
      (validate-pass/bound-input)
      (validate-pass/bound-output)
      (validate-pass/unique-processors)
      (validate-pass/valid-processors)))

(define (validate-pass/bound-input self)
  (-> pass? (or/c pass? none/c))
  "ensure the input of SELF is a valid binding"

  (let ([input (pass-input self)])
    (unless (language? input)
      (unless (identifier-binding input)
        [raise-pass-error self
         "unbound pass input"
         input])))

  self)

(define (validate-pass/bound-output self)
  (-> pass? (or/c pass? none/c))
  "ensure the output of SELF is a valid binding"

  (let ([input (pass-input self)])
    (unless (language? input)
      (unless (identifier-binding input)
        [raise-pass-error self
         "unbound pass input"
         input]))

    self))

(define (validate-pass/unique-processors self)
  (-> pass? (or/c pass? none/c))
  "ensure no duplicate processors are present in SELF"
  (let* ([processors (pass-processors self)]
         [processor-idents (map processor-ident processors)]
         [duplicate (check-duplicates processor-idents datum=?)])
    (when duplicate
      [raise-pass-error self
       "duplicate processor name"
       duplicate])
    self))

(define (validate-pass/valid-processors self)
  (-> pass? (or/c pass? none/c))
  "ensure the processors in SELF are all valid"
  (let ([processors (pass-processors self)])
    (map (curryr validate-processor self) processors)
    self))

(define (validate-processor self pass)
  (-> processor? pass? (or/c processor? none/c))
  "ensure SELF is a valid processor"
  (~> self
      (validate-processor/bound-input _ pass)
      (validate-processor/valid-input _ pass)
      (validate-processor/bound-output _ pass)
      (validate-processor/valid-output _ pass)
      (validate-processor/unique-clauses)
      (validate-processor/valid-clauses _ pass)))

(define (validate-processor/bound-input self pass)
  (-> processor? pass? (or/c processor? none/c))
  "if pass input is a predicate, ensure the input of SELF is a valid binding"

  (let ([pass-input (pass-input pass)])
    (unless (language? pass-input)
      (let ([input-ident (processor-input-ident self)])
        (unless (identifier-binding input-ident)
          [raise-processor-error self
           "unbound processor input"
           (processor-input-ident self)])))

    self))

(define (validate-processor/valid-input self pass)
  (-> processor? pass? (or/c processor? none/c))
  "if pass input is a language, ensure the input of SELF is a valid reference"

  (let ([pass-input (pass-input pass)])
    (when (language? pass-input)
      (let ([input (processor-input self)])
        (unless input
          [raise-processor-error self
           (format "invalid input non-terminal for language ~a"
                   (language-name pass-input))
           (processor-input-ident self)])))

    self))

(define (validate-processor/bound-output self pass)
  (-> processor? pass? (or/c processor? none/c))
  "if pass input is a predicate, ensure the output of SELF is a valid binding"

  (let ([pass-output (pass-output pass)])
    (unless (language? pass-output)
      (let ([output-ident (processor-output-ident self)])
        (unless (identifier-binding output-ident)
          [raise-processor-error self
           "unbound processor output"
           (processor-output-ident self)])))

    self))

(define (validate-processor/valid-output self pass)
  (-> processor? pass? (or/c processor? none/c))
  "if pass input is a language, ensure the output of SELF is a valid reference"

  (let ([pass-output (pass-output pass)])
    (when (language? pass-output)
      (let ([output (processor-output self)])
        (unless output
          [raise-processor-error self
           (format "invalid output non-terminal for language ~a"
                   (language-name pass-output))
           (processor-output-ident self)])))

    self))

(define (validate-processor/unique-clauses self)
  (-> processor? (or/c processor? none/c))
  "ensure no duplicate clauses are present in SELF"

  (let* ([clauses (processor-clauses self)]
         [patterns (map processor-clause-pattern clauses)]
         [duplicate (check-duplicates patterns pattern=?)])

    (when duplicate
      [raise-processor-error self
       "duplicate processor clause"
       (pattern-stx duplicate)])

    self))

(define (validate-processor/valid-clauses self pass)
  (-> processor? pass? (or/c processor? none/c))
  "ensure the clauses in SELF are all valid"
  (let ([clauses (processor-clauses self)])
    (map (curryr validate-processor-clause pass self) clauses)
    self))

(define (validate-processor-clause self pass processor)
  (-> processor-clause? pass? processor? processor-clause?)
  "ensure SELF is a valid processor clause"

  (~> self
      (validate-processor-clause/valid-pattern _ pass processor)))

(define (validate-processor-clause/valid-pattern self pass processor)
  (-> processor-clause? pass? processor? processor-clause?)
  "ensure the pattern of SELF corresponds to an input language non-terminal"

  (let* ([input (processor-input processor)])

    (when (non-terminal? input)
      (let* ([lang (pass-input pass)]
             [productions (non-terminal-productions input)]
             [pattern (processor-clause-pattern self)]
             [pattern (processor-clause-pattern->non-terminal-pattern pattern)])

        (unless (member pattern productions pattern=?)
          [raise-processor-clause-invalid-pattern-error self
           (language-name lang)
           (pattern-stx pattern)])))

    self))

