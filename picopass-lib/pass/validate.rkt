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

(define (validate-pass pass)
  (-> pass? (or/c pass? none/c))
  "ensure SELF is a valid pass"

  (~> pass
      (validate-pass/bound-input)
      (validate-pass/bound-output)
      (validate-pass/unique-processors)
      (validate-pass/input-coverage)
      (validate-pass/output-coverage)
      (validate-pass/valid-processors)))

(define (validate-pass/bound-input pass)
  (-> pass? (or/c pass? none/c))
  "ensure the input of SELF is a valid binding"

  (let ([input (pass-input pass)])
    (unless (language? input)
      (unless (identifier-binding input)
        [raise-pass-error pass
         "unbound pass input"
         input])))

  pass)

(define (validate-pass/bound-output pass)
  (-> pass? (or/c pass? none/c))
  "ensure the output of SELF is a valid binding"

  (let ([input (pass-input pass)])
    (unless (language? input)
      (unless (identifier-binding input)
        [raise-pass-error pass
         "unbound pass input"
         input]))

    pass))

(define (validate-pass/unique-processors pass)
  (-> pass? (or/c pass? none/c))
  "ensure no duplicate processors are present in SELF"
  (let* ([processors (pass-processors pass)]
         [processor-idents (map processor-ident processors)]
         [duplicate (check-duplicates processor-idents datum=?)])
    (when duplicate
      [raise-pass-error pass
       "duplicate processor name"
       duplicate])
    pass))

(define (validate-pass/input-coverage pass)
  (-> pass? (or/c pass? none/c))
  "when the input of SELF is a language and the output is not,
   (i.e. automatic clause generation is not taking place,)
   ensure each of its non-terminals is the input of at least one processor"
  (let ([input (pass-input pass)] 
        [output (pass-output pass)])

    (when (language? input)
      (unless (language? output)
        (let ([non-terminals (language-non-terminals input)])
          (for ([non-terminal (in-list non-terminals)])
            (unless (for/or ([processor (in-list (pass-processors pass))])
                      (non-terminal=? non-terminal (processor-input processor)))
              [raise-pass-error pass
               (format "~a non-terminal ~a is not the input of any processor"
                       (language-name input)
                       (non-terminal-name non-terminal))])))))

    pass))

(define (validate-pass/output-coverage pass)
  (-> pass? (or/c pass? none/c))
  "when the output of SELF is a language and the input is not,
   (i.e. automatic clause generation is not taking place,)
   ensure each of its non-terminals is the output of at least one processor"

  (let ([input (pass-input pass)] 
        [output (pass-output pass)])
    (when (language? output)
      (unless (language? input)
        (let ([non-terminals (language-non-terminals output)])
          (for ([non-terminal (in-list non-terminals)])
            (unless (for/or ([processor (in-list (pass-processors pass))])
                      (non-terminal=? non-terminal (processor-output processor)))
              [raise-pass-error pass
               (format "~a non-terminal ~a is not output by any processor"
                       (language-name output)
                       (non-terminal-name non-terminal))])))))

    pass))

(define (validate-pass/valid-processors pass)
  (-> pass? (or/c pass? none/c))
  "ensure the processors in SELF are all valid"
  (let ([processors (pass-processors pass)])
    (for ([processor (in-list processors)])
      (validate-processor pass processor))
    pass))

(define (validate-processor pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "ensure SELF is a valid processor"
  (~> processor
      (validate-processor/bound-input pass _)
      (validate-processor/valid-input pass _)
      (validate-processor/bound-output pass _)
      (validate-processor/valid-output pass _)
      (validate-processor/unique-clauses)
      (validate-processor/valid-clauses pass _)))

(define (validate-processor/bound-input pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "if pass input is a predicate, ensure the input of SELF is a valid binding"

  (let ([pass-input (pass-input pass)])
    (unless (language? pass-input)
      (let ([input-ident (processor-input-ident processor)])
        (unless (identifier-binding input-ident)
          [raise-processor-error processor
           "unbound processor input"
           (processor-input-ident processor)])))

    processor))

(define (validate-processor/valid-input pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "if pass input is a language, ensure the input of SELF is a valid reference"

  (let ([pass-input (pass-input pass)])
    (when (language? pass-input)
      (let ([input (processor-input processor)])
        (unless input
          [raise-processor-error processor
           (format "invalid input non-terminal for language ~a"
                   (language-name pass-input))
           (processor-input-ident processor)])))

    processor))

(define (validate-processor/bound-output pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "if pass input is a predicate, ensure the output of SELF is a valid binding"

  (let ([pass-output (pass-output pass)])
    (unless (language? pass-output)
      (let ([output-ident (processor-output-ident processor)])
        (unless (identifier-binding output-ident)
          [raise-processor-error processor
           "unbound processor output"
           (processor-output-ident processor)])))

    processor))

(define (validate-processor/valid-output pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "if pass input is a language, ensure the output of SELF is a valid reference"

  (let ([pass-output (pass-output pass)])
    (when (language? pass-output)
      (let ([output (processor-output processor)])
        (unless output
          [raise-processor-error processor
           (format "invalid output non-terminal for language ~a"
                   (language-name pass-output))
           (processor-output-ident processor)])))

    processor))

(define (validate-processor/unique-clauses processor)
  (-> processor? (or/c processor? none/c))
  "ensure no duplicate clauses are present in SELF"

  (let* ([clauses (processor-clauses processor)]
         [patterns (map processor-clause-pattern clauses)]
         [duplicate (check-duplicates patterns pattern=?)])

    (when duplicate
      [raise-processor-error processor
       "duplicate processor clause"
       (pattern-stx duplicate)])

    processor))

(define (validate-processor/valid-clauses pass processor)
  (-> pass? processor? (or/c processor? none/c))
  "ensure the clauses in SELF are all valid"
  (let ([clauses (processor-clauses processor)])
    (for ([clause (in-list clauses)])
      (validate-processor-clause pass processor clause))
    processor))

(define (validate-processor-clause pass processor clause)
  (-> pass? processor? processor-clause? processor-clause?)
  "ensure SELF is a valid processor clause"

  (~> clause
      (validate-processor-clause/valid-pattern pass processor _)))

(define (validate-processor-clause/valid-pattern pass processor clause)
  (-> pass? processor? processor-clause? processor-clause?)
  "ensure the pattern of SELF corresponds to an input language non-terminal"

  (let* ([input (processor-input processor)])

    (when (non-terminal? input)
      (let* ([lang (pass-input pass)]
             [productions (non-terminal-productions input)]
             [pattern (processor-clause-pattern clause)]
             [pattern (processor-clause-pattern->non-terminal-pattern pattern)])

        (unless (member pattern productions pattern=?)
          [raise-processor-clause-invalid-pattern-error clause
           (language-name lang)
           (pattern-stx pattern)])))

    clause))

