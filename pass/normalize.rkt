#lang picopass/impl

; Pass IR normalization pipeline
; 
; Performs transformations on the parsed representation of a pass
; to prepare it for compilation

(require racket/function

         picopass/syntax

         picopass/pattern/normalize

         picopass/language/ir/language
         picopass/language/ir/non-terminal

         picopass/pass/ir/pass
         picopass/pass/ir/processor
         picopass/pass/ir/processor-clause)

(provide (all-defined-out))

(define (normalize-pass self)
  (-> pass? pass?)
  "normalize SELF into compilable form"

  (let* ([stx (pass-stx self)]
         [ident (pass-ident self)]
         [input (pass-input self)]
         [input (syntax-local-value input (thunk input))]
         [output (pass-output self)]
         [output (syntax-local-value output (thunk output))]
         [processors (pass-processors self)]
         [processors (map (curryr normalize-processor input output)
                          processors)]
         [self-ref (pass-self-ref self)]
         [scope (pass-scope self)])
    (pass stx
          ident
          input
          output
          processors
          self-ref
          scope)))

(define (normalize-processor self pass-input pass-output)
  (-> processor?
      (or/c language? syntax?)
      (or/c language? syntax?)
      processor?)
  "normalize SELF into compilable form,
   populating input and output references with the corresponding
   non-terminals from PASS-INPUT and PASS-OUTPUT each is a language"

  (let* ([stx (processor-stx self)]
         [ident (processor-ident self)]

         [input-ident (processor-input-ident self)]
         [input (if (language? pass-input)
                    (findf (λ (non-terminal)
                             (datum=? input-ident
                                      (non-terminal-ident non-terminal)))
                           (language-non-terminals pass-input))
                    input-ident)]

         [output-ident (processor-output-ident self)]
         [output (if (language? pass-output)
                     (findf (λ (non-terminal)
                              (datum=? output-ident
                                       (non-terminal-ident non-terminal)))
                            (language-non-terminals pass-output))
                     output-ident)]

         [clauses (map (curryr normalize-processor-clause input)
                       (processor-clauses self))])

    (processor stx
               ident
               input-ident
               input
               output-ident
               output
               clauses)))

(define (normalize-processor-clause self processor-input)
  (-> processor-clause?
      (or/c non-terminal? syntax? #f)
      processor-clause?)
  "normalize SELF into compilable form,
   converting literals and datum-literals in its pattern
   based on those defined in PROCESSOR-INPUT, if it is a non-terminal"

  (let* ([stx (processor-clause-stx self)]

         [pattern (processor-clause-pattern self)]
         [pattern
          (if (non-terminal? processor-input)
              (normalize-pattern pattern
                                 (non-terminal-literals processor-input)
                                 (non-terminal-datum-literals processor-input))
              pattern)]

         [body (processor-clause-body self)])

    (processor-clause stx
                      pattern
                      body)))

