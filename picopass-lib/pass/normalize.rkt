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
         [input-ident (pass-input-ident self)]
         [output-ident (pass-output-ident self)]
         [processors (pass-processors self)]
         [processors (map (curryr normalize-processor input-ident)
                          processors)]
         [self-ref (pass-self-ref self)]
         [scope (pass-scope self)])
    (pass stx
          ident
          input-ident
          output-ident
          processors
          self-ref
          scope)))

(define (normalize-processor self pass-input-ident)
  (-> processor? syntax? processor?)

  "normalize SELF into compilable form,
   populating input and output references with the corresponding
   non-terminals from PASS-INPUT and PASS-OUTPUT each is a language"

  (let* ([stx (processor-stx self)]
         [ident (processor-ident self)]

         [input-ident (processor-input-ident self)]
         [output-ident (processor-output-ident self)]

         [clauses (for/list ([clause (in-list (processor-clauses self))])
                    (normalize-processor-clause clause 
                                                pass-input-ident 
                                                input-ident))])

    (processor stx
               ident
               input-ident
               output-ident
               clauses)))

(define (normalize-processor-clause self pass-input processor-input)
  (-> processor-clause?
      syntax?
      syntax?
      processor-clause?)
  "normalize SELF into compilable form,
   converting literals and datum-literals in its pattern
   based on those defined in PROCESSOR-INPUT, if it is a non-terminal"

  (let* ([stx (processor-clause-stx self)]

         [pattern (processor-clause-pattern self)]

         [pass-input-lang (syntax-local-language pass-input
                                                 (thunk #f))]

         [pattern
          (if pass-input-lang
              (let ([processor-input-nt
                     (language-non-terminal pass-input-lang
                                            processor-input)])
                (normalize-pattern
                  pattern
                  (non-terminal-literals processor-input-nt)
                  (non-terminal-datum-literals processor-input-nt)))
              pattern)]

         [body (processor-clause-body self)])

    (processor-clause stx
                      pattern
                      body)))

