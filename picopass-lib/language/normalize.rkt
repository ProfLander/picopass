#lang picopass/impl

; Language IR normalization pipeline
; 
; Performs transformations on the parsed representation of a language
; to prepare it for compilation

(require racket/function

         picopass/pattern/normalize

         picopass/language/ir/language
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

(define (normalize-language self)
  (-> language? language?)
  "normalize SELF into compilable form"

  (let* ([stx (language-stx self)]
         [scope (language-scope self)]
         [ident (language-ident self)]
         [entry-point-ident (language-entry-point-ident self)]
         [description (language-description self)]
         [terminals (language-terminals self)]
         [non-terminals (map normalize-non-terminal
                             (language-non-terminals self))]
         [scope-key (language-scope-key self)]
         [scope (language-scope self)])
    (language stx
              ident
              entry-point-ident
              description
              terminals
              non-terminals
              scope-key
              scope)))

(define (normalize-non-terminal self)
  (-> non-terminal? non-terminal?)
  "normalize SELF into compilable form"

  (let* ([stx (non-terminal-stx self)]
         [ident (non-terminal-ident self)]
         [description (non-terminal-description self)]
         [literals (non-terminal-literals self)]
         [datum-literals (non-terminal-datum-literals self)]
         [productions (map (curryr normalize-pattern
                                   literals datum-literals)
                           (non-terminal-productions self))])

    (non-terminal stx
                  ident
                  description
                  literals
                  datum-literals
                  productions)))
