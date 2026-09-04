#lang picopass/impl

; Pattern IR normalization pipeline
; 
; Performs transformations on the parsed representation of a pattern
; to prepare it for compilation

(require racket/function
         racket/match

         picopass/syntax

         picopass/pattern/ir)

(provide (all-defined-out))

(define (normalize-pattern self literals datum-literals)
  (-> pattern?
      (listof syntax?)
      (listof syntax?)
      pattern?)
  "replace identifiers with literals in SELF if they appear in
   LITERALS or DATUM-LITERALS"

  (match self
    [(p-ident ident)
     (if (or (member ident literals datum=?)
             (member ident datum-literals datum=?))
         (p-literal ident)
         self)]
    [(p-list stx lst tail)
     (p-list stx 
             (map (curryr normalize-pattern literals datum-literals)
                  lst)
             (normalize-pattern tail literals datum-literals))]
    [_ self]))

