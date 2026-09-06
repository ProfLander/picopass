#lang picopass/impl

; Processor IR
;
; Represents a transformation from a non-terminal of the parent pass'
; input language to a non-terminal of the parent pass' output language

(provide (all-defined-out)
         (struct-out processor))

(struct processor [stx
                   ident
                   input-ident
                   output-ident
                   clauses]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'processor
                           (list 'name (syntax->datum (processor-ident self)))
                           (list 'input (syntax->datum (processor-input-ident self)))
                           (list 'output (syntax->datum (processor-output-ident self)))
                           (cons 'clauses (processor-clauses self)))
                     port))])

(define (processor-name self)
  (-> processor? symbol?)
  "return the symbolic name of SELF"

  (syntax-e (processor-ident self)))

