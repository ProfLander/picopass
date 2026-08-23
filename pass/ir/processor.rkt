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
                   input
                   output-ident
                   output
                   clauses]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'processor
                           (list 'name (syntax->datum (processor-ident self)))
                           (list 'input (let ([input (processor-input self)])
                                          (if (syntax? input)
                                              (syntax->datum input)
                                              input)))
                           (list 'output (let ([output (processor-output self)])
                                           (if (syntax? output)
                                               (syntax->datum output)
                                               output)))
                           (cons 'clauses (processor-clauses self)))
                     port))])

(define (processor-name self)
  (-> processor? symbol?)
  "return the symbolic name of SELF"

  (syntax-e (processor-ident self)))

