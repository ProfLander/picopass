#lang picopass/impl

; Terminal IR
;
; Represents a named language terminal with a corresponding syntax class

(require syntax/parse)

(provide (all-defined-out)
         (struct-out terminal))

(struct terminal [stx ident/name ident/class]
  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
     (display (cons (terminal-name self)
                    (terminal-class self))
              port))])

(define (terminal-name self)
  (-> terminal? symbol?)
  "return the symbolic name of SELF"

  (syntax-e (terminal-ident/name self)))

(define (terminal-class self)
  (-> terminal? symbol?)
  "return the symbolic class of SELF"

  (syntax-e (terminal-ident/class self)))

(define (terminal=? a b)
  (-> terminal? terminal? boolean?)
  "equality predicate over terminals"

  (and (equal? (terminal-name a)
               (terminal-name b))
       (equal? (terminal-class a)
               (terminal-class b))))

