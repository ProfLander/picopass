#lang picopass/impl

; Terminal IR
;
; Represents a named language terminal with a corresponding syntax class

(require syntax/strip-context)

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
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (terminal-ident/name self)))

(define (terminal-class self)
  (-> terminal? symbol?)
  #:trace #f
  "return the symbolic class of SELF"

  (syntax-e (terminal-ident/class self)))

(define (terminal-replace-context self lctx) 
  (-> terminal? syntax? terminal?)
  (let ([stx (terminal-stx self)]
        [ident/name (terminal-ident/name self)]
        [ident/class (terminal-ident/class self)])
    (terminal stx
              (replace-context lctx ident/name)
              (replace-context lctx ident/class))))

(define (terminal=? a b)
  (-> terminal? terminal? boolean?)
  #:trace #f
  "equality predicate over terminals"

  (and (equal? (terminal-name a)
               (terminal-name b))
       (equal? (terminal-class a)
               (terminal-class b))))

(define (terminal->syntax self)
  (-> terminal? syntax?)
  (with-syntax ([this-syntax #`(quote-syntax #,(terminal-stx self))]
                [name (terminal-name self)]
                [class (terminal-class self)])
    #'(terminal this-syntax
                #'name
                #'class)))

