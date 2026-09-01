#lang picopass/impl

; Non-terminal IR
;
; Represents a named language non-terminal with literals, datum-literals,
; and a set of production forms

(require racket/function

         syntax/parse
         syntax/strip-context

         picopass/syntax

         picopass/pattern/ir)

(provide (all-defined-out))

(define (production-literal? stx)
  (-> syntax? boolean?)
  (or (datum=? stx #'~maybe)
      (datum=? stx #'~cut)))

(struct non-terminal [stx

                      ident
                      description

                      literals
                      datum-literals

                      productions]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
     (display (append
                (list 'non-terminal
                      (list 'name
                            (non-terminal-name self))
                      (list 'description
                            (non-terminal-description self))
                      (cons 'literals
                            (non-terminal-literal-names self))
                      (cons 'datum-literals
                            (non-terminal-datum-literal-names self))
                      (cons 'productions
                            (non-terminal-productions self))))
              port))])

(define (non-terminal-name self)
  (-> non-terminal? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (non-terminal-ident self)))

(define (non-terminal-literal-names self)
  (-> non-terminal? (listof symbol?))
  #:trace #f
  "return the symbolic names of the literals in SELF"

  (map syntax-e (non-terminal-literals self)))

(define (non-terminal-datum-literal-names self)
  (-> non-terminal? (listof symbol?))
  #:trace #f
  "return the symbolic names of the datum-literals in SELF"

  (map syntax-e (non-terminal-datum-literals self)))

(define (non-terminal-replace-context self lctx)
  (-> non-terminal? syntax? non-terminal?)

  (define (replace stx)
    (-> syntax? syntax?)
    (replace-context lctx stx))

  (let ([stx (non-terminal-stx self)]
        [ident (replace (non-terminal-ident self))]
        [description (non-terminal-description self)]
        [literals (map replace (non-terminal-literals self))]
        [datum-literals (map replace (non-terminal-datum-literals self))]
        [productions (map (curryr pattern-replace-context lctx)
                          (non-terminal-productions self))])

    (non-terminal stx
                  ident
                  description
                  literals
                  datum-literals
                  productions)))

(define (non-terminal=? a b)
  (-> non-terminal? non-terminal? boolean?)
  #:trace #f
  "equality over non-terminals"

  (and (datum=? (non-terminal-ident a)
                (non-terminal-ident b))

       (for/and ([a (in-list (non-terminal-literals a))]
                 [b (in-list (non-terminal-literals b))])
         (datum=? a b))

       (for/and ([a (in-list (non-terminal-datum-literals a))]
                 [b (in-list (non-terminal-datum-literals b))])
         (datum=? a b))

       (for/and ([a (in-list (non-terminal-productions a))]
                 [b (in-list (non-terminal-productions b))])
         (pattern=? a b))))

