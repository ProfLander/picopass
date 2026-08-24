#lang picopass/impl

; Non-terminal extension IR
;
; Holds additions / removals for literals, datum-literals, and productions

(require racket/function
         racket/pretty

         picopass/delta
         picopass/syntax

         picopass/pattern/ir
         picopass/pattern/normalize

         picopass/language/error
         picopass/language/ir/non-terminal)

(provide (all-defined-out)
         (struct-out non-terminal-delta))

(struct non-terminal-delta [stx
                            ident/name
                            delta-literals
                            delta-datum-literals
                            delta-productions]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (append
                       (list 'non-terminal-delta
                             (list 'name
                                   (non-terminal-delta-ident/name self))
                             (cons 'delta-literals
                                   (non-terminal-delta-delta-literals self))
                             (cons 'delta-datum-literals
                                   (non-terminal-delta-delta-datum-literals self))
                             (cons 'delta-productions
                                   (non-terminal-delta-delta-productions self))))
                     port))])

(define (extend-non-terminals base delta)
  (-> (listof non-terminal?)
      (listof non-terminal-delta?)
      (listof non-terminal?))
  "extend BASE with the removals and additions in DELTA"

  (filter (λ (non-terminal)
            (pair? (non-terminal-productions non-terminal)))

          (for/list ([ext (in-list delta)])
            (let* ([ident/name (non-terminal-delta-ident/name ext)]
                   [target (or (findf (compose (curry datum=? ident/name)
                                               non-terminal-ident)
                                      base)
                               (non-terminal ext
                                             ident/name
                                             null
                                             null
                                             null))])
              (extend-non-terminal target ext)))))

(define (extend-non-terminal base delta)
  (-> non-terminal? non-terminal-delta? non-terminal?)
  "extend BASE with the removals and additions in DELTA"

  (let* ([literals (non-terminal-literals base)]
         [delta-literals (non-terminal-delta-delta-literals delta)]
         [literals (delta-add delta-literals literals)]


         [datum-literals (non-terminal-datum-literals base)]
         [delta-datum-literals (non-terminal-delta-delta-datum-literals delta)]
         [datum-literals (delta-add delta-datum-literals datum-literals)]


         [productions (non-terminal-productions base)]
         [delta-productions (map-delta (curryr normalize-pattern literals datum-literals)
                                       (non-terminal-delta-delta-productions delta))]
         [productions (apply-delta delta-productions productions
                                   #:equal? pattern=?
                                   #:on-missing
                                   (raise-missing-removed-production-error
                                     (non-terminal-delta-stx delta)))]

         [literals (delta-remove delta-literals literals
                                 #:equal? datum=?
                                 #:on-missing
                                 (raise-missing-removed-literal-error
                                   (non-terminal-delta-stx delta)))]

         [datum-literals (delta-remove delta-datum-literals datum-literals
                                       #:equal? datum=?
                                       #:on-missing
                                       (raise-missing-removed-datum-literal-error
                                         (non-terminal-delta-stx delta)))])

    (non-terminal (non-terminal-stx base)
                  (non-terminal-ident base)
                  literals
                  datum-literals
                  productions)))

