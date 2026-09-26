#lang picopass/impl

; Non-terminal extension IR
;
; Holds additions / removals for literals, datum-literals, and productions

(require racket/list
         racket/function

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
                            description
                            delta-literals
                            delta-datum-literals
                            delta-productions]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (append
                       (list 'non-terminal-delta
                             (list 'name
                                   (non-terminal-delta-ident/name self))
                             (list 'description
                                   (non-terminal-delta-description self))
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

  (let* ([idents
          (remove-duplicates (append (map non-terminal-ident
                                          base)
                                     (map non-terminal-delta-ident/name
                                          delta))
                             datum=?)]

         [non-terminals
          (for/list ([ident (in-list idents)])

            (let* ([base
                    (findf (λ (cand)
                             (datum=?
                              ident
                              (non-terminal-ident cand)))
                           base)]

                   [ext
                    (findf (λ (cand)
                             (datum=?
                              ident
                              (non-terminal-delta-ident/name cand)))
                           delta)])

              (cond
                [(and base ext)
                 (extend-non-terminal base ext)]

                [(and (not base) ext)
                 (extend-non-terminal
                  (non-terminal (non-terminal-delta-stx ext)
                                ident
                                (non-terminal-delta-description ext)
                                null
                                null
                                null)
                  ext)]

                [(and base (not ext))
                 base])))])

    (filter (λ (non-terminal)
              (pair? (non-terminal-productions non-terminal)))
            non-terminals)))

(define (extend-non-terminal base delta)
  (-> non-terminal? non-terminal-delta? non-terminal?)
  "extend BASE with the removals and additions in DELTA"

  (let* ([description (or (non-terminal-delta-description delta) 
                          (non-terminal-description base))]
         [literals (non-terminal-literals base)]
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
                  description
                  literals
                  datum-literals
                  productions)))

