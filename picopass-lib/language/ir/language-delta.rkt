#lang picopass/impl

; Language extension IR
;
; Holds a potentially-overridden entrypoint,
; and additions / removals for terminals and non-terminals

(require racket/list
         racket/syntax

         threading

         picopass/delta

         picopass/pattern/ir

         picopass/language/error
         picopass/language/validate
         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal
         picopass/language/ir/non-terminal-delta)

(provide (all-defined-out)
         (struct-out language-delta))

(struct language-delta [stx
                        ident
                        entry-point-ident
                        description
                        delta-terminals
                        non-terminals]

  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'language-delta
                           (list 'name (syntax->datum (language-delta-ident self)))
                           (list 'entry-point
                                 (let ([entry-point
                                        (language-delta-entry-point-ident self)])
                                   (and entry-point
                                        (syntax->datum entry-point))))
                           (cons 'description (language-delta-description self))
                           (cons 'delta-terminals (language-delta-delta-terminals self))
                           (cons 'non-terminals (language-delta-non-terminals self)))
                     port))])

(define (language-delta-name self)
  (-> language-delta? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (language-delta-ident self)))

(define (validate-language-delta delta)
  (-> language-delta? language-delta?)
  "ensure SELF is a valid language delta"

  (~> delta
      (validate-language-delta/unique-terminal-removals)
      (validate-language-delta/unique-non-terminal-removals)))

(define (validate-language-delta/unique-terminal-removals delta)
  (-> language-delta? language-delta?)
  "ensure no duplicates are present in the terminal removals of SELF"

  (let* ([delta-terminals (language-delta-delta-terminals delta)]
         [tms- (delta-to-remove delta-terminals)]
         [duplicate (check-duplicates tms- terminal=?)])
    (when duplicate
      [raise-syntax-error (language-delta-name delta)
       "duplicate terminal removal"
       (terminal-stx duplicate)]))

  delta)

(define (validate-language-delta/unique-non-terminal-removals delta)
  (-> language-delta? language-delta?)
  "ensure no duplicates are present in the non-terminal removals of SELF"

  (for ([non-terminal (in-list (language-delta-non-terminals delta))])
    (let* ([delta-productions (non-terminal-delta-delta-productions non-terminal)]
           [productions- (delta-to-remove delta-productions)]
           [duplicate (check-duplicates productions- pattern=?)])
      (when duplicate
        [raise-syntax-error (language-delta-name delta)
         "duplicate non-terminal removal"
         (pattern-stx duplicate)])))

  delta)

(define (extend-language base delta)
  (-> language? language-delta? language?)
  "extend BASE with the removals and additions in SELF"

  (let* ([stx (language-delta-stx delta)]
         [delta (validate-language-delta delta)]
         [ident (language-delta-ident delta)]
         [entry-point (or (language-delta-entry-point-ident delta)
                          (language-entry-point-ident base))]

         [description (or (language-delta-description delta)
                          (language-description base))]

         [delta-terminals (language-delta-delta-terminals delta)]
         [terminals (apply-delta delta-terminals (language-terminals base)
                                 #:equal? terminal=?
                                 #:on-missing
                                 (raise-missing-removed-terminal-error
                                   (language-delta-name delta)))]

         [terminals
          (for/list ([terminal terminals])
            (terminal-replace-context terminal ident))]

         [non-terminals (language-non-terminals base)]

         [non-terminals
          (for/list ([non-terminal (in-list non-terminals)])
            (non-terminal-replace-context non-terminal ident))]

         [non-terminals
          (extend-non-terminals non-terminals
                                (language-delta-non-terminals delta))])

    (language stx
              ident
              entry-point
              description
              terminals
              non-terminals
              (make-syntax-introducer))))

