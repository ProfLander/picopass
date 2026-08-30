#lang picopass/impl

; Pass compilation pipeline

(require racket/list
         racket/function
         racket/match
         racket/format
         racket/syntax

         syntax/parse

         picopass/syntax

         picopass/pattern/ir

         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal

         picopass/pass/ir/pass
         picopass/pass/ir/processor
         picopass/pass/ir/processor-clause)

(provide (all-defined-out))

; Pass

(define (compile-pass pass)
  (-> pass? syntax?)
  "compile PASS to syntax"

  [with-pass-syntax pass ([begin 'begin])

   (with-syntax ([pass-dispatch
                  (compile-pass-dispatch pass)]

                 [(pass-input-handler-definition ...)
                  (compile-pass-input-handlers pass)]

                 [pass-entry-point
                  (compile-pass-entry-point pass)])

     #`(begin

         pass-dispatch

         pass-input-handler-definition
         ...

         pass-entry-point))])

(define (compile-pass-dispatch pass)
  (-> pass? syntax?)
  "compile dispatch machinery for PASS to syntax"

  (let* ([input (pass-input pass)])

    [with-pass-syntax pass ([define 'define])
     (with-syntax*
       ([pass-ref (pass-self-ref pass)]
        [dispatch
         (if (language? input)
             (compile-pass-dispatch/syntax-parse pass)
             (compile-pass-dispatch/match pass))])

       #'(define pass-ref
           dispatch))]))

(define (compile-pass-dispatch/syntax-parse pass)
  (-> pass? syntax?)
  "compile syntax-parse dispatch for PASS to syntax"

  (let* ([input (pass-input pass)]
         [non-terminals (language-non-terminals input)]
         [processors (pass-processors pass)]
         [input-idents
          (remove-duplicates
            (for/list ([processor (in-list (pass-processors pass))])
              (non-terminal-ident (processor-input processor)))
            datum=?)])

    (let-values ([(non-terminal-idents pass-idents)
                  (for/lists (_non-terminal-idents _pass-idents)
                             ([ident (in-list input-idents)])
                    (values (language-introduce input ident)
                            (pass-introduce pass ident)))])

      [with-pass-syntax pass ([syntax-parser 'syntax-parser]
                              [~var '~var]
                              [attribute 'attribute]
                              [%app '#%app])

       (with-syntax ([(non-terminal-ident ...) non-terminal-idents]
                     [(pass-ident ...) pass-idents])

         #`(syntax-parser
             [(~var prod non-terminal-ident)
              (%app pass-ident (attribute prod))]
             ...))])))

(define (compile-pass-dispatch/match pass)
  (-> pass? syntax?)
  "compile match dispatch for PASS to syntax"

  (let* ([processors (pass-processors pass)]
         [processor-inputs (map processor-input processors)]
         [processor-idents
          (for/list ([processor-input (in-list processor-inputs)])
            (pass-introduce pass processor-input))])

    [with-pass-syntax pass ([λ 'λ]
                            [cond 'cond]
                            [%app '#%app]
                            [raise-pass-dispatch-error
                             'raise-pass-dispatch-error]
                            [else 'else]
                            [quote 'quote])

     (with-syntax ([pass-name (pass-name pass)]
                   [(processor-pred ...) processor-inputs]
                   [(processor-ident ...) processor-idents])

       #`(λ (in)
           (cond
             [(%app processor-pred in)
              (%app processor-ident in)]
             ...
             [else
              (%app raise-pass-dispatch-error
                    (quote pass-name)
                    in)])))]))

(define (compile-pass-entry-point pass)
  (-> pass? syntax?)
  "compile the entry-point procedure for PASS to syntax"

  (let* ([pass-ident (pass-ident pass)]
         [input (pass-input pass)]
         [output (pass-output pass)]
         [pass-ref (pass-self-ref pass)]
         [entry-input #'stx]

         [pass-call
          [with-pass-syntax pass ([%if 'if]
                                  [%app '#%app]
                                  [quote 'quote]
                                  [raise-pass-input-predicate-error
                                   'raise-pass-input-predicate-error])
           (if (or (language? input)
                   (eq? #f input))
               #`(%app #,pass-ref #,entry-input)
               #`(%if (%app #,input #,entry-input)
                      (%app #,pass-ref #,entry-input)
                      (%app raise-pass-input-predicate-error
                            (quote #,pass-ident)
                            (quote #,input))))]]

         [entry-body
          (cond
            [(or (language? output)
                 (eq? #f output))
             pass-call]
            [else
             [with-pass-syntax pass ([let 'let]
                                     [unless 'unless]
                                     [raise-pass-output-predicate-error
                                      'raise-pass-output-predicate-error]
                                     [%app '#%app]
                                     [quote 'quote])
              #`(let ([result #,pass-call])
                  (unless (%app #,output result)
                    (%app raise-pass-output-predicate-error
                          (quote #,pass-ident)
                          (quote #,output)
                          result))
                  result)]])])

    [with-pass-syntax pass ([define 'define])
     #`(define (#,pass-ident #,entry-input)
         #,entry-body)]))

; Input handlers (aggregated processors)

(define (compile-pass-input-handlers pass)
  (-> pass? syntax?)
  "compile the input handlers for PASS to syntax"

  (let* ([processors (pass-processors pass)]
         [pass-input (pass-input pass)]
         [clause-pairs
          (for/fold ([clause-pairs (hash)])
                    ([processor (in-list processors)])

            (let* ([input (processor-input processor)]

                   [input-key
                    (if (non-terminal? input)
                        (non-terminal-name input)
                        (syntax-e input))]

                   [proc-clauses (processor-clauses processor)]
                   [proc-output (processor-output processor)]
                   [pairs (for/list ([clause (in-list proc-clauses)])
                            (cons clause proc-output))])

              (hash-set clause-pairs input-key
                        (if (hash-has-key? clause-pairs input-key)
                            (append (hash-ref clause-pairs input-key) pairs)
                            pairs))))])

    (let ([syntax-input (language? pass-input)])
      [datum->syntax (pass-stx pass)
       (for/list ([target (in-list (if syntax-input
                                       (language-non-terminals pass-input)
                                       processors))])

         (let* ([pair (hash-ref clause-pairs
                                (if syntax-input
                                    (non-terminal-name target)
                                    (syntax-e (processor-input target)))
                                null)]
                [clauses (map car pair)]
                [output-classes (map cdr pair)])

           ((if syntax-input
                compile-pass-input-handler/syntax-parse
                compile-pass-input-handler/match)
            pass target
            clauses output-classes)))])))

(define (compile-pass-input-handler/syntax-parse pass non-terminal
                                                 clauses outputs)
  (-> pass?
      non-terminal?
      (listof processor-clause?)
      (or/c (listof non-terminal?)
            (listof syntax?))
      syntax?)
  #:trace-depth 5
  "compile the input handler corresponding to NON-TERMINAL
   to syntax-parse syntax using CLAUSES with OUTPUTS in context of PASS"

  (let* ([auto-generate (and (language? (pass-input pass)) 
                             (language? (pass-output pass)))]
         [undefined (if auto-generate
                        (generate-undefined-clauses pass clauses non-terminal)
                        null)]

         [clauses (if auto-generate
                      (append clauses undefined)
                      clauses)]

         [outputs (if auto-generate
                      (append outputs (map (const non-terminal) undefined))
                      outputs)]

         [outputs
          (if auto-generate
              (for/list ([output (in-list outputs)])
                [language-introduce-datum (pass-output pass)
                 (non-terminal-ident output)])
              outputs)]

         [literals
          (for/list ([name (non-terminal-literal-names non-terminal)])
            (datum->pass-syntax pass name))]

         [datum-literals
          (for/list ([name (non-terminal-datum-literal-names non-terminal)])
            (datum->pass-syntax pass name))])

    [with-pass-syntax pass ([define 'define]
                            [let 'let]
                            [syntax-parser 'syntax-parser])

     [with-pass-bindings pass ([input-handler-ident
                                (non-terminal-ident non-terminal)])

      (with-syntax* ([pass-ident (pass-ident pass)]
                     [pass-ref (pass-self-ref pass)]
                     [(literal ...)
                      (if (pair? literals)
                          #`(#:literals #,literals)
                          #'())]
                     [(datum-literal ...)
                      (if (pair? datum-literals)
                          #`(#:datum-literals #,datum-literals)
                          #'())]
                     [(clause ...) [compile-clauses pass
                                    clauses
                                    outputs]])

        #'(define input-handler-ident
            (let ([pass-ident pass-ref])
              (syntax-parser
                literal
                ...
                datum-literal
                ...
                clause
                ...))))]]))

(define (compile-pass-input-handler/match pass processor
                                          clauses outputs)
  (-> pass?
      processor?
      (listof processor-clause?)
      (or/c (listof non-terminal?)
            (listof syntax?))
      syntax?)
  "compile the input handler corresponding to PROCESSOR
   to match syntax using CLAUSES with OUTPUTS in context of PASS"

  (let* ([pass-output (pass-output pass)]
         [outputs
          (for/list ([output (in-list outputs)])
            (if (non-terminal? output)
                [language-introduce-datum pass-output
                 (non-terminal-ident output)]
                output))])

    [with-pass-syntax pass ([define 'define]
                            [let 'let]
                            [match 'match])

     [with-pass-bindings pass ([input-handler-ident (processor-input processor)])

      (with-syntax ([pass-ident (pass-ident pass)]
                    [pass-ref (pass-self-ref pass)]
                    [(clause ...) [compile-clauses pass
                                   clauses
                                   outputs]])

        #'(define (input-handler-ident val)
            (let ([pass-ident pass-ref])
              (match val
                clause
                ...))))]]))

; Processor clause

(define (compile-clauses pass clauses output-classes)
  (-> pass?
      (listof processor-clause?)
      (listof (or/c non-terminal? syntax?))
      syntax?)
  "compile CLAUSES to syntax in context of PASS and PROCESSOR"

  (let* ([pass-output (pass-output pass)]

         [clause-patterns (map processor-clause-pattern clauses)]
         [clause-pattern-syntaces
          (for/list ([pattern (in-list clause-patterns)])
            (compile-clause-pattern pass pattern))]

         [clause-pattern-strings (map ~s clause-patterns)]
         [clause-bodies (map processor-clause-body clauses)]
         [clause-tails (map last clause-bodies)]
         [clause-bodies (map (curryr drop-right 1) clause-bodies)])

    (with-syntax*
      ([pass-name (pass-name pass)]
       [(clause-pattern-syntax ...) clause-pattern-syntaces]
       [([clause-body ...] ...) clause-bodies]
       [(output-class ...) output-classes]
       [(clause-tail ...)
        (cond
          [(eq? #f pass-output)
           clause-tails]

          [(language? pass-output)
           [with-pass-syntax pass ([syntax-parse 'syntax-parse]
                                   [~var '~var]
                                   [attribute 'attribute])
            (with-syntax ([(clause-tail ...) clause-tails])

              #'[(syntax-parse clause-tail
                   [(~var result output-class)
                    (attribute result)])
                 ...])]]

          [else
           [with-pass-syntax pass
            ([let 'let]
             [unless 'unless]
             [%app '#%app]
             [raise-processor-output-predicate-error
              'raise-processor-output-predicate-error]
             [quote 'quote]
             [%datum '#%datum])

            (with-syntax ([(clause-pattern-string ...)
                           clause-pattern-strings]
                          [(clause-tail ...)
                           clause-tails])

              #`((let ([result clause-tail])
                   (unless (%app output-class result)
                     (%app raise-processor-output-predicate-error
                           (quote pass-name)
                           (quote output-class)
                           (%datum . clause-pattern-string)
                           result))
                   result)
                 ...))]])])

      [with-pass-syntax pass ([raise-syntax-error 'raise-syntax-error]
                              [%app '#%app]
                              [%datum '#%datum]
                              [quote 'quote]
                              [syntax 'syntax])
       #`([clause-pattern-syntax
           clause-body
           ...
           clause-tail]
          ...

          [stx [%app raise-syntax-error (quote pass-name)
                (%datum . "unrecognized production")
                (syntax stx)]])])))

(define (generate-undefined-clauses pass clauses non-terminal)
  (-> pass? (listof processor-clause?) non-terminal? (listof processor-clause?))
  "compute the set of undefined clauses in PROCESSOR, and return
   the corresponding set of recursive pass-through clauses for them
   in the context of PASS"

  (let* ([productions
          (for/list ([clause (in-list clauses)])
            (processor-clause-pattern->non-terminal-pattern
              (processor-clause-pattern clause)))]

         [undefined
          (for/list ([prod (in-list (non-terminal-productions non-terminal))]
                     #:unless (member prod productions pattern=?))
            prod)])

    (for/list ([pattern (in-list undefined)])

      (let* ([clause (non-terminal-pattern->clause pass pattern)]
             [pat (car clause)]
             [body (cdr clause)])

        [with-pass-syntax pass ([%syntax 'syntax])
         (with-syntax ([body body])
           (processor-clause (pattern-stx pat)
                             pat
                             (list #'(%syntax body))))]))))

(define (non-terminal-pattern->clause pass pat)
  (-> pass? pattern? (cons/c pattern? syntax?))
  "convert the non-terminal pattern PAT into a processor clause
   in the context of PASS"

  (match pat
    [(p-list stx lst)
     (let-values ([(patterns bodies)
                   (for/lists (_patterns _bodies)
                              ([pattern (in-list lst)]
                               #:when (match pattern
                                        [(p-literal ident)
                                         #:when (datum=? #'~cut ident)
                                         #f]
                                        [else #t]))
                     (let* ([pattern*
                             (match pattern
                               [(p-list (p-literal ident) pattern)
                                #:when (datum=? #'~maybe ident)
                                pattern]
                               [_ pattern])]
                            [clause
                             (non-terminal-pattern->clause pass pattern*)])
                       (values (car clause)
                               (cdr clause))))])

       (cons (p-list stx patterns) (datum->syntax stx bodies)))]

    [(p-ident ident)
     (let* ([lctx (pass-context pass)]
            [input (pass-input pass)]
            [terminals (language-terminals input)])

       (if (findf (λ (terminal)
                    (datum=? (terminal-ident/name terminal) ident))
                  terminals)

           (cons (p-ident (format-id lctx "~a:~a" ident ident))
                 (format-id lctx "~a" ident))

           (let* ([tmp (format-id lctx "~a" (generate-temporary ident))]
                  [ident (format-id lctx "~a:~a" tmp ident)])
             (cons (p-list ident
                           (list (p-literal #'~rec)
                                 (p-ident ident)))
                   tmp))))]

    [(p-keyword stx)
     (cons (p-keyword stx) stx)]

    [(p-literal ident)
     (cons (p-ident ident) ident)]

    [(p-repeat stx min)
     (cons (p-repeat stx 0) (datum->pass-syntax pass '...))]))

; Processor clause

(define (compile-clause-pattern pass pat)
  (-> pass? pattern? syntax?)
  "compile the clause pattern PAT to syntax in the context of PASS"

  (match pat
    [(p-list _stx (list (p-literal lit)
                        (p-ident ident)))
     #:when (datum=? lit #'~rec)

     (if (language? (pass-input pass))
         (compile-clause-pattern/syntax-rec pass ident)
         (compile-clause-pattern/match-rec pass ident))]

    [(p-list stx lst)
     (compile-clause-pattern/list pass stx lst)]

    [(p-ident ident)
     (syntax-parse ident
       [ident+class:ident+class
        (compile-clause-pattern/ident+class pass
                                            #'ident+class.ident
                                            #'ident+class.class)]
       [_ (pattern-stx pat)])]

    [(p-literal ident)
     (cond
       [(datum=? #'~maybe ident)
        [with-pass-syntax pass ([~optional '~optional])
         #'~optional]]
       [(datum=? #'~cut ident)
        [with-pass-syntax pass ([~! '~!])
         #'~!]]
       [else (pattern-stx pat)])]

    [_ (pattern-stx pat)]))

(define (compile-clause-pattern/ident+class pass ident class)
  (-> pass? syntax? syntax? syntax?)
  "compile IDENT and CLASS to a pattern variable in the context of PASS"

  (let* ([input (pass-input pass)]
         [class
          (if (language? input)
              (language-introduce input
                                  (datum->pass-syntax pass class))
              class)])

    [with-pass-syntax pass ([~var '~var])
     #`(~var #,ident #,class)]))

(define (compile-clause-pattern/list pass lctx lst)
  (-> pass? syntax? (listof pattern?) syntax?)
  "compile the list pattern LST to syntax in the syntactic context of LCTX
   and compilation context of PASS"

  [datum->syntax lctx
   (for/list ([pattern (in-list lst)])
     (compile-clause-pattern pass pattern))])

(define (compile-clause-pattern/syntax-rec pass ident+class)
  (-> pass? syntax? syntax?)
  "compile a (~rec IDENT+CLASS) action pattern to syntax in context of PASS"

  (define-values (ident class) (split-ident+class ident+class))

  [with-pass-syntax pass ([~and '~and]
                          [~var '~var]
                          [~parse '~parse]
                          [attribute 'attribute]
                          [%app '#%app])

   (with-syntax ([ident (datum->pass-syntax pass ident)]

                 [temp-ident
                  (format-id #'pat "~a" (generate-temporary #'ident))]

                 [class
                  (let ([input (pass-input pass)])
                    (if (language? input)
                        [language-introduce input
                         (datum->pass-syntax pass class)]
                        class))]

                 [pass-ref (pass-self-ref pass)])

     #`(~and (~var temp-ident class)
             (~parse ident (%app pass-ref (attribute temp-ident)))))])

(define (compile-clause-pattern/match-rec pass ident)
  (-> pass? syntax? syntax?)
  "compile a (~rec IDENT) match pattern to syntax in context of PASS"

  (let ([pass-ref (pass-self-ref pass)])
    #`(app #,pass-ref #,ident)))

