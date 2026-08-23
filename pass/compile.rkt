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

(define (compile-pass self)
  (-> pass? syntax?)
  "compile the pass SELF to syntax"

  [with-pass-syntax self ([begin 'begin])

   (with-syntax ([pass-dispatch (compile-pass-dispatch self)]

                 [(processor-definition ...)
                  (map (curryr compile-processor self)
                       (pass-processors self))]

                 [pass-entry (compile-pass-entry self)])

     #`(begin

         pass-dispatch

         processor-definition
         ...

         pass-entry))])

(define (compile-pass-dispatch self)
  (-> pass? syntax?)
  "compile dispatch machinery for pass SELF to syntax"

  (let* ([input (pass-input self)])

    [with-pass-syntax self ([define 'define])
     (with-syntax*
       ([pass-ref (pass-self-ref self)]
        [dispatch
         (if (language? input)
             (compile-pass-dispatch/syntax-parse self)
             (compile-pass-dispatch/match self))])

       #'(define pass-ref
           dispatch))]))

(define (compile-pass-dispatch/syntax-parse self)
  (-> pass? syntax?)
  "compile match dispatch for pass SELF to syntax"

  (let* ([input (pass-input self)]
         [processors (pass-processors self)]
         [processor-inputs (map processor-input processors)]
         [processor-idents
          (map (compose (curry pass-introduce self)
                        processor-ident)
               processors)])

    [with-pass-syntax self ([syntax-parser 'syntax-parser]
                            [~var '~var]
                            [attribute 'attribute]
                            [%app '#%app])

     (with-syntax ([(processor-ident ...) processor-idents]
                   [(processor-class ...)
                    (map (compose (curry language-introduce
                                         input)
                                  non-terminal-ident)
                         processor-inputs)])

       #`(syntax-parser
           [(~var prod processor-class)
            (%app processor-ident (attribute prod))]
           ...))]))

(define (compile-pass-dispatch/match self)
  (-> pass? syntax?)
  "compile syntax-parse dispatch for pass SELF to syntax"

  (let* ([processors (pass-processors self)]
         [processor-inputs (map processor-input processors)]
         [processor-idents
          (map (compose (curry pass-introduce self)
                        processor-ident)
               processors)])

    [with-pass-syntax self ([λ 'λ]
                            [cond 'cond]
                            [%app '#%app]
                            [raise-pass-dispatch-error
                             'raise-pass-dispatch-error]
                            [else 'else]
                            [quote 'quote])

     (with-syntax ([pass-name (pass-name self)]
                   [(processor-ident ...) processor-idents]
                   [(processor-pred ...) processor-inputs])

       #`(λ (in)
           (cond
             [(%app processor-pred in)
              (%app processor-ident in)]
             ...
             [else
              (%app raise-pass-dispatch-error
                    (quote pass-name)
                    in)])))]))

(define (compile-pass-entry self)
  (-> pass? syntax?)
  "compile the entry-point procedure for pass SELF to syntax"

  (let* ([pass-ident (pass-ident self)]
         [input (pass-input self)]
         [output (pass-output self)]
         [pass-ref (pass-self-ref self)]
         [entry-input #'stx]

         [pass-call
          [with-pass-syntax self ([%if 'if]
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
             [with-pass-syntax self ([let 'let]
                                     [unless 'unless]
                                     [raise-pass-output-predicate-error
                                      'raise-pass-output-predicate-error]
                                     [%app '#%app]
                                     [quote 'quote])
              #`(let ([result #,pass-call])
                  (unless (%app #,output result)
                    (%app raise-pass-output-predicate-error
                          (quote #,pass-ident)
                          (quote #,output)))
                  result)]])])

    [with-pass-syntax self ([define 'define])
     #`(define (#,pass-ident #,entry-input)
         #,entry-body)]))


; Processor

(define (compile-processor self pass)
  (-> processor? pass? syntax?)
  "compile the processor SELF to syntax in context of PASS"

  [with-pass-syntax pass ([define 'define]
                          [let 'let])

   [with-pass-bindings pass ([processor-ident (processor-ident self)])

    (with-syntax ([pass-ident (pass-ident pass)]
                  [pass-ref (pass-self-ref pass)]
                  [processor-body (compile-processor-body pass self)])

      #'(define processor-ident
          (let ([pass-ident pass-ref])
            processor-body)))]])

(define (compile-processor-body pass processor)
  (-> pass? processor? syntax?)
  "compile PROCESSOR's body form in the context of PASS"

  (let* ([pass-input (pass-input pass)])
    (if (language? pass-input)
        (compile-processor-body/syntax-parse pass processor)
        (compile-processor-body/match pass processor))))

(define (compile-processor-body/syntax-parse pass processor)
  (-> pass? processor? syntax?)
  "compile a syntax-parse body form for PROCESSOR in the context of PASS"

  (let* ([clauses (processor-clauses processor)]
         [undefined (generate-undefined-clauses pass processor)]
         [input-non-terminal (processor-input processor)]
         [literals (map (curry datum->syntax (pass-context pass))
                        (non-terminal-literal-names input-non-terminal))]
         [datum-literals
          (map (curry datum->syntax (pass-context pass))
               (non-terminal-datum-literal-names input-non-terminal))])

    [with-pass-syntax pass ([syntax-parser 'syntax-parser])
     (with-syntax ([(literal ...)
                    (if (pair? literals)
                        #`(#:literals #,literals)
                        #'())]
                   [(datum-literal ...)
                    (if (pair? datum-literals)
                        #`(#:datum-literals #,datum-literals)
                        #'())]
                   [(clause ...) [compile-clauses pass processor
                                  (append clauses undefined)]])

       #'(syntax-parser
           literal
           ...
           datum-literal
           ...
           clause
           ...))]))

(define (compile-processor-body/match pass processor)
  (-> pass? processor? syntax?)
  "compile a match body form for PROCESSOR in the context of PASS"

  (let ([clauses (processor-clauses processor)])

    [with-pass-syntax pass ([λ 'λ]
                            [match 'match])

     (with-syntax ([(clause ...) (compile-clauses pass processor clauses)])

       #'(λ (val)
           (match val
             clause
             ...)))]))

(define (compile-clauses pass processor clauses)
  (-> pass? processor? (listof processor-clause?) syntax?)
  "compile CLAUSES to syntax in context of PASS and PROCESSOR"

  (let* ([pass-output (pass-output pass)]

         [processor-ident (pass-introduce pass (processor-ident processor))]

         [clause-patterns (map processor-clause-pattern clauses)]
         [clause-pattern-syntaces (map (curryr compile-clause-pattern pass)
                                       clause-patterns)]

         [clause-pattern-strings (map ~s clause-patterns)]
         [clause-bodies (map processor-clause-body clauses)]
         [clause-tails (map last clause-bodies)]
         [clause-bodies (map (curryr drop-right 1) clause-bodies)])

    (with-syntax*
      ([pass-name (pass-name pass)]
       [(clause-pattern-syntax ...) clause-pattern-syntaces]
       [([clause-body ...] ...) clause-bodies]
       [(clause-tail ...)
        (cond
          [(eq? #f pass-output)
           clause-tails]

          [(language? pass-output)
           [with-pass-syntax pass ([syntax-parse 'syntax-parse]
                                   [~var '~var]
                                   [attribute 'attribute])
            [with-language-bindings pass-output
             ([output-class
               (non-terminal-ident (processor-output processor))])

             (with-syntax ([(clause-tail ...) clause-tails])

               #'[(syntax-parse clause-tail
                    [(~var result output-class)
                     (attribute result)])
                  ...])]]]

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
                   (unless (%app #,pass-output result)
                     (%app raise-processor-output-predicate-error
                           (quote pass-name)
                           (quote #,pass-output)
                           (quote #,processor-ident)
                           (%datum clause-pattern-string)))
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
                (%datum "unrecognized production")
                (syntax stx)]])])))

(define (generate-undefined-clauses pass processor)
  (-> pass? processor? (listof processor-clause?))
  "compute the set of undefined clauses in PROCESSOR, and return
   the corresponding set of recursive pass-through clauses for them
   in the context of PASS"

  (let* ([productions
          (map (compose processor-clause-pattern->non-terminal-pattern
                        processor-clause-pattern)
               (processor-clauses processor))]

         [undefined
          (filter (compose not (curryr member productions pattern=?))
                  (non-terminal-productions (processor-input processor)))]

         [undefined-clauses
          (map (curry non-terminal-pattern->clause pass)
               undefined)]

         [patterns (map car undefined-clauses)]
         [bodies (map cdr undefined-clauses)]

         [clauses
          (for/list ([pat (in-list patterns)]
                     [body (in-list bodies)])
            [with-pass-syntax pass ([%syntax 'syntax])
             (with-syntax ([body body])
               (processor-clause (pattern-stx pat)
                                 pat
                                 (list #'(%syntax body))))])])

    clauses))

(define (non-terminal-pattern->clause pass pat)
  (-> pass? pattern? (cons/c pattern? syntax?))
  "convert the non-terminal pattern PAT into a processor clause
   in the context of PASS"

  (match pat
    [(p-list stx lst)
     (let* ([pairs (map (curry non-terminal-pattern->clause pass) lst)]
            [cars (map car pairs)]
            [cdrs (map cdr pairs)])
       (cons (p-list stx cars) (datum->syntax stx cdrs)))]

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

    [(p-literal ident)
     (cons (p-ident ident) ident)]

    [(p-repeat stx min)
     (cons (p-repeat stx min) stx)]))

; Processor clause

(define (compile-clause-pattern pat pass)
  (-> pattern? pass? syntax?)
  "compile the clause pattern PAT to syntax in the context of PASS"

  (match pat
    [(p-list _stx (list (p-literal lit)
                        (p-ident ident+class)))
     #:when (datum=? lit #'~rec)

     (compile-clause-pattern/syntax-rec pass ident+class)]

    [(p-list _stx (list (p-literal lit)
                        (p-ident ident)))
     #:when (datum=? lit #'rec)
     (compile-clause-pattern/match-rec pass ident)]

    [(p-list stx lst)
     (compile-clause-pattern/list pass stx lst)]

    [(p-ident ident)
     (syntax-parse ident
       [ident+class:ident+class
        (compile-clause-pattern/ident+class pass
                                            #'ident+class.ident
                                            #'ident+class.class)]
       [_ (pattern-stx pat)])]

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
   (map (curryr compile-clause-pattern pass) lst)])

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
  "compile a (rec IDENT) match pattern to syntax in context of PASS"

  (let ([pass-ref (pass-self-ref pass)])
    #`(app #,pass-ref #,ident)))

