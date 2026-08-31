#lang picopass/impl

; Pass compilation pipeline

(require (for-template racket/base
                       racket/match
                       syntax/parse
                       picopass/pass/error)

         racket/list
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

        pass-entry-point)))

(define (compile-pass-dispatch pass)
  (-> pass? syntax?)
  "compile dispatch machinery for PASS to syntax"

  (let* ([input (pass-input pass)])

    (with-syntax*
      ([pass-ref (pass-self-ref pass)]
       [dispatch
        (if (language? input)
            (compile-pass-dispatch/syntax-parse pass)
            (compile-pass-dispatch/match pass))])

      #'(define pass-ref
          dispatch))))

(define (compile-pass-dispatch/syntax-parse pass)
  (-> pass? syntax?)
  "compile syntax-parse dispatch for PASS to syntax"

  (let* ([input (pass-input pass)]
         [entry-point-ident (language-entry-point-ident input)])

    (with-syntax ([non-terminal-ident
                   (language-introduce input entry-point-ident)]
                  [pass-ident
                   (pass-introduce pass entry-point-ident)])
      #'(syntax-parser
          [(~var prod non-terminal-ident)
           (pass-ident (attribute prod))]))))

(define (compile-pass-dispatch/match pass)
  (-> pass? syntax?)
  "compile match dispatch for PASS to syntax"

  (let* ([processors (pass-processors pass)]
         [processor-inputs (map processor-input processors)]
         [processor-idents
          (for/list ([processor-input (in-list processor-inputs)])
            (pass-introduce pass processor-input))])

    (with-syntax ([pass-name (pass-name pass)]
                  [(processor-pred ...) processor-inputs]
                  [(processor-ident ...) processor-idents])

      #`(λ (in)
          (cond
            [(processor-pred in)
             (processor-ident in)]
            ...
            [else
             (raise-pass-dispatch-error
               (quote pass-name)
               in)])))))

(define (compile-pass-entry-point pass)
  (-> pass? syntax?)
  "compile the entry-point procedure for PASS to syntax"

  (let* ([pass-ident (pass-ident pass)]
         [input (pass-input pass)]
         [output (pass-output pass)]
         [pass-ref (pass-self-ref pass)]
         [entry-input #'stx]

         [pass-call
          (if (or (language? input)
                  (eq? #f input))
              #`(#,pass-ref #,entry-input)
              #`(if (#,input #,entry-input)
                    (#,pass-ref #,entry-input)
                    (raise-pass-input-predicate-error
                      (quote #,pass-ident)
                      (quote #,input))))]

         [entry-body
          (cond
            [(or (language? output)
                 (eq? #f output))
             pass-call]
            [else
             #`(let ([result #,pass-call])
                 (unless (#,output result)
                   (raise-pass-output-predicate-error
                     (quote #,pass-ident)
                     (quote #,output)
                     result))
                 result)])])

    #`(define (#,pass-ident #,entry-input)
        #,entry-body)))

; Input handlers (aggregated processors)

(struct input-handler [ident processors]
  #:transparent)

(define (make-input-handler ident)
  (-> syntax? input-handler?)
  (input-handler ident null))

(define (input-handler-with-processor handler processor)
  (-> input-handler? processor? input-handler?)
  (let ([ident (input-handler-ident handler)]
        [processors (input-handler-processors handler)])

    (input-handler ident
                   (append processors (list processor)))))

(define (compile-pass-input-handlers pass)
  (-> pass? syntax?)
  "compile the input handlers for PASS to syntax"

  (let* ([pass-input (pass-input pass)]
         [processors (pass-processors pass)]
         [unique-input-idents (remove-duplicates
                                (map processor-input-ident processors)
                                datum=?)]

         [input-handlers (map make-input-handler unique-input-idents)]

         [input-handlers
          (for/fold ([input-handlers input-handlers])
                    ([processor (in-list processors)])
            (let ([index [index-where input-handlers
                          (λ (input-handler)
                            (datum=? (input-handler-ident input-handler)
                                     (processor-input-ident processor)))]])
              (let ([input-handler (list-ref input-handlers index)])
                (list-set input-handlers index
                          (input-handler-with-processor input-handler
                                                        processor)))))])

    [datum->syntax (pass-stx pass)
     (for/list ([input-handler (in-list input-handlers)])
       (let-values ([(clauses outputs)
                     (for/fold ([clauses null]
                                [outputs null])
                               ([processor (in-list (input-handler-processors
                                                      input-handler))])
                       (let* ([proc-clauses (processor-clauses processor)]
                              [proc-outputs (map (const (processor-output processor))
                                                 proc-clauses)])
                         (values (append clauses proc-clauses)
                                 (append outputs proc-outputs))))])
         (let* ([syntax-input (language? pass-input)]
                [input-ident (input-handler-ident input-handler)])
           ((if syntax-input
                compile-pass-input-handler/syntax-parse
                compile-pass-input-handler/match)
            pass input-ident
            clauses outputs))))]))

(define (compile-pass-input-handler/syntax-parse pass input-ident
                                                 clauses outputs)
  (-> pass?
      syntax?
      (listof processor-clause?)
      (or/c (listof non-terminal?)
            (listof syntax?))
      syntax?)
  #:trace-depth 5
  "compile the input handler corresponding to NON-TERMINAL
   to syntax-parse syntax using CLAUSES with OUTPUTS in context of PASS"

  (let* ([non-terminal (findf (λ (nt)
                                (datum=? input-ident
                                         (non-terminal-ident nt)))
                              (language-non-terminals (pass-input pass)))]
         [auto-generate (language? (pass-output pass))]
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

    [with-pass-bindings pass ([input-ident input-ident])

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

       #'(define input-ident
           (syntax-parser
             literal
             ...
             datum-literal
             ...
             clause
             ...)))]))

(define (compile-pass-input-handler/match pass input-ident
                                          clauses outputs)
  (-> pass?
      syntax?
      (listof processor-clause?)
      (or/c (listof non-terminal?)
            (listof syntax?))
      syntax?)
  "compile the input handler corresponding to PROCESSOR
   to match syntax using CLAUSES with OUTPUTS in context of PASS"

  (let* ([processor (findf (λ (processor)
                             (datum=? input-ident
                                      (processor-input processor)))
                           (pass-processors pass))]
         [pass-output (pass-output pass)]
         [outputs
          (for/list ([output (in-list outputs)])
            (if (non-terminal? output)
                [language-introduce-datum pass-output
                 (non-terminal-ident output)]
                output))])

    [with-pass-bindings pass ([input-ident input-ident])

     (with-syntax ([pass-ident (pass-ident pass)]
                   [pass-ref (pass-self-ref pass)]
                   [(clause ...) [compile-clauses pass
                                  clauses
                                  outputs]])

       #'(define (input-ident val)
           (match val
             clause
             ...)))]))

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
           (with-syntax ([(clause-tail ...) clause-tails])

             #'[(syntax-parse clause-tail
                  [(~var result output-class)
                   (attribute result)])
                ...])]

          [else
           (with-syntax ([(clause-pattern-string ...)
                          clause-pattern-strings]
                         [(clause-tail ...)
                          clause-tails])

             #`((let ([result clause-tail])
                  (unless (output-class result)
                    (raise-processor-output-predicate-error
                      (quote pass-name)
                      (quote output-class)
                      clause-pattern-string
                      result))
                  result)
                ...))])])

      #`([clause-pattern-syntax
          clause-body
          ...
          clause-tail]
         ...

         [stx [raise-syntax-error (quote pass-name)
               "unrecognized production"
               (syntax stx)]]))))

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

        (with-syntax ([body body])
          (processor-clause (pattern-stx pat)
                            pat
                            (list #'#'body)))))))

(define (non-terminal-pattern->clause pass pat)
  (-> pass? pattern? (cons/c pattern? syntax?))
  "convert the non-terminal pattern PAT into a processor clause
   in the context of PASS"

  (let ([lctx (pass-context pass)])
    (match pat
      [(p-list stx lst)
       (let-values
         ([(patterns bodies)
           (for/lists (_patterns _bodies)
                      ([pattern (in-list lst)]
                       #:when (match pattern
                                [(p-literal ident)
                                 #:when (datum=? #'~cut ident)
                                 #f]
                                [_ #t]))
             (match pattern
               [(p-list _stx (list (p-literal lit) pattern*))
                #:when (datum=? #'~maybe lit)
                (let ([clause (non-terminal-pattern->clause pass pattern*)])
                  [with-pass-syntax pass ([~? '~?]
                                          [tmp (generate-temporary)])
                   (values (p-list lctx
                                   (list (p-literal #'~optional)
                                         (p-list lctx
                                                 (list (p-literal #'~and)
                                                       (p-ident #'tmp)
                                                       (car clause)))))
                           #'(~? tmp))])]
               [_ (let ([clause (non-terminal-pattern->clause pass pattern)])
                    (values (car clause)
                            (cdr clause)))]))])

         (cons (p-list stx patterns) (datum->syntax stx bodies)))]

      [(p-ident ident)
       (let* ([input (pass-input pass)]
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

      [(p-repeat stx _min)
       (cons (p-repeat stx 0) (datum->pass-syntax pass '...))])))

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
        #'~optional]
       [(datum=? #'~cut ident)
        #'~!]
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

    #`(~var #,ident #,class)))

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

  (let ([lang (pass-input pass)])
    (with-syntax ([ident (datum->pass-syntax pass ident)]

                  [temp-ident
                   (format-id #'pat "~a" (generate-temporary #'ident))]

                  [class/language [language-introduce lang class]]

                  [class/pass [pass-introduce pass class]])

      #`(~and (~var temp-ident class/language)
              (~parse ident (class/pass (attribute temp-ident)))))))

(define (compile-clause-pattern/match-rec pass ident)
  (-> pass? syntax? syntax?)
  "compile a (~rec IDENT) match pattern to syntax in context of PASS"

  (let ([pass-ref (pass-self-ref pass)])
    #`(app #,pass-ref #,ident)))

