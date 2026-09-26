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
         syntax/strip-context

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
                   [pass-introduce pass
                    [replace-context (pass-context pass)
                     entry-point-ident]]])

      #'(syntax-parser
          [(~var prod non-terminal-ident)
           (pass-ident (attribute prod))]))))

(define (compile-pass-dispatch/match pass)
  (-> pass? syntax?)
  "compile match dispatch for PASS to syntax"

  (let* ([processors (pass-processors pass)]
         [processor-inputs (map processor-input-ident processors)]
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

(define (input-handler-generate-unhandled handler pass)
  (-> input-handler? pass? input-handler?)
  (let* ([pass-input (pass-input pass)])

    (if (language? pass-input)

        (let* ([handler-input (input-handler-ident handler)]
               [non-terminal (language-non-terminal pass-input handler-input)])

          (let* ([productions (for*/list ([processor (in-list (input-handler-processors handler))]
                                          [clause (in-list (processor-clauses processor))])
                                (processor-clause-pattern->non-terminal-pattern
                                  (processor-clause-pattern clause)))]

                 [undefined
                  (for/list ([prod (in-list (non-terminal-productions non-terminal))]
                             #:unless (member prod productions pattern=?))
                    prod)]

                 [clauses
                  (for/list ([pattern (in-list undefined)])
                    (let* ([clause (non-terminal-pattern->clause pass pattern)]
                           [pat (car clause)]
                           [body (cdr clause)])
                      (with-syntax ([body body])
                        (processor-clause (pattern-stx pat)
                                          pat
                                          (list #'#'body)))))]

                 [pass-output (pass-output pass)]

                 [processor (processor (pass-context pass)
                                       #'unhandled
                                       handler-input
                                       (if (language? pass-output)
                                           handler-input
                                           pass-output)
                                       clauses)]

                 [handler (input-handler handler-input
                                         (append (input-handler-processors handler)
                                                 (list processor)))])

            handler))

        handler)))

(define (compile-pass-input-handlers pass)
  (-> pass? syntax?)
  "compile the input handlers for PASS to syntax"

  (let* ([pass-input (pass-input pass)]
         [processors (pass-processors pass)]
         [unique-input-idents
          (if (language? pass-input)
              (for/list ([non-terminal (in-list (language-non-terminals pass-input))])
                (replace-context (pass-context pass)
                                 (non-terminal-ident non-terminal)))
              (remove-duplicates
                (map processor-input-ident processors)
                datum=?))]

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

    [datum->syntax (pass-context pass)

     (let ([compile-pass-input-handler
            (if (language? pass-input)
                compile-pass-input-handler/syntax-parse
                compile-pass-input-handler/match)])

       (for/list ([handler (in-list input-handlers)])
         (let ([handler (input-handler-generate-unhandled handler pass)])
           (compile-pass-input-handler pass handler))))]))

(define (compile-pass-input-handler/syntax-parse pass handler)
  (-> pass? input-handler? syntax?)
  #:trace-depth 5
  "compile the input handler corresponding to NON-TERMINAL
   to syntax-parse syntax using CLAUSES with OUTPUTS in context of PASS"

  (let* ([input-ident (input-handler-ident handler)]
         [non-terminal (language-non-terminal (pass-input pass)
                                              input-ident)]

         [literals
          (for/list ([literal (non-terminal-literals non-terminal)])
            (replace-context (pass-context pass) literal))]

         [datum-literals
          (for/list ([datum-literal (non-terminal-datum-literals non-terminal)])
            (replace-context (pass-context pass) datum-literal))])

    (with-syntax* ([pass-name (pass-name pass)]
                   [input-ident (pass-introduce pass input-ident)]
                   [(literal ...)
                    (if (pair? literals)
                        #`(#:literals #,literals)
                        #'())]
                   [(datum-literal ...)
                    (if (pair? datum-literals)
                        #`(#:datum-literals #,datum-literals)
                        #'())]
                   [([clause ...] ...)
                    (for/list ([processor (in-list (input-handler-processors handler))])
                      (compile-processor pass handler processor))])

      #'(define input-ident
          (syntax-parser
            literal
            ...
            datum-literal
            ...
            clause
            ...
            ...
            [stx [raise-syntax-error (quote pass-name)
                  "unrecognized production"
                  #'stx]])))))

(define (compile-pass-input-handler/match pass handler)
  (-> pass? input-handler? syntax?)
  "compile the input handler corresponding to PROCESSOR
   to match syntax using CLAUSES with OUTPUTS in context of PASS"

  (with-syntax ([pass-name (pass-name pass)]
                [input-ident (pass-introduce pass (input-handler-ident handler))]
                [([clause ...] ...)
                 (for/list ([processor (in-list (input-handler-processors handler))])
                   (compile-processor pass handler processor))])

    #'(define (input-ident val)
        (match val
          clause
          ...
          ...
          [val [raise-syntax-error (quote pass-name)
                "unrecognized production"
                val]]))))

; Processor

(define (compile-processor pass handler processor)
  (-> pass? input-handler? processor? syntax?)
  [datum->syntax (input-handler-ident handler)
   (for/list ([clause (in-list (processor-clauses processor))])
     (compile-clause pass handler processor clause))])

; Processor clause

(define (compile-clause pass _handler processor clause)
  (-> pass?
      input-handler?
      processor?
      processor-clause?
      syntax?)

  (let* ([pass-output (pass-output pass)]
         [processor-output (processor-output-ident processor)]
         [clause-pattern (processor-clause-pattern clause)]
         [clause-pattern-syntax (compile-clause-pattern pass clause-pattern)]
         [clause-pattern-string (~s clause-pattern)]
         [clause-body (processor-clause-body clause)]
         [clause-tail (last clause-body)]
         [clause-body (drop-right clause-body 1)])

    (with-syntax*
      ([pass-name (pass-name pass)]
       [clause-pattern-syntax clause-pattern-syntax]
       [[clause-body ...] clause-body]
       [output
        (if (language? pass-output)
            (language-introduce pass-output processor-output)
            processor-output)]
       [clause-tail
        (cond
          [(eq? #f pass-output)
           clause-tail]

          [(language? pass-output)
           (with-syntax ([clause-tail clause-tail])
             #'(syntax-parse clause-tail
                 [(~var result output)
                  (attribute result)]))]

          [else
           (with-syntax ([clause-pattern-string clause-pattern-string]
                         [clause-tail clause-tail])
             #`(let ([result clause-tail])
                 (unless (output result)
                   (raise-processor-output-predicate-error
                     (quote pass-name)
                     (quote output)
                     clause-pattern-string
                     result))
                 result))])])

      #`[clause-pattern-syntax
         clause-body
         ...
         clause-tail])))

(define (non-terminal-pattern->clause pass pat)
  (-> pass? pattern? (cons/c pattern? syntax?))
  "convert the non-terminal pattern PAT into a processor clause
   in the context of PASS"

  (let ([lctx (pass-context pass)])
    (match pat
      [(p-list stx lst tail)
       (let ([tail (and tail
                        (non-terminal-pattern->clause tail))])
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
                 [(p-list _stx (list (p-literal lit) pattern*) #f)
                  #:when (datum=? #'~maybe lit)
                  (let ([clause (non-terminal-pattern->clause pass pattern*)])
                    (with-syntax ([~? (quote-syntax ~?)]
                                  [tmp (generate-temporary)])
                      (values (p-list lctx
                                      (list (p-literal #'~optional)
                                            (p-list lctx
                                                    (list (p-literal #'~and)
                                                          (p-ident #'tmp)
                                                          (car clause))
                                                    #f))
                                      #f)
                              #'(~? tmp))))]
                 [_ (let ([clause (non-terminal-pattern->clause pass pattern)])
                      (values (car clause)
                              (cdr clause)))]))])

           (cons (p-list stx patterns tail)
                 (datum->syntax stx bodies))))]

      [(p-ident ident)
       (let* ([input (pass-input pass)]
              [terminals (language-terminals input)])

         (if (findf (λ (terminal)
                      (datum=? (terminal-ident/name terminal) ident))
                    terminals)

             (let* ([tmp (format-id lctx "~a" (generate-temporary ident))])
               (cons (p-ident (format-id lctx "~a:~a" tmp ident))
                     tmp))

             (let* ([tmp (format-id lctx "~a" (generate-temporary ident))]
                    [ident (format-id lctx "~a:~a" tmp ident)])
               (cons (p-list ident
                             (list (p-literal #'~rec)
                                   (p-ident ident))
                             #f)
                     tmp))))]

      [(p-keyword stx)
       (cons (p-keyword stx) stx)]

      [(p-literal ident)
       (let ([ident (replace-context (pass-context pass) ident)])
         (cons (p-ident ident) ident))]

      [(p-repeat stx _min)
       (cons (p-repeat stx 0)
             (datum->syntax (pass-context pass) '...))])))

; Processor clause

(define (compile-clause-pattern pass pat)
  (-> pass? pattern? syntax?)
  "compile the clause pattern PAT to syntax in the context of PASS"

  (match pat
    [(p-list _stx (list (p-literal lit) (p-ident ident)) #f)
     #:when (datum=? lit #'~rec)

     (if (language? (pass-input pass))
         (compile-clause-pattern/syntax-rec pass ident)
         (compile-clause-pattern/match-rec pass ident))]

    [(p-list stx lst tail)
     [datum->syntax stx
      (foldr cons
             (if tail
                 (compile-clause-pattern pass tail)
                 null)
             (for/list ([pattern (in-list lst)])
               (compile-clause-pattern pass pattern)))]]

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
              (language-introduce input class)
              class)])

    #`(~var #,ident #,class)))

(define (compile-clause-pattern/syntax-rec pass ident+class)
  (-> pass? syntax? syntax?)
  "compile a (~rec IDENT+CLASS) action pattern to syntax in context of PASS"

  (define-values (ident class) (split-ident+class ident+class))

  (let ([lang (pass-input pass)])

    (with-syntax ([ident (replace-context (pass-context pass) ident)]

                  [temp-ident
                   (format-id #'pat "~a" (generate-temporary #'ident))]

                  [class/language [language-introduce lang
                                   [replace-context (language-context lang)
                                    class]]]

                  [class/pass [pass-introduce pass class]])

      #`(~and (~var temp-ident class/language)
              (~parse ident (class/pass (attribute temp-ident)))))))

(define (compile-clause-pattern/match-rec pass ident)
  (-> pass? syntax? syntax?)
  "compile a (~rec IDENT) match pattern to syntax in context of PASS"

  (let ([pass-ref (pass-self-ref pass)])
    #`(app #,pass-ref #,ident)))

