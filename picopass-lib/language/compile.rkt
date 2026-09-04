#lang picopass/impl

; Language compilation pipeline

(require (for-template racket/base
                       racket/pretty
                       syntax/parse
                       picopass/logger)

         racket/match
         racket/syntax

         syntax/strip-context

         threading

         picopass/syntax

         picopass/pattern/ir

         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

(define (compile-language language)
  (-> language? syntax?)
  "compile LANGUAGE to syntax"
  (with-syntax* ([ident (language-ident language)]

                 [(terminal ...)
                  (for/list ([terminal (in-list (language-terminals language))])
                    (compile-terminal language terminal))]

                 [(non-terminal ...)
                  (for/list ([non-terminal (in-list (language-non-terminals language))])
                    (compile-non-terminal language non-terminal))]

                 [language (language->syntax language)])

    #`(begin

        (define-syntax ident
          language)

        terminal
        ...

        non-terminal
        ...)))

(define (compile-language-parser name language)
  (-> syntax? language? syntax?)
  "compile the parser for LANGUAGE to syntax"
  (with-syntax* ([name name]
                 [entry-point [language-introduce language
                               (language-entry-point-ident language)]])
    #`(define name
        (syntax-parser
          [(~var _ entry-point)
           this-syntax]))))

(define (compile-terminal language terminal)
  (-> language? terminal? syntax?)
  "compile TERMINAL to syntax in context of LANGUAGE"

  (with-syntax* ([class-name [language-introduce language
                              (terminal-ident/name terminal)]]
                 [class #`(quote-syntax #,(terminal-ident/class terminal))])

    #'(define-syntax class-name 
        (make-rename-transformer class))))

(define (compile-non-terminal language non-terminal)
  (-> language? non-terminal? syntax?)
  "compile NON-TERMINAL to syntax in context of LANGUAGE"

  (let* ([language-name (language-name language)]
         [name (non-terminal-name non-terminal)]

         [literals
          (for/list ([literal (in-list (non-terminal-literals
                                         non-terminal))])
            (language-introduce language literal))]

         [datum-literals
          (for/list ([datum-literal (in-list (non-terminal-datum-literals
                                               non-terminal))])
            (language-introduce language datum-literal))]

         [productions
          (for/list ([production (in-list (non-terminal-productions
                                            non-terminal))])
            (compile-production language non-terminal production))])

    (with-syntax ([description
                   (format "~a ~a"
                           (or (language-description language)
                               language-name)
                           (or (non-terminal-description non-terminal)
                               name))]
                  [(production ...) productions]
                  [(literal ...)
                   (if (pair? literals)
                       #`(#:literals #,literals)
                       #'())]
                  [(datum-literal ...)
                   (if (pair? datum-literals)
                       #`(#:datum-literals #,datum-literals)
                       #'())]
                  [class-name [language-introduce language
                               (non-terminal-ident non-terminal)]])

      #`(define-syntax-class class-name
          #:description description
          literal
          ...
          datum-literal
          ...
          (pattern production
                   #:do ([log-picopass-debug "parse ~a:\n~a"
                          description
                          (pretty-format (syntax->datum this-syntax)
                                         #:mode 'display)]))
          ...))))

(define (compile-production lang non-terminal production)
  (-> language? non-terminal? pattern? syntax?)
  "compile PRODUCTION to syntax in context of NON-TERMINAL in LANGUAGE"

  (match production
    [(p-ident ident)
     (let ([literals (non-terminal-literals non-terminal)]
           [datum-literals (non-terminal-datum-literals non-terminal)])
       (if (or (member ident literals datum=?)
               (member ident datum-literals datum=?))
           ident
           (with-syntax ([ident (language-introduce lang ident)])
             #'(~var _ ident))))]

    [(p-literal literal)
     (cond
       [(datum=? #'~maybe literal)
        #'~optional]
       [(datum=? #'~cut literal)
        #'~!]
       [else
        (language-introduce lang literal)])]

    [(p-keyword keyword)
     keyword]

    [(p-list stx lst tail)
     (let ([pats
            (for/list ([pattern (in-list lst)])
              (compile-production lang non-terminal pattern))]
           [tail (and tail
                      (compile-production lang non-terminal tail))])
       (if tail
           (datum->syntax stx (foldr cons tail pats))
           (datum->syntax stx pats)))]

    [(p-repeat stx _min)
     stx]))

