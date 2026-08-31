#lang picopass/impl

; Language compilation pipeline

(require (for-template racket/base
                       racket/pretty
                       syntax/parse
                       picopass/logger)

         racket/syntax

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
                    (compile-non-terminal language non-terminal))])

    #`(begin

        (define-syntax ident
          #,language)

        terminal
        ...

        non-terminal
        ...)))

(define (compile-language-parser name language)
  (-> syntax? language? syntax?)
  "compile the parser for LANGUAGE to syntax"
  [with-language-bindings language ([entry-point
                                     (language-entry-point-ident language)])
   (with-syntax* ([name name])
     #`(define name
         (syntax-parser
           [(~var _ entry-point)
            this-syntax])))])

(define (compile-terminal language terminal)
  (-> language? terminal? syntax?)
  "compile TERMINAL to syntax in context of LANGUAGE"

  [with-language-syntax language ([class (terminal-class terminal)])

   [with-language-bindings language ([class-name (terminal-name terminal)])

    (with-syntax ([class (make-rename-transformer #'class)])

      #`(define-syntax class-name class))]])

(define (compile-non-terminal language non-terminal)
  (-> language? non-terminal? syntax?)
  "compile NON-TERMINAL to syntax in context of LANGUAGE"

  (let* ([language-name (language-name language)]
         [name (non-terminal-name non-terminal)]

         [literals
          (for/list ([literal (in-list (non-terminal-literals
                                         non-terminal))])
            (language-introduce-datum language (syntax->datum literal)))]

         [datum-literals
          (for/list ([datum-literal (in-list (non-terminal-datum-literals
                                               non-terminal))])
            (language-introduce-datum language (syntax->datum datum-literal)))]

         [productions
          (for/list ([production (in-list (non-terminal-productions
                                            non-terminal))])
            (compile-production language non-terminal production))])

    [with-language-syntax language
     ([description
       (format "~a ~a"
               (or (language-description language)
                   language-name)
               (or (non-terminal-description non-terminal)
                   name))])

     [with-language-bindings language ([class-name name])

      (with-syntax ([(production ...) productions]
                    [(literal ...)
                     (if (pair? literals)
                         #`(#:literals #,literals)
                         #'())]
                    [(datum-literal ...)
                     (if (pair? datum-literals)
                         #`(#:datum-literals #,datum-literals)
                         #'())])

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
            ...))]]))

(define (compile-production lang non-terminal production)
  (-> language? non-terminal? pattern? syntax?)
  "compile PRODUCTION to syntax in context of NON-TERMINAL in LANGUAGE"

  (cond
    [(p-ident? production)
     (let ([ident (p-ident-ident production)]
           [literals (non-terminal-literals non-terminal)]
           [datum-literals (non-terminal-datum-literals non-terminal)])
       (if (or (member ident literals datum=?)
               (member ident datum-literals datum=?))
           ident
           [with-language-bindings lang ([ident ident])
            #'(~var _ ident)]))]

    [(p-literal? production)
     (let ([ident (p-literal-ident production)])
       (cond
         [(datum=? #'~maybe ident)
          #'~optional]
         [(datum=? #'~cut ident)
          #'~!]
         [else
          (language-introduce-datum lang ident)]))]

    [(p-keyword? production)
     (p-keyword-stx production)]

    [(p-list? production)
     [datum->syntax (p-list-stx production)
      (for/list ([pattern (in-list (p-list-list production))])
        (compile-production lang non-terminal pattern))]]

    [(p-repeat? production)
     (p-repeat-stx production)]))

