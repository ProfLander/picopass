#lang picopass/impl

; Language compilation pipeline

(require racket/syntax

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
  [with-language-syntax language ([begin 'begin]
                                  [define 'define]
                                  [define-syntax 'define-syntax]
                                  [syntax-parser 'syntax-parser]
                                  [~var '~var]
                                  [this-syntax 'this-syntax])

   [with-language-bindings language ([entry-point
                                      (language-entry-point-ident language)])

    (with-syntax* ([ident (language-ident language)]

                   [(terminal ...)
                    (for/list ([terminal (in-list (language-terminals language))])
                      (compile-terminal language terminal))]

                   [(non-terminal ...)
                    (for/list ([non-terminal (in-list (language-non-terminals language))])
                      (compile-non-terminal language non-terminal))]

                   [parse-language
                    (format-id (language-context language) "parse-~a" #'ident)])

      #`(begin
          (define-syntax ident
            #,language)
          terminal
          ...
          non-terminal
          ...
          (define parse-language
            (syntax-parser
              [(~var _ entry-point) this-syntax]))))]])

(define (compile-terminal language terminal)
  (-> language? terminal? syntax?)
  "compile TERMINAL to syntax in context of LANGUAGE"

  (let* ([name (terminal-name terminal)]
         [description (format "~a ~a" (language-name language) name)])

    [with-language-syntax language ([define-syntax-class
                                     'define-syntax-class]
                                    [pattern 'pattern]
                                    [~var '~var]
                                    [description description]
                                    [class (terminal-class terminal)])

     [with-language-bindings language ([class-name name])

      #'(define-syntax-class class-name
          #:description description
          (pattern (~var _ class)))]]))

(define (compile-non-terminal language non-terminal)
  (-> language? non-terminal? syntax?)
  "compile NON-TERMINAL to syntax in context of LANGUAGE"

  (let* ([language-name (language-name language)]
         [name (non-terminal-name non-terminal)]
         [literals (non-terminal-literals non-terminal)]
         [datum-literals (non-terminal-datum-literals non-terminal)]

         [productions
          (for/list ([production (in-list
                                   (non-terminal-productions non-terminal))])
            (compile-production language non-terminal production))])

    [with-language-syntax language ([define-syntax-class 'define-syntax-class]
                                    [pattern 'pattern]
                                    [description
                                     (format "~a ~a" language-name name)])

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
            (pattern production)
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
           [with-language-syntax lang ([~var '~var]
                                       [%_ '_])
            [with-language-bindings lang ([ident ident])
             #'(~var %_ ident)]]))]

    [(p-literal? production)
     (language-introduce-datum lang (p-literal-ident production))]

    [(p-list? production)
     [datum->syntax (p-list-stx production)
      (for/list ([pattern (in-list (p-list-list production))])
        (compile-production lang non-terminal pattern))]]

    [(p-repeat? production)
     (p-repeat-stx production)]))

