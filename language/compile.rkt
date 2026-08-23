#lang picopass/impl

; Language compilation pipeline

(require racket/function
         racket/syntax

         threading

         picopass/syntax

         picopass/pattern/ir

         picopass/language/ir/language
         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out))

(define (compile-language self)
  (-> language? syntax?)
  "compile SELF to syntax"
  [with-language-syntax self ([begin 'begin]
                              [define 'define]
                              [define-syntax 'define-syntax]
                              [syntax-parser 'syntax-parser]
                              [~var '~var]
                              [this-syntax 'this-syntax])

   [with-language-bindings self ([entry-point
                                  (language-entry-point-ident self)])
                           
    (with-syntax* ([ident (language-ident self)]

                   [(terminal ...)
                    (map (curryr compile-terminal self)
                         (language-terminals self))]

                   [(non-terminal ...)
                    (map (curryr compile-non-terminal self)
                         (language-non-terminals self))]

                   [parse-language
                    (format-id (language-context self)
                               "parse-~a"
                               #'ident)])

      #`(begin
          (define-syntax ident
            #,self)
          terminal
          ...
          non-terminal
          ...
          (define parse-language
            (syntax-parser
              [(~var _ entry-point) this-syntax]))))]])

(define (compile-terminal self lang)
  (-> terminal? language? syntax?)
  "compile SELF to syntax in context of LANG"

  (let* ([name (terminal-name self)]
         [description (format "~a ~a" (language-name lang) name)])

    [with-language-syntax lang ([define-syntax-class 'define-syntax-class]
                                [pattern 'pattern]
                                [~var '~var]
                                [description description]
                                [class (terminal-class self)])

     [with-language-bindings lang ([class-name name])

      #'(define-syntax-class class-name
          #:description description
          (pattern (~var _ class)))]]))

(define (compile-non-terminal self lang)
  (-> non-terminal? language? syntax?)
  "compile SELF to syntax in context of LANG"

  (let* ([language-name (language-name lang)]
         [name (non-terminal-name self)]
         [literals (non-terminal-literals self)]
         [datum-literals (non-terminal-datum-literals self)]
         [productions (map (curryr compile-production lang self)
                           (non-terminal-productions self))])

    [with-language-syntax lang ([define-syntax-class 'define-syntax-class]
                                [pattern 'pattern]
                                [description
                                 (format "~a ~a" language-name name)])

     [with-language-bindings lang ([class-name name])

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

(define (compile-production self lang non-terminal)
  (-> pattern? language? non-terminal? syntax?)
  "compile SELF to syntax in context of LANG and NON-TERMINAL"

  (cond
    [(p-ident? self)
     (let ([ident (p-ident-ident self)]
           [literals (non-terminal-literals non-terminal)]
           [datum-literals (non-terminal-datum-literals non-terminal)])
       (if (or (member ident literals datum=?)
               (member ident datum-literals datum=?))
           ident
           [with-language-syntax lang ([~var '~var]
                                       [%_ '_])
            [with-language-bindings lang ([ident ident])
             #'(~var %_ ident)]]))]

    [(p-literal? self)
     (language-introduce-datum lang (p-literal-ident self))]

    [(p-list? self)
     [datum->syntax (p-list-stx self) 
      (map (curryr compile-production lang non-terminal) 
           (p-list-list self))]]

    [(p-repeat? self)
     (p-repeat-stx self)]))

