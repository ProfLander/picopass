#lang picopass

(require (for-syntax racket/syntax)

         syntax/parse/define

         picopass/lang/racket/datum
         picopass/lang/racket/base/module-path
         picopass/lang/racket/expanded/require
         picopass/lang/racket/expanded/provide)

(provide (all-defined-out))

[define-syntax-parser define-expanded-racket-language
 [(_ name:id
     (~alt (~optional (~seq #:expr name/expr:id)
                      #:defaults ([name/expr (format-id #'name "~a/expr" #'name)]))
           (~optional (~seq #:expr-class expr-class:id)
                      #:defaults ([expr-class #'expr-class])))
     ...)

  (with-syntax ([ooo (quote-syntax ...)] 
                [ooo+ (quote-syntax ...+)])

    #'(begin
        (define-splicing-syntax-class declaration-keyword
          (pattern #:cross-phase-persistent)
          (pattern #:empty-namespace)
          (pattern #:require=define)
          (pattern #:flatten-requires)
          (pattern #:unlimited-compile)
          (pattern #:unsafe)
          (pattern (~seq #:realm realm:id)))

        (define-syntax-class declare
          (pattern (#%declare declaration-keyword:declaration-keyword ooo)))

        [define-language name/expr
         #:entry-point expr
         #:terminals ([id id]
                      [datum datum])

         (expr
           #:literals [#%plain-lambda
                       case-lambda
                        if
                        begin
                        begin0
                        let-values
                        letrec-values
                        set!
                        quote
                        quote-syntax
                        with-continuation-mark
                        #%plain-app
                        #%top
                        #%variable-reference
                        #%foreign-inline]

           id
           (#%plain-lambda formals expr ooo+)
           (case-lambda (formals expr ooo+) ooo)
           (if expr expr expr)
           (begin expr ooo+)
           (begin0 expr expr ooo)
           (let-values ([(id ooo) expr] ooo))
           (letrec-values ([(id ooo) expr] ooo))
           (set! id expr)
           (quote datum)
           (quote-syntax datum)
           (quote-syntax datum #:local)
           (with-continuation-mark expr expr expr)
           (#%plain-app expr ooo+)
           (#%top . id)
           (#%variable-reference id)
           (#%variable-reference (#%top . id))
           (#%variable-reference)
           (#%foreign-inline datum maybe-mode))

         (maybe-mode
           #:effect
           #:pure
           #:pure*
           #:copy
           #:copy*)

         (formals
           (id ooo)
           (id ooo+ . id)
           id)]

        (define-language-classes name/expr [expr-class expr])

        [define-language name
         #:entry-point top-level-form
         #:terminals ([id id]
                      [module-path module-path-class]
                      [raw-require-spec raw-require-spec-class]
                      [raw-provide-spec raw-provide-spec-class]
                      [datum datum]
                      [expr expr-class]
                      [declare declare])

         (top-level-form
           #:literals [#%expression
                       module
                       #%plain-module-begin
                       begin
                       begin-for-syntax]

           general-top-level-form
           (#%expression expr)
           (module id module-path
             (#%plain-module-begin
               module-level-form ooo))
           (begin top-level-form ooo)
           (begin-for-syntax top-level-form ooo))

         (module-level-form
           #:literals [#%provide
                       begin-for-syntax
                       #%declare]

           general-top-level-form
           (#%provide raw-provide-spec ooo)
           (begin-for-syntax module-level-form ooo)
           submodule-form
           declare)

         (submodule-form
           #:literals [module
                       module*
                       #%plain-module-begin]

           (module id module-path
             (#%plain-module-begin
               module-level-form ooo))
           (module* id module-path
             (#%plain-module-begin
               module-level-form ooo))
           (module* id #f
             (#%plain-module-begin
               module-level-form ooo)))

         (general-top-level-form
           #:literals [define-values
                       define-syntaxes
                        #%require]

           expr
           (define-values (id ooo) expr)
           (define-syntaxes (id ooo) expr)
           (#%require raw-require-spec))]))]]

