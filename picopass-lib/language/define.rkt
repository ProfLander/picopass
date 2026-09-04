#lang racket/base

; Language definition macro

(require (for-syntax racket/base
                     racket/function
                     racket/pretty

                     picopass/logger
                     picopass/syntax

                     picopass/language/parse
                     picopass/language/normalize
                     picopass/language/validate
                     picopass/language/compile

                     picopass/language/ir/language
                     picopass/language/ir/language-delta
                     picopass/language/ir/non-terminal)

         syntax/parse
         syntax/parse/define

         picopass/logger)

(provide (all-from-out syntax/parse)
         (all-from-out picopass/logger)
         (all-defined-out))

(define-for-syntax (syntax-local-language lctx ident)
  (let ([language [syntax-local-value ident
                   (thunk
                     [raise-syntax-error 'define-language
                      (format "unbound language ~a" (syntax-e ident))
                      lctx
                      ident])]])

    (unless (language? language)
      [raise-syntax-error 'define-language
       (format "~a is not a language" (syntax-e ident))
       lctx
       #'delta.extends])

    language))

[define-syntax-parser define-language
 ; Define a named language with a given set of terminals,
 ; non-terminals, and specific entry point non-terminal
 [language:parse-language

  (let* ([lang (attribute language.struct)]
         [lang (normalize-language lang)]

         [lang-name (language-name lang)])

    (log-picopass-info "define-language: ~a:\n~a\n"
                       lang-name
                       (pretty-format lang))

    (let* ([lang (validate-language lang)]
           [stx (compile-language lang)])
      (log-picopass-info "define-language: ~a output:\n~a\n"
                         lang-name
                         (pretty-format (syntax->datum stx)
                                        #:mode 'print))
      stx))]

 ; Define a named language extending another,
 ; with a set of removals and additions over terminals / non-terminals,
 ; and a potentially-rebound entry point
 [delta:parse-language-delta

  (let ([base (syntax-local-language this-syntax #'delta.extends)])

    (let* ([delta (attribute delta.struct)]
           [delta-name (language-delta-name delta)]
           [lang (extend-language base delta)]
           [lang (normalize-language lang)])

      (log-picopass-info "define-language/extend: ~a:\n~a\n"
                         delta-name
                         (pretty-format lang))

      (let* ([lang (validate-language lang)]
             [stx (compile-language lang)])
        (log-picopass-info "define-language/extend: ~a output:\n~a\n"
                           delta-name
                           (pretty-format (syntax->datum stx)
                                          #:mode 'write))
        stx)))]]

[define-syntax-parser define-language-parser
 [(_ name:id language:id)

  (define lang (syntax-local-language this-syntax #'language))

  (let* ([stx (compile-language-parser #'name lang)])
    (log-picopass-info "define-language-parser ~a output:\n~a\n"
                       (language-name lang)
                       (pretty-format (syntax->datum stx)
                                      #:mode 'write))
    stx)]]

[define-syntax-parser define-language-classes
 [(_ language:id [name:id class:id] ...)

  (let* ([lang (syntax-local-language this-syntax #'language)]
         [non-terminals (language-non-terminals lang)]
         [non-terminal-idents
          (for/list ([non-terminal (in-list non-terminals)])
            (non-terminal-ident non-terminal))])

    [with-syntax ([(class ...)
                   (for/list ([class (in-list (attribute class))])

                     (unless (member class non-terminal-idents datum=?)
                       [raise-syntax-error 'define-language-classes
                        (format "~a does not name a non-terminal in ~a"
                                (syntax-e class)
                                (language-name lang))])

                     #`(quote-syntax #,(language-introduce lang class)))])

      (let ([stx #'(begin
                     (define-syntax name
                       (make-rename-transformer class))
                     ...)])

        (log-picopass-info "define-language-classes ~a output:\n~a\n"
                           (language-name lang)
                           (pretty-format (syntax->datum stx)
                                          #:mode 'write))

        stx)])]]

