#lang racket/base

; Language definition macro

(require (for-syntax racket/base
                     racket/function
                     racket/pretty

                     picopass/logger

                     picopass/language/parse
                     picopass/language/normalize
                     picopass/language/validate
                     picopass/language/compile

                     picopass/language/ir/language
                     picopass/language/ir/language-delta)

         syntax/parse
         syntax/parse/define)

(provide (all-from-out syntax/parse)
         (all-defined-out))

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
                                        #:mode 'write))
      stx))]

 ; Define a named language extending another,
 ; with a set of removals and additions over terminals / non-terminals,
 ; and a potentially-rebound entry point
 [delta:parse-language-delta

  (let ([base [syntax-local-value #'delta.extends
               (thunk
                 [raise-syntax-error 'define-language
                  "unbound base language"
                  this-syntax
                  #'delta.extends])]])

    (unless (language? base)
      [raise-syntax-error 'define-language
       "bound identifier is not a language"
       this-syntax
       #'delta.extends])

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
