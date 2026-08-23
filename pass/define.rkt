#lang racket/base

; Pass definition macro

(require (for-syntax racket/base
                     racket/pretty

                     picopass/logger

                     picopass/pass/parse
                     picopass/pass/normalize
                     picopass/pass/validate
                     picopass/pass/compile

                     picopass/pass/ir/pass)

         racket/match

         syntax/parse
         syntax/parse/define

         picopass/pass/error)

(provide (all-from-out racket/match)
         (all-from-out syntax/parse)
         (all-from-out picopass/pass/error)
         (all-defined-out))

[define-syntax-parser define-pass
 [pass:parse-pass

  (let* ([pass (attribute pass.struct)]
         [pass-name (pass-name pass)]
         [_ (log-picopass-info "parsed: ~a\n~a\n"
                               pass-name
                               (pretty-format pass
                                              #:mode 'write))]
         [pass (normalize-pass pass)])

    (log-picopass-info "define-pass: ~a:\n~a\n"
                       pass-name
                       (pretty-format pass
                                      #:mode 'write))

    (let* ([pass (validate-pass pass)]
           [stx (compile-pass pass)])
      (log-picopass-info "define-pass: ~a output:\n~a\n"
                         pass-name
                         (pretty-format (syntax->datum stx)
                                        #:mode 'write))
      stx))]]
