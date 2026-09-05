#lang racket/base

(require racket/function
         rackunit)

(check-exn
  exn:fail:syntax?
  (thunk
    (expand
      #'(module duplicate-terminal picopass/lang
          (define-language duplicate-terminal
                           #:entry-point expr
                           #:terminals ([ident id]
                                        [ident id])
                           (expr ident))))))

