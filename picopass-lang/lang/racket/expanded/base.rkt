#lang racket/base

(require picopass/base
         picopass/lang/racket/expanded/define)

(provide (all-defined-out))

(define-expanded-racket-language racket)
(define-language-parser parse-racket racket)

