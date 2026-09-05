#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class datum
  (pattern datum))
