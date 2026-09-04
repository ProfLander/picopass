#lang racket/base

(require rackunit
         picopass/syntax)

(provide (all-defined-out))

(define-binary-check (check-datum=? datum=? actual expected))

