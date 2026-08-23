#lang racket/base

; Picopass logger

(provide picopass-logger
         log-picopass-debug
         log-picopass-info
         log-picopass-warning
         log-picopass-error
         log-picopass-fatal)

(define-logger picopass)

