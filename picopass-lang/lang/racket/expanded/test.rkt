#lang racket/base

(module+ test

  (require racket/function
           rackunit
           picopass/lang/racket/expanded/base)

  (define base-stx
    #'(module foo racket/base
        (printf "hello, world!")))

  (define expanded-stx
    (expand base-stx))

  (test-case "parse-racket"
    (check-not-exn
      (thunk
        (parse-racket expanded-stx)))))

