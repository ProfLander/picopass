#lang racket/base

(module+ test
  (require racket/function
           rackunit
           picopass/base)

  [define-language test
   #:entry-point top-level
   #:terminals [id
                number]
   (top-level
     id
     number)]

  (define-language-parser parse-test test)

  (test-case "parse-test ident"
    (check-not-exn
      (thunk
        (parse-test #'foo))))

  (test-case "parse-test number"
    (check-not-exn
      (thunk
        (parse-test #'1234))))

  [define-pass test->syntax
   (-> test syntax?)]

  (test-case "test->syntax ident"
    (check-not-exn
      (thunk
        (test->syntax #'foo))))

  (test-case "test->syntax number"
    (check-not-exn
      (thunk
        (test->syntax #'1234)))))

