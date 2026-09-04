#lang picopass

(module+ test

  (require rackunit
           picopass/rackunit)

  [define-language improper
   #:entry-point expr
   #:terminals ([id id])
   (expr
     (id ... . id))]

  (define-language-parser parse-improper improper)

  (define improper-stx
    #'(foo bar baz . zap))

  (test-case "parse-improper"
    (check-not-exn
      (thunk
        (parse-improper improper-stx))))

  [define-language proper
   #:entry-point expr
   #:terminals ([id id])
   (expr
     (id ...))]

  (define-language-parser parse-proper proper)

  (define proper-stx #'(foo bar baz zap))

  (test-case "parse-proper"
    (check-not-exn
      (thunk
        (parse-proper proper-stx))))

  [define-pass improper->proper
   (-> improper proper)
   (expr
     (-> expr expr)
     [(lst:id ... . tail:id)
      #'(lst ... tail)])]

  (test-case "improper->proper"
    (check-datum=? (improper->proper improper-stx)
                   proper-stx)))

