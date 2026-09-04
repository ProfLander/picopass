#lang picopass

(module+ test
  (require racket/format
           racket/pretty

           rackunit

           threading

           picopass/rackunit)

  ;; L0: Lambda calculus with n-ary abs / app

  [define-language L0
   #:entry-point expr
   #:description "n-ary lambda"

   #:terminals ([ident id]
                [number number])

   (expr
     #:description "expression"
     #:datum-literals [begin abs app]
     ident
     number
     (begin ~cut expr ...)
     (abs ~cut (ident ...) expr)
     (app ~cut expr ...+))]

  (define-language-parser parse-L0 L0)

  (define l0-stx
    #'(begin (app (abs (x y) x) 1234 5678)))

  (test-case "parse-L0"
    (check-not-exn
      (thunk
        (parse-L0 l0-stx))))

  ;; L1: Lambda calculus with unary abs / app

  [define-language L1
   #:extends L0
   #:description "unary lambda"

   (expr
     (- (abs ~cut (ident ...) expr)
        (app ~cut expr ...+))
     (+ (abs ~cut ident expr)
        (app ~cut expr expr)))]

  (define-language-parser parse-L1 L1)

  (define l1-stx
    #'(begin (app (app (abs x (abs y x)) 1234) 5678)))

  (test-case "parse-L1"
    (check-not-exn
      (thunk
        (parse-L1 l1-stx))))

  ; test pass for value -> value

  [define-pass procedure-pass
   (-> string? number?)

   [string
    (-> string? number?)

    [str (string->number str)]]]

  (test-case "procedure-pass"
    (check-equal? (procedure-pass "1234")
                  1234))

  ;; from-structs: arbitrary record IR to L0 syntax

  (struct ir-ident [ident])
  (struct ir-number [num])
  (struct ir-begin [body])
  (struct ir-abs [args body])
  (struct ir-app [proc args])

  (define (ir-expr? val)
    (or (ir-ident? val)
        (ir-number? val)
        (ir-begin? val)
        (ir-abs? val)
        (ir-app? val)))

  (define (ir? val)
    (ir-expr? val))

  [define-pass from-structs
   (-> ir? L0)

   [expr
    (-> ir-expr? expr)

    [(ir-ident ident) ident]

    [(ir-number num) num]

    [(ir-begin (list (~rec body) ...))
     #`(begin #,@body)]

    [(ir-abs (list (~rec arg) ...)
             (~rec body))
     #`(abs (#,@arg) #,body)]

    [(ir-app (~rec proc)
             (list (~rec arg) ...))
     #`(app #,proc #,@arg)]]]

  (test-case "from-structs"
    (check-datum=?
      (from-structs
        (ir-begin
          (list (ir-app (ir-abs (list (ir-ident 'x) (ir-ident 'y))
                                (ir-ident 'x))
                        (list (ir-number 1234)
                              (ir-number 5678))))))
      l0-stx))

  ;; unary-lambda: L0 syntax to L1 syntax

  [define-pass unary-lambda
   (-> L0 L1)

   [expr-abs
    (-> expr expr)

    [(abs ~cut (arg:ident ...) (~rec body:expr))
     (for/fold ([acc (attribute body)])
               ([arg (in-list (reverse (attribute arg)))])
       #`(abs #,arg #,acc))]]

   [expr-app
    (-> expr expr)

    [(app ~cut (~rec arg:expr) ...+)
     (for/fold ([acc (car (attribute arg))])
               ([arg (in-list (cdr (attribute arg)))])
       #`(app #,acc #,arg))]]]

  (test-case "unary-lambda"
    (check-datum=? (unary-lambda #'(begin (app (abs (x y) x) 1234 5678)))
                   l1-stx))

  ;; to-source: L1 syntax to string source

  [define-pass to-source
   (-> L1 string?)

   [expr
    (-> expr string?)

    [ident:ident
     (~a (syntax-e #'ident))]

    [number:number
     (~a (syntax-e #'number))]

    [(begin ~cut (~rec body:expr) ...)
     (~a (cons 'begin
               (map syntax-e (attribute body))))]

    [(abs ~cut arg:ident (~rec body:expr))
     (format "(abs ~a ~a)" (syntax-e #'arg) (syntax-e #'body))]

    [(app ~cut (~rec proc:expr) (~rec arg:expr))
     (format "(app ~a ~a)" (syntax-e #'proc) (syntax-e #'arg))]]]

  (test-case "to-source"
    (check-equal? (~> #'(begin (app (abs (x y) x) 1234 5678))
                      (unary-lambda)
                      (to-source))
                  "(begin (app (app (abs x (abs y x)) 1234) 5678))")))

