#lang picopass

(require racket/format
         racket/pretty

         threading)

;; L0: Lambda calculus with n-ary abs / app

[define-language L0
 #:entry-point expr

 #:terminals ([ident id]
              [number number])

 (expr
   #:datum-literals [begin abs app]
   ident
   number
   (begin expr ...)
   (abs (ident ...) expr)
   (app expr ...+))]

(printf "parsed L0: ~a\n"
        (~> #'(begin (app (abs (x y) x) 1234 5678))
            (parse-L0)
            (syntax->datum)
            (pretty-format)))

;; L1: Lambda calculus with unary abs / app

[define-language L1
 #:extends L0

 (expr
   (- (abs (ident ...) expr)
      (app expr ...+))
   (+ (abs ident expr)
      (app expr expr)))]

(printf "parsed L1: ~a\n"
        (~> #'(begin (app (app (abs x (abs y x)) 1234) 5678))
            (parse-L1)
            (syntax->datum)
            (pretty-format)))

; test pass for value -> value

[define-pass procedure-pass
 (-> string? number?)

 [string
  (-> string? number?)

  [str (string->number str)]]]

(printf "procedure-pass: ~a\n" (procedure-pass "1234"))

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

(printf "structs to L0: ~a\n"
        (~> (ir-begin
              (list (ir-app (ir-abs (list (ir-ident 'x) (ir-ident 'y))
                                    (ir-ident 'x))
                            (list (ir-number 1234)
                                  (ir-number 5678)))))
            (from-structs)
            (syntax->datum)
            (pretty-format)))

;; unary-lambda: L0 syntax to L1 syntax

[define-pass unary-lambda
 (-> L0 L1)

 [expr-abs
  (-> expr expr)

  [(abs ((~rec arg:ident) ...) (~rec body:expr))
   (for/fold ([acc (attribute body)])
             ([arg (in-list (reverse (attribute arg)))])
     #`(abs #,arg #,acc))]]

 [expr-app
  (-> expr expr)

  [(app (~rec arg:expr) ...+)
   (for/fold ([acc (car (attribute arg))])
             ([arg (in-list (cdr (attribute arg)))])
     #`(app #,acc #,arg))]]]

(printf "L0 to L1: ~a\n"
        (~> #'(begin (app (abs (x y) x) 1234 5678))
            (unary-lambda)
            (syntax->datum)
            (pretty-format)))

;; to-source: L1 syntax to string source

[define-pass to-source
 (-> L1 string?)

 [expr
  (-> expr string?)

  [ident:ident
   (~a (syntax-e #'ident))]

  [number:number
   (~a (syntax-e #'number))]

  [(begin (~rec body:expr) ...)
   (~a (cons 'begin
             (map syntax-e (attribute body))))]

  [(abs arg:ident (~rec body:expr))
   (format "(abs ~a ~a)" (syntax-e #'arg) (syntax-e #'body))]

  [(app (~rec proc:expr) (~rec arg:expr))
   (format "(app ~a ~a)" (syntax-e #'proc) (syntax-e #'arg))]]]

(printf "L0 to source: ~a\n"
        (~> #'(begin (app (abs (x y) x) 1234 5678))
            (unary-lambda)
            (to-source)
            (pretty-format)))

