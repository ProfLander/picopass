#lang picopass/impl

; Pattern IR
;
; Represents a match or syntax-parse pattern

(require racket/function
         racket/match

         syntax/strip-context

         picopass/syntax)

(provide (all-defined-out))

; Identifier
(struct p-ident [ident]
  #:methods gen:custom-write
  [(%define (write-proc self port mode)
            ((if mode write display)
             (let ([name (p-ident-name self)])
               (if mode
                   name
                   (list 'p-ident name)))
             port))])

(define (p-ident-name self)
  (-> p-ident? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (p-ident-ident self)))

; Literal
(struct p-literal [ident]
  #:methods gen:custom-write
  [(%define (write-proc self port mode)
            ((if mode write display)
             (let ([name (p-literal-name self)])
               (if mode
                   name
                   (list 'p-literal name)))
             port))])

(define (p-literal-name self)
  (-> p-literal? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (p-literal-ident self)))

; Keyword
(struct p-keyword [stx]
  #:methods gen:custom-write
  [(%define (write-proc self port mode)
            ((if mode write display)
             (let ([symbol (p-keyword-symbol self)])
               (if mode
                   symbol
                   (list 'p-keyword symbol)))
             port))])

(define (p-keyword-symbol self)
  (-> p-keyword? keyword?)
  #:trace #f
  (syntax-e (p-keyword-stx self)))

; List
(struct p-list [stx list]
  #:methods gen:custom-write
  [(%define (write-proc self port mode)
            ((if mode write display)
             (let ([lst (p-list-list self)])
               (if mode
                   lst
                   (cons 'p-list lst)))
             port))])

; Repetition (... / ...+)
(struct p-repeat [stx min]
  #:methods gen:custom-write
  [(%define (write-proc self port mode)
            ((if mode write display)
             (let ([min (p-repeat-min self)])
               (if mode
                   (case min
                     [(0) '...]
                     [(1) '...+])
                   (list 'p-repeat min)))
             port))])

(define (pattern? self)
  (-> any/c boolean?)
  #:trace #f
  "predicate identifying the implicit union type of patterns"

  (or (p-ident? self)
      (p-literal? self)
      (p-keyword? self)
      (p-list? self)
      (p-repeat? self)))

(define (pattern=? a b)
  (-> pattern? pattern? boolean?)
  #:trace #f
  "equality over patterns"

  (cond
    [(and (p-ident? a)
          (p-ident? b))
     (datum=? (p-ident-ident a)
              (p-ident-ident b))]

    [(and (p-literal? a)
          (p-literal? b))
     (datum=? (p-literal-ident a)
              (p-literal-ident b))]

    [(and (p-keyword? a)
          (p-keyword? b))
     (datum=? (p-keyword-stx a)
              (p-keyword-stx b))]

    [(and (p-list? a)
          (p-list? b))
     (let ([a (p-list-list a)]
           [b (p-list-list b)])
       (and (= (length a)
               (length b))
            (for/and ([a (in-list a)]
                      [b (in-list b)])
              (pattern=? a b))))]

    [(and (p-repeat? a)
          (p-repeat? b))
     (= (p-repeat-min a)
        (p-repeat-min b))]

    [else #f]))

(define (pattern-stx self)
  (-> pattern? syntax?)
  #:trace #f
  "return the underlying syntax of SELF"

  (cond
    [(p-ident? self) (p-ident-ident self)]
    [(p-literal? self) (p-literal-ident self)]
    [(p-keyword? self) (p-keyword-stx self)]
    [(p-list? self) (p-list-stx self)]
    [(p-repeat? self) (p-repeat-stx self)]
    [else (error "not a pattern:" self)]))

(define (pattern-replace-context self lctx)
  (-> pattern? syntax? pattern?)

  (define (replace stx)
    (-> syntax? syntax?)
    (replace-context lctx stx))

  (match self
    [(p-ident ident)
     (p-ident (replace ident))]
    [(p-literal ident)
     (p-literal (replace ident))]
    [(p-keyword keyword)
     (p-keyword (replace keyword))]
    [(p-list stx lst)
     (p-list (replace stx) (map (curryr pattern-replace-context lctx) lst))]
    [(p-repeat stx min)
     (p-repeat (replace stx) min)]))

(define (pattern->syntax self)
  (-> pattern? syntax?)
  (match self
    [(p-ident ident)
     (with-syntax ([ident #`(quote-syntax #,ident)])
       #'(p-ident ident))]
    [(p-literal ident)
     (with-syntax ([ident #`(quote-syntax #,ident)])
       #'(p-literal ident))]
    [(p-keyword keyword)
     (with-syntax ([keyword #`(quote-syntax #,keyword)])
       #'(p-keyword keyword))]
    [(p-list stx lst)
     (with-syntax ([stx #`(quote-syntax #,stx)]
                   [(pat ...) (map pattern->syntax lst)])
       #'(p-list stx (list pat ...)))]
    [(p-repeat stx min)
     (with-syntax ([stx #`(quote-syntax #,stx)]
                   [min min])
       #'(p-repeat stx min))]))

