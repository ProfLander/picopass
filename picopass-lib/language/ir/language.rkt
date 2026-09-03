#lang picopass/impl

; Language IR
;
; Represents a named language with atomic terminal forms,
; non-terminal productions, an entrypoint, and a private definition scope

(require (for-syntax racket/base
                     syntax/parse)

         picopass/language/ir/terminal
         picopass/language/ir/non-terminal)

(provide (all-defined-out)
         (struct-out language))

(struct language [stx
                  ident
                  entry-point-ident
                  description
                  terminals
                  non-terminals
                  scope-key
                  scope]
  #:methods gen:custom-write
  ([%define (write-proc self port _mode)
    (display (list 'language
                   (list 'name (syntax->datum (language-ident self)))
                   (list 'entry-point
                         (let ([entry-point
                                (language-entry-point-ident self)])
                           (and entry-point
                                (syntax->datum entry-point))))
                   (list 'description (language-description self))
                   (cons 'terminals (language-terminals self))
                   (cons 'non-terminals (language-non-terminals self))
                   (cons 'scope-key (language-scope-key self)))
             port)]))

(define (language-name self)
  (-> language? symbol?)
  #:trace #f
  "return the symbolic name of SELF"

  (syntax-e (language-ident self)))

(define (language-context self)
  (-> language? syntax?)
  #:trace #f
  "return the syntactic context of SELF"

  (language-ident self))

(define (language-introduce self stx)
  (-> language? syntax? syntax?)
  #:trace #f
  "add the private scope of SELF to the context of STX"

  ((language-scope self) stx))

(define (make-language-scope-key name)
  (-> (or/c string? symbol?) symbol?)
  (string->symbol
    (format "picopass:language:~a:~a"
            name (gensym))))

(define (language->syntax self)
  (-> language? syntax?)
  (with-syntax ([stx #`(quote-syntax #,(language-stx self))]
                [ident (language-ident self)]
                [entry-point-ident (language-entry-point-ident self)]
                [description (language-description self)]
                [(terminal ...) (map terminal->syntax
                                     (language-terminals self))]
                [(non-terminal ...) (map non-terminal->syntax
                                         (language-non-terminals self))]
                [scope-key (language-scope-key self)])
    #`(language stx
                #'ident
                #'entry-point-ident
                description
                (list terminal ...)
                (list non-terminal ...)
                'scope-key
                (make-interned-syntax-introducer 'scope-key))))

