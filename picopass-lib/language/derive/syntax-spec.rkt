#lang racket/base

; Language derivation backend for syntax-spec
;
; Parses syntax-spec-v3 syntax and produces a corresponding
; define-language form

(require syntax/parse

         picopass/syntax
         picopass/language/derive/define-language-spec)

(provide (all-defined-out))

(define (syntax-spec->define-language-spec define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [syntax-spec]
    [(syntax-spec spec-def ...)

     (for ([spec-def (in-list (attribute spec-def))])
       (gather-binding-classes define-language-spec spec-def))

     (for ([spec-def (in-list (attribute spec-def))])
       (gather-nonterminals define-language-spec spec-def))

     (for ([spec-def (in-list (attribute spec-def))])
       (gather-productions define-language-spec spec-def))

     (define-language-spec->syntax define-language-spec)]))

(define (gather-binding-classes define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [binding-class]

    [(binding-class name:id option ...)
     (binding-class->define-language-spec define-language-spec
                                          #'name
                                          #'(option ...))]

    [_ (void)]))

(define (gather-nonterminals define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [nonterminal
                      nonterminal/nesting
                      nonterminal/exporting]

    [((~and (~or (~seq nonterminal name:id)
                 (~seq nonterminal/nesting name:id (_))
                 (~seq nonterminal/exporting name:id)))
      _ ...)

     [define-language-spec-add-non-terminal! define-language-spec
      (make-non-terminal-spec #'name)]]

    [_ (void)]))

(define (gather-productions define-language-spec stx)
  (syntax-parse stx
    #:datum-literals [nonterminal
                      nonterminal/nesting
                      nonterminal/exporting]

    [((~and (~or (~seq nonterminal name:id)
                 (~seq nonterminal/nesting name:id (_))
                 (~seq nonterminal/exporting name:id)))
      production ...)
     (let ([non-terminal-spec
            (findf (λ (non-terminal)
                     (datum=? (non-terminal-spec-name non-terminal)
                              #'name))
                   (define-language-spec-non-terminals define-language-spec))])
       (nonterminal->define-language-spec define-language-spec
                                          non-terminal-spec
                                          #'name
                                          #'(production ...)))]

    [_ (void)]))

(define (binding-class->define-language-spec define-language-spec
                                             name
                                             options)
  (syntax-parse options
    [((~alt (~optional (~seq #:description _))
            (~optional (~seq #:binding-space _))
            (~optional (~seq #:reference-compiler _)))
      ...)
     (define-language-spec-add-terminal! define-language-spec
                                         name
                                         #'id)]))

(define (nonterminal->define-language-spec define-language-spec
                                           non-terminal-spec
                                           _name
                                           productions)
  (syntax-parse productions
    #:datum-literals [~>]

    ([(~alt (~optional (~seq #:description _))
            (~optional (~seq #:allow-extension _))
            (~optional (~seq #:binding-space _)))
      ...
      (~or (~seq #:binding _)
           (~> _ ...)
           production)
      ...]

     (for/list ([production (in-list (attribute production))])
       (production->non-terminal-spec define-language-spec
                                      non-terminal-spec
                                      production)))))

(define-syntax-class (syntax-spec define-language-spec
                                  non-terminal-spec)
  (pattern ()
           #:with picopass this-syntax)

  (pattern kw:keyword
           #:with picopass this-syntax)

  (pattern (~datum ...)
           #:with picopass this-syntax)

  (pattern (~datum ...+)
           #:with picopass this-syntax)

  (pattern ((~datum ~literal) id (~optional (~seq #:space _)))
           #:do [(non-terminal-spec-add-literal! non-terminal-spec
                                                 #'id)]
           #:with picopass #'id)

  (pattern ((~datum ~datum) id)
           #:do [(non-terminal-spec-add-datum-literal! non-terminal-spec
                                                       #'id)]
           #:with picopass #'id)

  (pattern ((~var spec (syntax-spec define-language-spec
                                    non-terminal-spec))
            ...)
           #:with picopass #'(spec.picopass ...))

  (pattern ic:ident+class
           #:do [(define-language-spec-add-terminal! define-language-spec
                                                     #'ic.class
                                                     #'ic.class)]
           #:with picopass #'ic.class))

(define (production->non-terminal-spec define-language-spec
                                       non-terminal-spec
                                       stx)
  (syntax-parse stx

    [(~var stx-spec (syntax-spec define-language-spec
                                 non-terminal-spec))

     [non-terminal-spec-add-production! non-terminal-spec
      #'stx-spec.picopass]]

    [(ident:id (~var stx-spec (syntax-spec define-language-spec
                                           non-terminal-spec))
               ...)

     [non-terminal-spec-add-datum-literal! non-terminal-spec
      #'ident]

     [non-terminal-spec-add-production! non-terminal-spec
      #'(ident stx-spec.picopass ...)]]

    [ident:id

     [non-terminal-spec-add-datum-literal! non-terminal-spec
      #'ident]

     [non-terminal-spec-add-production! non-terminal-spec
      #'ident]]))

