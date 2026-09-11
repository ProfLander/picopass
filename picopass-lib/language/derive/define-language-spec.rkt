#lang racket/base

; Language definition spec
;
; Provides an IR for constructing derived define-language forms

(require picopass/syntax 
         picopass/language/define)

(provide (all-defined-out))

(struct define-language-spec [name
                              entry-point
                              description
                              terminals
                              non-terminals]
  #:mutable
  #:transparent)

(define (make-define-language-spec name
                                   entry-point
                                   [description #f]
                                   [terminals null]
                                   [non-terminals null])
  (define-language-spec name
                        entry-point
                        description
                        terminals
                        non-terminals))

(define (define-language-spec-add-terminal! spec name class)
  (let ([terminals (define-language-spec-terminals spec)]
        [non-terminals (define-language-spec-non-terminals spec)])

    (unless (or (findf (λ (terminal)
                         (datum=? name
                                  (terminal-spec-name terminal)))
                       terminals)
                (findf (λ (non-terminal)
                         (datum=? name
                                  (non-terminal-spec-name non-terminal)))
                       non-terminals))

      [set-define-language-spec-terminals! spec
       (append terminals
               (list (terminal-spec name class)))])))

(define (define-language-spec-add-non-terminal! spec non-terminal)
  [set-define-language-spec-non-terminals! spec
   (append (define-language-spec-non-terminals spec)
           (list non-terminal))])

(define (define-language-spec->syntax spec)

  (let ([description (define-language-spec-description spec)]
        [terminals (define-language-spec-terminals spec)]
        [non-terminals (define-language-spec-non-terminals spec)])

    (with-syntax ([name
                   (define-language-spec-name spec)]

                  [entry-point
                   (define-language-spec-entry-point spec)]

                  [(description ...)
                   (if description
                       #`(#:description #,description)
                       #`())]

                  [(terminal ...)
                   (map terminal-spec->syntax terminals)]

                  [(non-terminal ...)
                   (map non-terminal-spec->syntax non-terminals)])

      #'[define-language name
         #:entry-point entry-point
         description ...
         #:terminals [terminal ...]
         non-terminal ...])))

(struct terminal-spec [name class]
  #:transparent)

(define (terminal-spec->syntax terminal)
  (with-syntax ([name (terminal-spec-name terminal)]
                [class (terminal-spec-class terminal)])
    #'(name class)))

(struct non-terminal-spec [name
                           description
                           literals
                           datum-literals
                           productions]
  #:mutable)

(define (make-non-terminal-spec name
                                [description #f]
                                [literals null]
                                [datum-literals null]
                                [productions null])
  (non-terminal-spec name
                     description
                     literals
                     datum-literals
                     productions))

(define (non-terminal-spec-add-literal! spec literal)
  [set-non-terminal-spec-literals! spec
   (append (non-terminal-spec-literals spec)
           (list literal))])

(define (non-terminal-spec-add-datum-literal! spec datum-literal)
  [set-non-terminal-spec-datum-literals! spec
   (append (non-terminal-spec-datum-literals spec)
           (list datum-literal))])

(define (non-terminal-spec-add-production! spec production)
  [set-non-terminal-spec-productions! spec
   (append (non-terminal-spec-productions spec)
           (list production))])

(define (non-terminal-spec->syntax spec)

  (let ([description (non-terminal-spec-description spec)]
        [literals (non-terminal-spec-literals spec)]
        [datum-literals (non-terminal-spec-datum-literals spec)])

    (with-syntax ([name
                   (non-terminal-spec-name spec)]

                  [(description ...)
                   (if description
                       #'(#:description #,description)
                       #'())]

                  [(literals ...)
                   (if (pair? literals)
                       #`(#:literals #,literals)
                       #'())]

                  [(datum-literals ...)
                   (if (pair? datum-literals)
                       #`(#:datum-literals #,datum-literals)
                       #'())]

                  [(production ...)
                   (non-terminal-spec-productions spec)])

      #'(name
          description ...
          literals ...
          datum-literals ...
          production ...))))

