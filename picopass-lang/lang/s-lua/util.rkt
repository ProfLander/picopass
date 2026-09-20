#lang racket/base

(require syntax/parse
         picopass/lang/s-lua/language)

(provide (all-defined-out))

(define (splice-requires requires body)

  (define module-requires
    (for/list ([pair (in-hash-keys requires)])
      (list (string->symbol (cdr pair)) (car pair))))

  (define member-requires
    (for/list ([(pair reqs) (in-hash requires)])
      (let ([name (string->symbol (cdr pair))])
        (map (lambda (req)
               (list name req))
             reqs))))

  (define module-binding
    (if (pair? module-requires)
        (with-syntax ([([require-target require-target-str] ...)
                       module-requires])
          #'[(#%local [require-target ...]
                      [(require require-target-str) ...])])
        #'[]))

  (define member-binding
    (if (pair? member-requires)
        (with-syntax ([([[require-id-target require-id] ...] ...)
                       member-requires])
          #'[(#%local [require-id ... ...]
                      [(#%member require-id-target require-id) ... ...])])
        #'[]))

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block)
       body ...))
     (with-syntax
       ([(module-binding ...) module-binding]
        [(member-binding ...) member-binding])

       #'(#%chunk
          (#%block
           module-binding ...
           member-binding ...
           body ...)))]))

(define (splice-provides provides body)
  "Splice PROVIDES into BODY as a return table"

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block)
       body ...))
     (with-syntax
       ([(module-provide ...) provides])

       #'(#%chunk
          (#%block

           body ...

           (#%return (#%table
                      [module-provide module-provide]
                      ...)))))]))

(define (make-package-preloader name body)
  "Return an S-Lua statement that injects BODY into package.preload
   as a module with NAME"

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block) body ...))
     #`(#%assign [(#%member (#%member package preload)
                            #,name)]
                 [(#%function (#%vararg)
                              (#%block
                               body ...))])]))

