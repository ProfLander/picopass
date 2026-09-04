#lang picopass/impl

; Processor clause IR
;
; Represents a transformation from a form in the input language
; to a form in the output language

(require racket/match

         syntax/parse

         picopass/syntax
         picopass/pattern/ir)

(provide (all-defined-out)
         (struct-out processor-clause))

(struct processor-clause [stx
                          pattern
                          body]
  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (list 'processor-clause
                           (list 'pattern
                                 (processor-clause-pattern self))
                           (cons 'body (map syntax->datum
                                            (processor-clause-body self))))
                     port))])

(define (processor-clause-literal? stx)
  (-> syntax? boolean?)
  #:trace #f
  "pattern literal recognition predicate"

  (or (datum=? #'~rec stx) 
      (datum=? #'~maybe stx)
      (datum=? #'~cut stx)))

(define (processor-clause-pattern->non-terminal-pattern pat)
  (-> pattern? pattern?)
  #:trace #f
  "convert the processor clause pattern PAT to a language non-terminal pattern"

  (match pat
    [(p-list _stx (list (p-literal lit) ident) #f)
     #:when (datum=? lit #'~rec)
     (processor-clause-pattern->non-terminal-pattern ident)]
    [(p-list stx lst tail)
     (p-list stx 
             (map processor-clause-pattern->non-terminal-pattern lst)
             (and tail
                  (processor-clause-pattern->non-terminal-pattern tail)))]
    [(p-ident ident)
     (syntax-parse ident
       [ic:ident+class
        (p-ident #'ic.class)]
       [_
        pat])]
    [(p-literal ident)
     (p-literal ident)]
    [(p-keyword ident)
     (p-keyword ident)]
    [(p-repeat stx min)
     (p-repeat stx min)]))

