#lang picopass/impl

; Delta wrapper
;
; Represents removals and additions to the contents of a list

(require racket/string)

(provide (all-defined-out)
         (struct-out delta))

(struct delta [remove add]
  #:methods gen:custom-write
  [(%define (write-proc self port _mode)
            (display (append (list 'delta
                                   (cons 'remove (delta-remove self))
                                   (cons 'add (delta-add self))))
                     port))])

(define (make-delta #:remove removes 
                    #:add adds)
  (-> #:remove (listof any/c) #:add (listof any/c) delta?)
  "construct a delta from REMOVES and ADDS"

  (delta removes adds))

(define (map-delta f self)
  (-> (-> any/c any/c) delta? delta?)
  "produce a new delta by mapping F over the removals and additions in SELF"

  (delta (map f (delta-remove self))
         (map f (delta-add self))))

(define (raise-apply-delta-error target missing)
  (-> (listof any/c) (listof any/c) none/c)

  (error (string-join (list "missing values for delta removal:"
                            (format "  target: ~a" target)
                            (format "  missing: ~a" missing))
                      "\n")))

(define (apply-delta self
                     target
                     #:equal? [=? equal?]
                     #:on-missing [on-missing raise-apply-delta-error])
  (->* [delta? (listof any/c)]
       [#:equal? (-> any/c any/c boolean?)
        #:on-missing (-> (listof any/c) (listof any/c) any/c)]
       (or/c (listof any/c) any/c))
  "apply the removals and additions in SELF to TARGET and return the result,
   using =? to determine equality, deferring to ON-MISSING
   if specified removals are not present in TARGET"

  (let* ([add (delta-add self)]
         [remove (delta-remove self)]
         [missing (remove* target remove =?)])

    (if (pair? missing)
        (on-missing target missing)
        (append (remove* remove target =?) add))))

(module+ test
  (define del (make-delta #:remove '(1 6) #:add '(3 4)))
  (printf "del: ~a\n" del)

  (define tgt '(1 2 5 6))
  (printf "tgt: ~a\n" tgt)

  (define res (apply-delta del tgt))
  (printf "res: ~a\n" res))

