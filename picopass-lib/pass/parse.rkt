#lang racket/base

; Pass parsing pipeline
;
; Parses definition syntax and produces pass IR

(require syntax/parse

         picopass/pattern/parse

         picopass/pass/ir/pass
         picopass/pass/ir/processor
         picopass/pass/ir/processor-clause)

(provide (all-defined-out))

(define-syntax-class parse-pass
  #:description "pass"
  #:datum-literals [-> *]
  (pattern (_ name:id
              (-> (~or * input:id)
                  (~or * output:id))
              processor:parse-processor
              ...+)
           #:do [(define introduce (make-syntax-introducer))
                 (define self-ref (introduce (datum->syntax #'name 'pass)))]
           #:attr struct (pass this-syntax
                               (attribute name)
                               (attribute input)
                               (attribute output)
                               (attribute processor.struct)
                               self-ref
                               introduce)))

(define-syntax-class parse-processor
  #:description "processor"
  #:datum-literals [: -> *]
  (pattern (name:id
             (-> (~or *
                      input:id)
                 (~or *
                      output:id))
             clause:parse-processor-clause
             ...+)
           #:attr struct (processor this-syntax
                                    (attribute name)
                                    (attribute input)
                                    #f
                                    (attribute output)
                                    #f
                                    (attribute clause.struct))))

(define-syntax-class parse-processor-clause
  #:description "processor clause"
  (pattern ((~var pat (parse-pattern processor-clause-literal?))
            body ...)
           #:attr struct (processor-clause this-syntax
                                           (attribute pat.struct)
                                           (attribute body))))

