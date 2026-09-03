#lang info

(define collection 'multi)

(define deps '("base"
               "picopass-lib"
               "picopass-doc"
               "picopass-test"
               "picopass-lang"))

(define implies '("picopass-lib"
                  "picopass-doc"
                  "picopass-test"
                  "picopass-lang"))

(define pkg-desc "A syntax-flavoured nanopass framework for Racket.")
(define version "0.0")
(define pkg-authors '(lander))
(define blurb '("Write compilers composed of several simple passes that operate over well-defined intermediate languages."))
(define categories '(utility))

