#lang info

(define collection "picopass")
(define deps '("base" "threading"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "A syntax-flavoured nanopass framework for Racket.")
(define version "0.1")
(define pkg-authors '(lander))
(define blurb '("Write compilers composed of several simple passes that operate over well-defined intermediate languages."))
(define categories '(utility))
(define compile-omit-paths '("tests"))

