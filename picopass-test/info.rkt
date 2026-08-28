#lang info

(define collection "picopass")
(define deps '("base" "picopass-lib"))
(define build-deps '("rackunit-lib"))
(define compile-omit-paths '("tests"))
(define clean '("compiled" "tests/compiled"))
