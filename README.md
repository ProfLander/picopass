Picopass
========

*Nanopass, expressed through Racket syntax objects.*

Picopass is an implementation of the Nanopass approach to compiler construction, designed around Racket syntax objects and built on Racket's existing pattern-matching facilities.

Status
------
Picopass is currently pre-1.0. Its core language and pass machinery are implemented; additional features such as input parameters and extra return values are planned for 1.0.

Documentation
-------------

Formal documentation is available on [the repository's GitHub Pages site](https://proflander.github.io/picopass/).

Example
-------

A language definition describes the syntax of an intermediate representation. Languages can be extended incrementally, and passes specify transformations between them.

The following example defines a lambda calculus with n-ary abstractions and applications, extends it with their unary forms, and defines a pass to perform the transformation.

```racket
#lang picopass

; Lambda calculus with n-ary abstraction and application
[define-language L0
 #:entry-point expr

 ; Define terminals as name + syntax-class pairs
 #:terminals ([ident id])

 ; Non-terminals reference terminals and literals
 ; to construct more complex expressions
 (expr
   #:datum-literals [abs app]
   ident
   (abs (ident ...) expr)
   (app expr ...+))]

; Define a parser for L0
(define-parser parse-L0 L0)

; Define some syntax in L0
(define l0-syntax #'(app (abs (x y) x) foo bar))

; Use the parser defined by L0 to validate it
(println (parse-L0 l0-syntax))

; Lambda calculus with unary abstraction and application
[define-language L1
 ; Languages can be defined by extension
 #:extends L0

 ; ...and add or remove specific forms
 (expr
   (- (abs (ident ...) expr)
      (app expr ...+))
   (+ (abs ident expr)
      (app expr expr)))]

; Define a parser for L1
(define-parser parse-L1 L1)

; Define some syntax in L1
(define l1-syntax #'(app (app (abs x (abs y x)) foo) bar))

; Use the parser defined by L1 to validate it
(println (parse-L1 l1-syntax))

; Transform from n-ary to unary
[define-pass unary-lambda
 (-> L0 L1)

 ; A processor from L0 expr to L1 expr
 [expr
  (-> expr expr)

  ; Processor clauses match input language forms and return output language forms
  [(abs ((~rec arg:ident) ...) (~rec body:expr))
   (for/fold ([acc (attribute body)])
             ([arg (in-list (reverse (attribute arg)))])
     #`(abs #,arg #,acc))]

  ; The (~rec ...) action pattern recursively applies the pass to the target form
  [(app (~rec arg:expr) ...+)
   (for/fold ([acc (car (attribute arg))])
             ([arg (in-list (cdr (attribute arg)))])
     #`(app #,acc #,arg))]]]

; Transform L0 syntax to L1 syntax
(println (unary-lambda l0-syntax))
```

Passes operate directly on Racket syntax objects, using Racket's existing pattern-matching facilities to inspect and construct terms.

They can therefore take syntax objects from, and return them to, Racket's broader syntax-processing ecosystem.

Core concepts
------------

### Languages

A language definition establishes the syntax of its terminals and non-terminals, and defines a `parse-<language name>` procedure for validating syntax against the language.

### Passes

A pass produces a syntax parser that validates its input against the source language, performs the transformation, and validates the result against the target language.

### Arbitrary values

Passes can also transform arbitrary Racket values. In this mode, predicates take the place of languages and non-terminals: they dispatch processors based on their inputs and assert that their outputs have the expected form, while `racket/match` provides the pattern matching that `syntax/parse` provides for syntax transformations.

Why syntax objects?
-------------------

Picopass was motivated in part by the practical difficulty of moving between Racket syntax objects and a separate IR representation.

### Syntax is already the representation

Working directly with syntax objects means compiler and macro transformations can operate on the same representation used by their surrounding Racket code, without requiring conversion to and from a separate IR.

### Syntax context is preserved

Because syntax objects retain their lexical context, including scopes, that information persists through transformations. Derived language definitions can therefore report errors that correspond directly to the original syntax.

### Racket syntax tooling

Since language and pass patterns are expressed using Racket's existing syntax tooling, Picopass can build directly on facilities like `syntax/parse` rather than introducing a separate pattern language.

Installation
------------

Clone the repository and either install via `make`:

```bash
make install
```

Or via `raco pkg install`:

```bash
raco pkg install --auto --link picopass-lib picopass-test picopass-doc picopass
```

Usage
-----

The core Picopass forms are provided by `picopass/base`:

```racket
#lang racket

(require picopass/base)
```

Picopass also provides a `picopass` language, which extends `racket` with the core Picopass bindings:

```racket
#lang picopass
```

Relationship to Nanopass
------------------------

Picopass is directly inspired by the Nanopass framework and follows the same general approach to compiler construction. It differs primarily in its representation: Picopass operates directly on Racket syntax objects, integrating with Racket's existing syntax machinery rather than introducing a separate IR representation.

The existing Nanopass implementation is a mature and substantially more feature-complete project. Picopass is an alternative approach motivated by tighter integration with Racket's syntax system, and is currently under active development.

For more information on Nanopass, see the [Nanopass Framework for Racket repository](https://github.com/nanopass/nanopass-framework-racket), the [Nanopass Framework website](https://nanopass.org/index.html), or the [Nanopass Framework for Racket documentation](https://docs.racket-lang.org/nanopass/index.html).

Contributing
------------

Picopass is currently under active development, primarily driven by its use in a private project. Contributions and feedback are welcome.
