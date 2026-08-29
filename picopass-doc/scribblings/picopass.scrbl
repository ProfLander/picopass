#lang scribble/manual

@require[picopass/base
         @for-label[@only-in[picopass/base define-pass
                                           define-language]]]

@title{Picopass}

@declare-exporting[picopass/base picopass #:use-sources (picopass/base)]

@defmodule*/no-declare[(picopass/base)]{
  The @racketmodname[picopass/base] library provides the core Picopass forms.
}

@defmodulelang*/no-declare[(picopass)]{
  The @racketmodname[picopass] language provides everything
  in @racketmodname[picopass/base], as well as @racketmodname[racket],
  making it suitable for use as a language.
}

@section{Overview}

Picopass is an implementation of the Nanopass approach to compiler
construction, designed around Racket syntax objects and built on Racket's
existing pattern-matching facilities.

A language definition describes the syntax of an intermediate representation.
Languages can be extended incrementally, and passes specify transformations
between them.

Passes operate directly on Racket syntax objects. A pass validates its input
against its source language, performs its transformation, and validates the
result against its target language.

Passes can also transform arbitrary Racket values. In this mode, predicates
take the place of languages and non-terminals: they dispatch processors based
on their inputs and assert that their outputs have the expected form, while
@racketmodname[racket/match] provides the pattern matching that
@racketmodname[syntax/parse] provides for syntax transformations.

Because Picopass operates directly on syntax objects, compiler and macro
transformations can work with the same representation used by surrounding
Racket syntax-processing tools. Syntax objects also preserve lexical context,
including scopes, through transformations.

@section{The @racket[define-language] Form}

The @racket[define-language] form establishes the syntax of a language in
terms of terminals and non-terminals, and defines a parser named
@racketidfont{parse-}@racket[language-id] for validating syntax against the
language.

@defform[#:literals (+ -)

         (define-language language-name
           language-clause)

         #:grammar [(language-clause language
                                     language-delta)

                    (language (code:line
                                #:entry-point non-terminal-ident
                                [#:terminals (terminal ...)]
                                non-terminal ...+))

                    (language-delta (code:line
                                      #:extends extends-ident
                                      [#:entry-point non-terminal-ident]
                                      [#:terminals- (terminal ...+)]
                                      [#:terminals+ (terminal ...+)]
                                      non-terminal-delta ...))

                    (terminal (name syntax-class))

                    (non-terminal (name
                                    [#:literals (literal-ident ...+)]
                                    [#:datum-literals (literal-ident ...+)]
                                    pattern ...+))

                    (non-terminal-delta (name
                                          [#:literals- (literal-ident ...+)]
                                          [#:literals+ (literal-ident ...+)]
                                          [#:datum-literals- (literal-ident ...+)]
                                          [#:datum-literals+ (literal-ident ...+)]
                                          [(- pattern ...+)]
                                          [(+ pattern ...+)]))

                    (pattern ident
                             literal-ident
                             keyword
                             (~maybe pattern)
                             (pattern ...)
                             ...
                             ...+)]]

@subsection{Defining a language}

@racket[language-name] is the identifier bound to the language.

The @racket[language] clause defines a freestanding language:

@racket[#:entry-point] specifies the non-terminal used as the
language's entry point.

@racket[#:terminals] clause defines the set of terminals available in the
language. Each terminal associates an identifier with a syntax class.

@subsection{Extending a language}

The @racket[language-delta] clause derives a language from an existing language:

@racket[#:extends] specifies the language to use as a base.

@racket[#:literals-] and @racket[#:literals+] specify literals to remove and
add, respectively.

@racket[#:datum-literals-] and @racket[#:datum-literals+] specify datum literals
to remove and add, respectively.

@racket[-] and @racket[+] clauses specify production @racket[pattern]s to remove
and add, respectively.

@subsection{Non-terminals}

A @racket[non-terminal] definition consists of a non-terminal name, followed by
its literals and one @racket[pattern] for each of its productions:

@racket[#:literals] declares literals that are matched by their binding in the
surrounding definition scope.

@racket[#:datum-literals] declares literals that are matched by their written
form.

@subsection{Non-terminal productions}

The @racket[pattern] clause may contain terminal or non-terminal identifiers,
literals, keywords, lists of nested patterns, @racket[~maybe] whose child
pattern may appear zero or one times, a zero-or-more repetition @racket[...],
or a one-or-more repetition @racket[...+].

@subsubsection{Production ordering}

During parsing, a non-terminal's @racket[pattern]s are considered
in top-to-bottom order.

This can cause a less-specific pattern (such as a terminal with the @racket[id]
syntax class) to override a more-specific pattern (such as a literal) if they
are otherwise structurally equivalent.

Therefore, to prevent unintended mis-parses, clauses should be ordered from
most- to least- specific.

@subsection{Products of @racket[define-language]}

@defform[(define-language-parser parser-name language-name)]

Binds @racket[parser-name] to a parser for @racket[language-name].

This is chiefly useful as a validation mechanism, as a successful parse returns
the original syntax unmodified.

@defform[(define-language-classes language-name
           [ident non-terminal-ident] ...)]

Binds the supplied @racket[name]s to syntax classes corresponding to the
respective @racket[non-terminal-ident].

This is useful in language composition, as it allows the terminals of one
language to mention the non-terminals of another.

@section{The @racket[define-pass] Form}

The @racket[define-pass] form defines a transformation between a source
representation and a target representation. The representations may be
languages or arbitrary Racket values described by predicates.

@defform[#:literals (-> *)

         (define-pass pass-name
           (-> pass-input-spec pass-output-spec)
           processor ...)

         #:grammar [(pass-input-spec pass-io-spec)
                    (pass-output-spec pass-io-spec)

                    (pass-io-spec language-ident
                                  predicate-ident
                                  *)

                    (processor (processor-name
                                 (-> processor-input-spec processor-output-spec)
                                 processor-clause ...))

                    (processor-input-spec processor-io-spec)
                    (processor-output-spec processor-io-spec)

                    (processor-io-spec non-terminal-ident
                                       predicate-ident
                                       *)

                    (processor-clause (pattern
                                       body-expr ...+))
                    (pattern ident
                             (pattern ...)
                             (~rec ident)
                             (~maybe pattern)
                             ...
                             ...+)]]

@subsection{Defining a pass}

@racket[pass-name] is the identifier bound to the pass.

@racket[pass-input-spec] determines the input of the pass:

@racket[language-ident] causes the pass to take syntax in the given language,
and requires that all @racket[processor-input-spec]s name one of its
non-terminals.

@racket[predicate-ident] and @racket[*] cause the pass to take arbitrary
Racket values, with the former asserting the given predicate when the pass is
called.

@racket[pass-output-spec] determines the input of the pass:

@racket[language-ident] causes the pass to return syntax in the given language,
and requires that all @racket[processor-output-spec]s name one of its
non-terminals.

@racket[predicate-ident] and @racket[*] cause the pass to return arbitrary
Racket values, with the former asserting the given predicate before the pass
returns.

@subsection{Processors}

Each @racket[processor] defines a processor with the given
@racket[processor-name].

@racket[processor-input-spec] determines the input of the processor:

@racket[non-terminal-ident] causes the processor to take productions of the
given non-terminal in the language specified by @racket[pass-input].

@racket[predicate-ident] and @racket[*] cause the processor to take arbitrary
Racket values, where predicates are used for dispatch against processor clauses.

@racket[processor-output-spec] determines the output of a processor:

@racket[non-terminal-ident] causes the processor to return productions of the
given non terminal in the language specified by @racket[pass-output],
which are validated against the outgoing language.

@racket[predicate-ident] and @racket[*] cause the processor to return arbitrary
Racket values, where predicates are used to assert their type.

@subsubsection{Processor ordering}

@racket[processor]s sharing a @racket[non-terminal-ident] input are merged at
definition time, and considered in top-to-bottom order when parsing syntax.

This can cause a processor with less-specific patterns to override one
containing more-specific patterns if they are otherwise structurally equivalent.

This is chiefly pertinent to application of the pass method,
but - currently - also to recursion via @racket[~rec], as it operates by dispatching
the pass over the target form, rather than invoking a specific processor.

Therefore, to prevent unintended mis-parses, processors should be ordered from
most- to least- specific.

@subsection{Processor clauses}

@racket[processor-clause] defines a processor which takes input in the shape
@racket[pattern], and transforms it into the processor's output via
@racket[body-expr].

When the input of a pass is a @racket[language-ident], @racket[pattern] acts
as a subset of a @racketmodname[syntax/parse] patterns, extended with the
cata-morphism action pattern @racket[(~rec ident)] for dispatching the pass over
@racket[ident] before the body is evaluated, and the zero-or-one specifier
@racket[(~maybe pattern)]. The processor body may use forms from the pattern
bodies of a @racketmodname[syntax/parse] syntax class.

When the input of a pass is a @racket[predicate-ident] or @racket[*],
@racket[pattern] behaves as a @racketmodname[racket/match] pattern,
with @racket[(~rec ident)] providing equivalent behavior.
@racket[(~maybe pattern)] is not available in this mode.

