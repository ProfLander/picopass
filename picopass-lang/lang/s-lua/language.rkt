#lang picopass

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define reserved-symbols
  (list '=
        'break
        'goto
        'do
        'while
        'repeat
        'until
        'if
        'for
        'function
        'local-function
        'vararg
        'local
        'then
        'elseif
        'else
        'return
        '::
        '->
        'nil
        'false
        'true
        '+
        '-
        '*
        '/
        '^
        '%
        '..
        '<
        '<=
        '>
        '>=
        '==
        '~=
        'and
        'or
        'not
        'length))

(define-syntax-class name
  (pattern ident:id
           #:attr symbol (syntax-e #'ident)
           #:fail-when (member (attribute symbol) reserved-symbols)
           "reserved name"))

(define-syntax-class vararg
  (pattern (~datum ...)))

; funcname ::=
(define-syntax-class function-name
  (pattern ident:id

           #:with (segments (~optional member))
           (string-split (symbol->string (syntax-e #'ident)) ":")

           #:with (path ...)
           (string-split (syntax-e #'segments) ".")))

[define-language s-lua
 #:entry-point chunk
 #:terminals [name
              boolean
              number
              string
              vararg
              function-name]

 ; chunk ::=
 (chunk
  #:datum-literals [#%chunk]
  (#%chunk block))

 ; block ::=
 (block
  #:datum-literals [#%block]
  (#%block
   statement ...
   (~maybe return-statement)))

 ; stat ::=
 (statement
   #:datum-literals [=
                     break
                     goto
                     do
                     while
                     repeat
                     until
                     if
                     for
                     function
                     local-function
                     local]

   (= ~cut [var ...] [expr ...])

   function-call
   label
   (break)
   (goto ~cut name)
   (do ~cut block)
   (while ~cut expr block)
   (repeat ~cut block (until ~cut expr))

   ; if - subforms handle block inlining
   (if ~cut expr
       if/then
       if/elseif
       ...
       if/else)

   ; for name = exp, exp [, exp]
   (for (name expr expr (~maybe expr))
     block)

   ; for-in
   (for ([name expr] ...)
     block)

   ; function - subform handles block inlining
   statement/function

   (local [var ...])
   (local [var ...] [expr ...])

   ; local function - subform handles block inlining
   (local statement/function))

 ; if subforms
 (if/then
   #:description "then"
   #:datum-literals [then]
   (then ~cut block))

 (if/elseif
   #:description "elseif"
   #:datum-literals [elseif]
   (elseif ~cut expr if/then))

 (if/else
   #:description "else"
   #:datum-literals [else]
   (else ~cut block))

 ; function subforms
 (statement/function
   #:description "function"
   #:datum-literals [function]
   (function ~cut (function-name name ... (~maybe vararg))
             block))

 ; retstat ::=
 (return-statement
   #:description "return"
   #:datum-literals [return]
   (return ~cut expr ...))

 ; label ::=
 (label
   #:datum-literals [::]
   (:: ~cut name))

 ; varlist - inlined into parent forms

 ; var ::=
 ; . is reserved in racket, replaced with ->
 ; [] has same semantic with different target, replaced with ->
 (var
   #:description "variable"
   #:datum-literals [->]
   name
   (-> prefix-expr name)
   (-> prefix-expr expr))

 ; namelist - inlined into parent forms

 ; explist - inlined into parent forms

 ; exp ::=
 (expr
   #:description "expression"
   #:datum-literals [nil]
   nil
   boolean
   number
   string
   vararg
   function-definition
   prefix-expr
   table
   (unary-op expr)
   (binary-op expr ~cut expr))

 ; prefixexp ::=
 (prefix-expr
   #:description "prefix expression"
   #:datum-literals [quote]
   var
   (quote expr)
   function-call)

 ; functioncall ::=
 (function-call
   #:description "function call"
   (prefix-expr expr ...)
   (prefix-expr table)
   (prefix-expr string))

 ; args - inlined into function-call

 ; functiondef ::=
 (function-definition
   #:description "function definition"
   #:datum-literals [function]
   (function ~cut (name ... (~maybe vararg))
             block))

 ; funcbody - inlined into parent forms

 ; parlist - inlined into parent forms

 ; tableconstructor ::=
 (table
   #:datum-literals [table]
   (table ~cut table-field ...))

 ; fieldlist - inlined into table

 ; field ::=
 (table-field
   #:description "table field"
   [name expr]
   [expr expr]
   expr)

 ; fieldsep - unneeded with s-expressions

 ; binop ::=
 (binary-op
   #:description "binary operator"
   #:datum-literals [+ - * / ^ % .. < <= > >= == ~= and or]
   + - * / ^ % .. < <= > >= == ~= and or)

 ; unop ::=
 ; # is reserved in racket, replaced with length
 (unary-op
   #:description "unary operator"
   #:datum-literals [- not length]
   - not length)]

(define-language-parser parse-s-lua s-lua)

