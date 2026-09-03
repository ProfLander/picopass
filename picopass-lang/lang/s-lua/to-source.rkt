#lang picopass

(require picopass/lang/s-lua/language)

(provide (all-defined-out))

(define (indent str)
  (string-replace str "\n" "\n  "))

(define (format-body stmts ret
                     #:leading-newline [leading-newline #t])
  (string-join (append (if (and leading-newline
                                (or (pair? stmts)
                                    ret))
                           (list "")
                           null)
                       (map syntax-e stmts)
                       (if ret
                           (list (syntax-e ret))
                           null))
               "\n"))

[define-pass s-lua->lua
 (-> s-lua string?)

 (chunk
   (-> chunk string?)

   [(~rec block:block)
    (syntax-e (attribute block))])

 (block
   (-> block string?)

   [[(~rec stmt:statement) ...
     (~maybe (~rec ret:return-statement))]
    (format-body (attribute stmt)
                 (attribute ret)
                 #:leading-newline #f)])

 (statement
   (-> statement string?)

   [(= ~cut [(~rec var:var) ...] [(~rec exp:expr) ...])
    (format "~a = ~a"
            (string-join (map syntax-e (attribute var)) ", ")
            (string-join (map syntax-e (attribute exp)) ", "))]

   [(~rec call:function-call)
    (syntax-e (attribute call))]

   [(~rec label:label)
    (syntax-e (attribute label))]

   [(break)
    "break"]

   [(goto ~cut name:name)
    (format "goto ~a" (syntax-e (attribute name)))]

   [(do ~cut (~rec stmt:statement) ...
      (~maybe (~rec ret:return-statement)))
    (format "do~a\nend"
            (indent (format-body (attribute stmt) (attribute ret))))]

   [(while ~cut (~rec cond:expr)
           (~rec stmt:statement) ...
           (~maybe (~rec ret:return-statement)))
    (format "while ~a do~a\nend"
            (syntax-e (attribute cond))
            (indent (format-body (attribute stmt) (attribute ret))))]

   [(repeat ~cut
            (~rec stmt:statement) ...
            (~maybe (~rec ret:return-statement))
            (until ~cut (~rec cond:expr)))
    (format "repeat~a\nuntil ~a"
            (indent (format-body (attribute stmt) (attribute ret)))
            (syntax-e (attribute cond)))]

   [(if ~cut (~rec cond:expr)
        (~rec then:statement/if/then)
        (~rec elseif:statement/if/elseif)
        ...
        (~rec else:statement/if/else))
    (format "if ~a ~a~a~a\nend"
            (syntax-e (attribute cond))
            (syntax-e (attribute then))
            (string-join (append (if (pair? (attribute elseif))
                                     (list "")
                                     null)
                                 (map syntax-e (attribute elseif)))
                         "\n")
            (string-append "\n" (syntax-e (attribute else))))]

   [(for (name:name (~rec from:expr)
                    (~rec to:expr)
                    (~maybe (~rec step:expr)))
      (~rec stmt:statement) ...
      (~maybe (~rec ret:return-statement)))
    (format "for ~a = ~a, ~a~a do~a\nend"
            (syntax-e (attribute name))
            (syntax-e (attribute from))
            (syntax-e (attribute to))
            (if (attribute step)
                (string-append ", " (syntax-e (attribute step)))
                "")
            (indent (format-body (attribute stmt) (attribute ret))))]

   [(for ([name:name (~rec exp:expr)] ...)
      (~rec stmt:statement) ...
      (~maybe (~rec ret:return-statement)))
    (format "for ~a in ~a do~a\nend"
            (string-join (map (compose symbol->string syntax-e) (attribute name)) ", ")
            (string-join (map syntax-e (attribute exp)) ", ")
            (indent (format-body (attribute stmt) (attribute ret))))]

   [(~rec stat:statement/function)
    (syntax-e (attribute stat))]

   [(local [(~rec var:var) ...])
    (format "local ~a"
            (string-join (map syntax-e (attribute var)) ", "))]

   [(local [(~rec var:var) ...] [(~rec exp:expr) ...])
    (format "local ~a = ~a"
            (string-join (map syntax-e (attribute var)) ", ")
            (string-join (map syntax-e (attribute exp)) ", "))]

   [(local (~rec func:statement/function))
    (format "local ~a" (syntax-e (attribute func)))])

 (statement/if/then
   (-> statement/if/then string?)

   [(then ~cut (~rec stmt:statement) ...
          (~maybe (~rec ret:return-statement)))
    (format "then~a"
            (indent (format-body (attribute stmt) (attribute ret))))])

 (statement/if/elseif
   (-> statement/if/elseif string?)

   [(elseif ~cut (~rec exp:expr) (~rec then:statement/if/then))
    (format "elseif ~a ~a"
            (syntax-e (attribute exp))
            (syntax-e (attribute then)))])

 (statement/if/else
   (-> statement/if/else string?)

   [(else ~cut (~rec stmt:statement) ...
          (~maybe (~rec ret:return-statement)))
    (format "else~a"
            (indent (format-body (attribute stmt) (attribute ret))))])

 (statement/function
   (-> statement/function string?)

   [(function ~cut (name:function-name arg:name ... (~maybe vararg:vararg))
              (~rec stmt:statement) ...
              (~maybe (~rec ret:return-statement)))
    (format "function ~a(~a)~a\nend"
            (syntax-e (attribute name))
            (string-join (append (map (compose symbol->string syntax-e)
                                      (attribute arg))
                                 (if (attribute vararg)
                                     (list "...")
                                     null))
                         ", ")
            (indent (format-body (attribute stmt) (attribute ret))))])

 (return-statement
   (-> return-statement string?)

   [(return ~cut (~rec exp:expr) ...)
    (format "return~a~a"
            (if (pair? (attribute exp))
                " "
                "")
            (string-join (map syntax-e (attribute exp))
                         ", "))])

 (label
   (-> label string?)

   [(:: ~cut name:name)
    (format "::~a::" (syntax-e (attribute name)))])

 (var
   (-> var string?)

   [name:name
    (symbol->string (attribute name.symbol))]

   [(-> (~rec exp:prefix-expr) field:name)
    (format "~a.~a" (syntax-e #'exp) (syntax-e #'field))]

   [(-> (~rec exp:prefix-expr) (~rec field:expr))
    (format "~a[~a]" (syntax-e #'exp) (syntax-e #'field))])

 (expr
   (-> expr string?)

   [nil
    "nil"]

   [false
    "false"]

   [true
    "true"]

   [num:number
    (number->string (syntax-e #'num))]

   [str:string
    (format "\"~a\"" (syntax-e #'str))]

   [vararg:vararg
    "..."]

   [(~rec func:function-definition)
    (syntax-e (attribute func))]

   [(~rec tbl:table)
    (syntax-e (attribute tbl))]

   [(~rec prefix:prefix-expr)
    (syntax-e (attribute prefix))]

   [((~rec unop:unary-op) (~rec exp:expr))
    (let ([op-sym (syntax-e (attribute unop))])
      (format "~a~a~a"
              op-sym
              (if (eq? "not" op-sym)
                  " "
                  "")
              (syntax-e (attribute exp))))]

   [((~rec binop:binary-op) (~rec a:expr) ~cut (~rec b:expr))
    (format "~a ~a ~a"
            (syntax-e (attribute a))
            (syntax-e (attribute binop))
            (syntax-e (attribute b)))])

 (prefix-expr
   (-> prefix-expr string?)

   [(~rec var:var)
    (syntax-e (attribute var))]

   [(prefix (~rec exp:expr))
    (format "(~a)" (syntax-e (attribute exp)))]

   [(~rec call:function-call)
    (syntax-e (attribute call))])

 (function-call
   (-> function-call string?)

   [((~rec prefix:prefix-expr) str:string)
    (format "~a\"~a\""
            (syntax-e (attribute prefix))
            (syntax-e (attribute str)))]

   [((~rec prefix:prefix-expr) (~rec table:table))
    (format "~a~a"
            (syntax-e (attribute prefix))
            (syntax-e (attribute table)))]

   [((~rec prefix:prefix-expr) (~rec exp:expr) ...)
    (format "~a(~a)"
            (syntax-e (attribute prefix))
            (string-join (map syntax-e (attribute exp))
                         ", "))])

 (function-definition
   (-> function-definition string?)

   [(function ~cut (arg:name ... (~maybe vararg:vararg))
              (~rec stmt:statement) ...
              (~maybe (~rec ret:return-statement)))
    (format "function(~a)~a\nend"
            (string-join (append (map (compose symbol->string syntax-e) (attribute arg))
                                 (if (attribute vararg)
                                     (list "...")
                                     null))
                         ", ")
            (indent
              (string-join (append (if (or (pair? (attribute stmt))
                                           (attribute ret))
                                       (list "")
                                       null)
                                   (map syntax-e (attribute stmt))
                                   (if (attribute ret)
                                       (list (syntax-e (attribute ret)))
                                       null))
                           "\n")))])

 (table
   (-> table string?)

   [(table ~cut (~rec field:table-field) ...)
    (format "{~a}" (string-join (map syntax-e (attribute field))
                                ", "))])

 (table-field
   (-> table-field string?)

   [[key:name (~rec exp:expr)]
    (format "~a = ~a"
            (syntax-e (attribute key))
            (syntax-e (attribute exp)))]

   [[(~rec key:expr) (~rec exp:expr)]
    (format "[~a] = ~a"
            (syntax-e (attribute key))
            (syntax-e (attribute exp)))]

   [(~rec exp:expr)
    (syntax-e (attribute exp))])

 (binary-op
   (-> binary-op string?)

   [+ "+"]
   [- "-"]
   [* "*"]
   [/ "/"]
   [^ "^"]
   [% "%"]
   [.. ".."]
   [< "<"]
   [<= "<="]
   [> ">"]
   [>= ">="]
   [== "=="]
   [~= "~="]
   [and "and"]
   [or "or"])

 (unary-op
   (-> unary-op string?)

   [- "-"]
   [not "not"]
   [length "#"])]

