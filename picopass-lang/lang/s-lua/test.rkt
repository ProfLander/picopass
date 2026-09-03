#lang racket/base

(module+ test
  (require racket/string
           rackunit
           picopass/lang/s-lua/language
           picopass/lang/s-lua/to-source)

  (test-equal? "assignment"
               (s-lua->lua #'[(= [x] [y])
                              (= [(-> x 1) (-> x k)]
                                 [y z])])
               (string-join '("x = y"
                              "x[1], x.k = y, z")
                            "\n"))

  (test-equal? "functioncall"
               (s-lua->lua #'[(f)
                              (f 1 2)
                              (f (table))
                              (f "s")
                              (x:f)])
               (string-join '("f()"
                              "f(1, 2)"
                              "f{}"
                              "f\"s\""
                              "x:f()")
                            "\n"))

  (test-equal? "label / goto"
               (s-lua->lua #'[(:: L)
                              (goto L)])
               (string-join '("::L::"
                              "goto L")
                            "\n"))

  (test-equal? "do"
               (s-lua->lua #'[(do)])
               (string-join '("do"
                              "end")
                            "\n"))

  (test-equal? "while"
               (s-lua->lua #'[(while false
                                     (break))])
               (string-join '("while false do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "repeat"
               (s-lua->lua #'[(repeat (break)
                                      (until true))])
               (string-join '("repeat"
                              "  break"
                              "until true")
                            "\n"))

  (test-equal? "if / elseif / else"
               (s-lua->lua #'[(if true (then)
                                  (elseif false (then))
                                  (elseif true (then))
                                  (else))])
               (string-join '("if true then"
                              "elseif false then"
                              "elseif true then"
                              "else"
                              "end")
                            "\n"))

  (test-equal? "numeric for"
               (s-lua->lua #'[(for [i 1 2]
                                (break))
                              (for [i 1 2 3]
                                (break))])
               (string-join '("for i = 1, 2 do"
                              "  break"
                              "end"
                              "for i = 1, 2, 3 do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "generic for"
               (s-lua->lua #'[(for ([k f]
                                    [v g])
                                (break))])
               (string-join '("for k, v in f, g do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "function name body"
               (s-lua->lua #'[(function (a.b.c:d x y))])
               (string-join '("function a.b.c:d(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local function name body"
               (s-lua->lua #'[(local (function (lf x y)))])
               (string-join '("local function lf(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local namelist"
               (s-lua->lua #'[(local [a b])])
               (string-join '("local a, b")
                            "\n"))

  (test-equal? "local namelist = explist"
               (s-lua->lua #'[(local [literals_and_forms] [nil
                                                           false
                                                           true
                                                           1
                                                           "s"
                                                           vararg])])
               (string-join '("local literals_and_forms = nil, false, true, 1, \"s\", ...")
                            "\n"))

  (test-equal? "local functiondef"
               (s-lua->lua #'[(local [functiondef] [(function ())])])
               (string-join '("local functiondef = function()"
                              "end")
                            "\n"))

  (test-equal? "local prefixexp"
               (s-lua->lua #'[(local [prefixexp] [literals_and_forms])])
               (string-join '("local prefixexp = literals_and_forms")
                            "\n"))

  (test-equal? "local table"
               (s-lua->lua #'[(local [table] [(table)])])
               (string-join '("local table = {}")
                            "\n"))

  (test-equal? "local binop"
               (s-lua->lua #'[(local [binop] [(+ 1 1)])])
               (string-join '("local binop = 1 + 1")
                            "\n"))

  (test-equal? "local unop"
               (s-lua->lua #'[(local [unop] [(- 1)])])
               (string-join '("local unop = -1")
                            "\n"))

  (test-equal? "prefixexp"
               (s-lua->lua #'[(local [p1] [literals_and_forms])
                              (local [p2] [(f)])
                              (local [p3] [(prefix literals_and_forms)])])
               (string-join '("local p1 = literals_and_forms"
                              "local p2 = f()"
                              "local p3 = (literals_and_forms)")
                            "\n"))

  (test-equal? "var"
               (s-lua->lua #'[(= [p1] [1])
                              (= [(-> p1 1)] [2])
                              (= [(-> p1 k)] [3])])
               (string-join '("p1 = 1"
                              "p1[1] = 2"
                              "p1.k = 3")
                            "\n"))

  (test-equal? "namelist function"
               (s-lua->lua #'[(local (function (named_varargs a b vararg)
                                               (return)))])
               (string-join '("local function named_varargs(a, b, ...)"
                              "  return"
                              "end")
                            "\n"))

  (test-equal? "anonymous function"
               (s-lua->lua #'[(local [anonymous] [(function (a b)
                                                            (return 1 2))])])
               (string-join '("local anonymous = function(a, b)"
                              "  return 1, 2"
                              "end")
                            "\n"))

  (test-equal? "anonymous vararg function"
               (s-lua->lua #'[(local [anonymous] [(function (a b vararg)
                                                            (return vararg))])])
               (string-join '("local anonymous = function(a, b, ...)"
                              "  return ..."
                              "end")
                            "\n"))

  (test-equal? "table"
               (s-lua->lua #'[(local [t] [(table [1 2]
                                                 [name 3]
                                                 4
                                                 [5 6])])])
               (string-join '("local t = {[1] = 2, name = 3, 4, [5] = 6}")
                            "\n"))

  (test-equal? "binops"
               (s-lua->lua #'[(local [add] [(+ 1 1)])
                              (local [sub] [(- 1 1)])
                              (local [mul] [(* 1 1)])
                              (local [div] [(/ 1 1)])
                              (local [exp] [(^ 1 1)])
                              (local [mod] [(% 1 1)])
                              (local [cat] [(.. "a" "b")])
                              (local [lt] [(< 1 2)])
                              (local [le] [(<= 1 2)])
                              (local [gt] [(> 1 2)])
                              (local [ge] [(>= 1 2)])
                              (local [eq] [(== 1 2)])
                              (local [ne] [(~= 1 2)])
                              (local [op_and] [(and true false)])
                              (local [op_or] [(or true false)])])
               (string-join '("local add = 1 + 1"
                              "local sub = 1 - 1"
                              "local mul = 1 * 1"
                              "local div = 1 / 1"
                              "local exp = 1 ^ 1"
                              "local mod = 1 % 1"
                              "local cat = \"a\" .. \"b\""
                              "local lt = 1 < 2"
                              "local le = 1 <= 2"
                              "local gt = 1 > 2"
                              "local ge = 1 >= 2"
                              "local eq = 1 == 2"
                              "local ne = 1 ~= 2"
                              "local op_and = true and false"
                              "local op_or = true or false")
                            "\n"))

  (test-equal? "unops"
               (s-lua->lua #'[(local [unops] [(- 1)
                                              (not true)
                                              (length "s")])])
               (string-join '("local unops = -1, not true, #\"s\"")
                            "\n"))

  (test-equal? "retstat"
               (s-lua->lua #'[(function (r0)
                                        (return))
                              (function (r1)
                                        (return 1))])
               (string-join '("function r0()"
                              "  return"
                              "end"
                              "function r1()"
                              "  return 1"
                              "end")
                            "\n"))

  (test-equal? "vararg-host"
               (s-lua->lua #'[(local (function (vararg_host vararg)
                                               (local [_] [vararg])))])
               (string-join '("local function vararg_host(...)"
                              "  local _ = ..."
                              "end")
                            "\n")))

