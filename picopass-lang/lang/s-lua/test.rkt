#lang racket/base

(module+ test
  (require racket/string
           racket/syntax
           rackunit
           picopass/lang/s-lua/language
           picopass/lang/s-lua/to-source)

  (define/with-syntax ooo (quote-syntax ...))

  (test-equal? "assignment"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (= [x] [y])
                               (= [(-> x 1) (-> x k)]
                                  [y z]))))
               (string-join '("x = y"
                              "x[1], x.k = y, z")
                            "\n"))

  (test-equal? "functioncall"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (f)
                               (f 1 2)
                               (f (table))
                               (f "s")
                               (x:f))))
               (string-join '("f()"
                              "f(1, 2)"
                              "f{}"
                              "f\"s\""
                              "x:f()")
                            "\n"))

  (test-equal? "label / goto"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (:: L)
                               (goto L))))
               (string-join '("::L::"
                              "goto L")
                            "\n"))

  (test-equal? "do"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (do (#%block)))))
               (string-join '("do"
                              "end")
                            "\n"))

  (test-equal? "while"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (while #f
                                 (#%block
                                  (break))))))
               (string-join '("while false do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "repeat"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (repeat
                                (#%block
                                 (break))
                                (until #t)))))
               (string-join '("repeat"
                              "  break"
                              "until true")
                            "\n"))

  (test-equal? "if / elseif / else"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (if #t (then (#%block))
                                   (elseif #f (then (#%block)))
                                   (elseif #t (then (#%block)))
                                   (else (#%block))))))
               (string-join '("if true then"
                              "elseif false then"
                              "elseif true then"
                              "else"
                              "end")
                            "\n"))

  (test-equal? "numeric for"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (for [i 1 2]
                                 (#%block
                                  (break)))
                               (for [i 1 2 3]
                                 (#%block
                                  (break))))))
               (string-join '("for i = 1, 2 do"
                              "  break"
                              "end"
                              "for i = 1, 2, 3 do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "generic for"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (for ([k f]
                                     [v g])
                                 (#%block
                                  (break))))))
               (string-join '("for k, v in f, g do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "function name body"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (function (a.b.c:d x y)
                                         (#%block)))))
               (string-join '("function a.b.c:d(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local function name body"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local (function (lf x y)
                                                (#%block))))))
               (string-join '("local function lf(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local namelist"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [a b]))))
               (string-join '("local a, b")
                            "\n"))

  (test-equal? "local namelist = explist"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [literals_and_forms] [nil
                                                            #f
                                                            #t
                                                            1
                                                            "s"
                                                            ooo]))))
               (string-join '("local literals_and_forms = nil, false, true, 1, \"s\", ...")
                            "\n"))

  (test-equal? "local functiondef"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [functiondef] [(function () (#%block))]))))
               (string-join '("local functiondef = function()"
                              "end")
                            "\n"))

  (test-equal? "local prefixexp"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [prefixexp] [literals_and_forms]))))
               (string-join '("local prefixexp = literals_and_forms")
                            "\n"))

  (test-equal? "local table"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [table] [(table)]))))
               (string-join '("local table = {}")
                            "\n"))

  (test-equal? "local binop"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [binop] [(+ 1 1)]))))
               (string-join '("local binop = 1 + 1")
                            "\n"))

  (test-equal? "local unop"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [unop] [(- 1)]))))
               (string-join '("local unop = -1")
                            "\n"))

  (test-equal? "prefixexp"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [p1] [literals_and_forms])
                               (local [p2] [(f)])
                               (local [p3] ['literals_and_forms]))))
               (string-join '("local p1 = literals_and_forms"
                              "local p2 = f()"
                              "local p3 = (literals_and_forms)")
                            "\n"))

  (test-equal? "var"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (= [p1] [1])
                               (= [(-> p1 1)] [2])
                               (= [(-> p1 k)] [3]))))
               (string-join '("p1 = 1"
                              "p1[1] = 2"
                              "p1.k = 3")
                            "\n"))

  (test-equal? "namelist function"
               (s-lua->lua
                #'(#%chunk
                   (#%block
                    (local (function (named_varargs a b ooo)
                                     (#%block
                                      (return)))))))
               (string-join '("local function named_varargs(a, b, ...)"
                              "  return"
                              "end")
                            "\n"))

  (test-equal? "anonymous function"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [anonymous]
                                 [(function (a b)
                                            (#%block
                                             (return 1 2)))]))))
               (string-join '("local anonymous = function(a, b)"
                              "  return 1, 2"
                              "end")
                            "\n"))

  (test-equal? "anonymous vararg function"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [anonymous]
                                 [(function (a b ooo)
                                            (#%block
                                             (return ooo)))]))))
               (string-join '("local anonymous = function(a, b, ...)"
                              "  return ..."
                              "end")
                            "\n"))

  (test-equal? "table"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [t] [(table [1 2]
                                                  [name 3]
                                                  4
                                                  [5 6])]))))
               (string-join '("local t = {[1] = 2, name = 3, 4, [5] = 6}")
                            "\n"))

  (test-equal? "binops"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [add] [(+ 1 1)])
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
                               (local [op_and] [(and #t #f)])
                               (local [op_or] [(or #t #f)]))))
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
               (s-lua->lua #'(#%chunk
                              (#%block
                               (local [unops] [(- 1)
                                               (not #t)
                                               (length "s")]))))
               (string-join '("local unops = -1, not true, #\"s\"")
                            "\n"))

  (test-equal? "retstat"
               (s-lua->lua #'(#%chunk
                              (#%block
                               (function (r0)
                                         (#%block
                                          (return)))
                               (function (r1)
                                         (#%block
                                          (return 1))))))
               (string-join '("function r0()"
                              "  return"
                              "end"
                              "function r1()"
                              "  return 1"
                              "end")
                            "\n"))

  (test-equal? "vararg-host"
               (s-lua->lua
                #'(#%chunk
                   (#%block
                    (local (function (vararg_host ooo)
                                     (#%block
                                      (local [_] [ooo])))))))
               (string-join '("local function vararg_host(...)"
                              "  local _ = ..."
                              "end")
                            "\n")))

