(troupe-ir-sexp
 2
 (program
  (@
   ("tests/rt/pos/core/fib10.trp" 1 9)
   (fun
    "fib40"
    (arg (@ ("tests/rt/pos/core/fib10.trp" 1 13) "fib_arg141"))
    (consts
     ("gensym114" (int 2))
     ("gensym111" (int 1))
     ("gensym112" (int 1)))
    (bb
     ((@
       ("tests/rt/pos/core/fib10.trp" 2 12)
       (assign
        "gensym113"
        (bin
         Gt
         (@ ("tests/rt/pos/core/fib10.trp" 2 10) (local "fib_arg141"))
         (@ ("tests/rt/pos/core/fib10.trp" 2 14) (local "gensym114"))))))
     (@
      ("tests/rt/pos/core/fib10.trp" 2 7)
      (if
       (@ ("tests/rt/pos/core/fib10.trp" 2 7) (local "gensym113"))
       (bb
        ((@
          ("tests/rt/pos/core/fib10.trp" 3 19)
          (assign
           "gensym110"
           (bin
            Minus
            (@ ("tests/rt/pos/core/fib10.trp" 3 17) (local "fib_arg141"))
            (@ ("tests/rt/pos/core/fib10.trp" 3 21) (local "gensym111"))))))
        (@
         ("tests/rt/pos/core/fib10.trp" 3 12)
         (stack-expand
          "gensym106"
          (bb
           ()
           (@
            ("tests/rt/pos/core/fib10.trp" 3 12)
            (tail-call
             (@ ("tests/rt/pos/core/fib10.trp" 3 12) self)
             (@ ("tests/rt/pos/core/fib10.trp" 3 12) (local "gensym110")))))
          (bb
           ((@
             ("tests/rt/pos/core/fib10.trp" 3 33)
             (assign
              "gensym108"
              (bin
               Minus
               (@ ("tests/rt/pos/core/fib10.trp" 3 31) (local "fib_arg141"))
               (@ ("tests/rt/pos/core/fib10.trp" 3 35) (local "gensym114"))))))
           (@
            ("tests/rt/pos/core/fib10.trp" 3 26)
            (stack-expand
             "gensym107"
             (bb
              ()
              (@
               ("tests/rt/pos/core/fib10.trp" 3 26)
               (tail-call
                (@ ("tests/rt/pos/core/fib10.trp" 3 26) self)
                (@ ("tests/rt/pos/core/fib10.trp" 3 26) (local "gensym108")))))
             (bb
              ((@
                ("tests/rt/pos/core/fib10.trp" 3 24)
                (assign
                 "gensym105"
                 (bin
                  Plus
                  (@ ("tests/rt/pos/core/fib10.trp" 3 12) (local "gensym106"))
                  (@ ("tests/rt/pos/core/fib10.trp" 3 26) (local "gensym107"))))))
              (@
               ("tests/rt/pos/core/fib10.trp" 3 24)
               (ret
                (@
                 ("tests/rt/pos/core/fib10.trp" 3 24)
                 (local "gensym105")))))))))))
       (bb
        ()
        (@
         ("tests/rt/pos/core/fib10.trp" 4 12)
         (ret
          (@
           ("tests/rt/pos/core/fib10.trp" 4 12)
           (local "gensym112"))))))))))
  (fun
   "fwriteln2"
   (arg "fwriteln_arg18")
   (consts
    ("gensym16" (int 2))
    ("gensym18" (bool false))
    ("gensym13" (string "pattern match failure in function fwriteln"))
    ("gensym4" (string "\\n")))
   (bb
    ()
    (stack-expand
     "gensym12"
     (bb
      ((assign "gensym19" (un IsTuple (local "fwriteln_arg18"))))
      (if
       (local "gensym19")
       (bb
        ((assign "gensym15" (un TupleLength (local "fwriteln_arg18")))
         (assign
          "gensym14"
          (bin
           Eq
           (local "gensym15")
           (@ (rt "CaseElimination") (local "gensym16")))))
        (ret (local "gensym14")))
       (bb () (ret (local "gensym18")))))
     (bb
      ()
      (assert-else-error
       (local "gensym12")
       (bb
        ((assign "gensym10" (proj-idx (local "fwriteln_arg18") 0))
         (assign "gensym8" (proj-idx (local "fwriteln_arg18") 1)))
        (stack-expand
         "$decltemp$13"
         (bb
          ((assign "gensym6" (base "fwrite"))
           (assign "gensym7" (tuple (local "gensym10") (local "gensym8"))))
          (tail-call (local "gensym6") (local "gensym7")))
         (bb
          ((assign "gensym3" (base "fwrite"))
           (assign "gensym5" (tuple (local "gensym10") (local "gensym4"))))
          (tail-call (local "gensym3") (local "gensym5")))))
       (local "gensym13"))))))
  (fun
   "fwritelnWithLabels3"
   (arg "fwritelnWithLabels_arg115")
   (consts
    ("gensym41" (int 2))
    ("gensym43" (bool false))
    ("gensym38"
     (string "pattern match failure in function fwritelnWithLabels")))
   (bb
    ()
    (stack-expand
     "gensym37"
     (bb
      ((assign
        "gensym44"
        (un IsTuple (local "fwritelnWithLabels_arg115"))))
      (if
       (local "gensym44")
       (bb
        ((assign
          "gensym40"
          (un TupleLength (local "fwritelnWithLabels_arg115")))
         (assign
          "gensym39"
          (bin
           Eq
           (local "gensym40")
           (@ (rt "CaseElimination") (local "gensym41")))))
        (ret (local "gensym39")))
       (bb () (ret (local "gensym43")))))
     (bb
      ()
      (assert-else-error
       (local "gensym37")
       (bb
        ((assign
          "gensym35"
          (proj-idx (local "fwritelnWithLabels_arg115") 0))
         (assign
          "gensym33"
          (proj-idx (local "fwritelnWithLabels_arg115") 1))
         (assign "gensym32" (base "toStringL")))
        (stack-expand
         "gensym30"
         (bb () (tail-call (local "gensym32") (local "gensym33")))
         (bb
          ((assign "gensym31" (tuple (local "gensym35") (local "gensym30"))))
          (tail-call (env "fwriteln2") (local "gensym31")))))
       (local "gensym38"))))))
  (fun
   "printString4"
   (arg "printString_arg120")
   (consts)
   (bb
    ()
    (stack-expand
     "$decltemp$24"
     (bb
      ((assign "gensym56" (base "stdout")))
      (tail-call (local "gensym56") (env "gensym125")))
     (bb
      ((assign
        "gensym55"
        (tuple (local "$decltemp$24") (local "printString_arg120"))))
      (tail-call (env "fwriteln2") (local "gensym55"))))))
  (fun
   "print5"
   (arg "print_arg126")
   (consts)
   (bb
    ((assign "gensym68" (base "toString")))
    (stack-expand
     "gensym67"
     (bb () (tail-call (local "gensym68") (local "print_arg126")))
     (bb () (tail-call (env "printString4") (local "gensym67"))))))
  (fun
   "printWithLabels6"
   (arg "printWithLabels_arg130")
   (consts)
   (bb
    ((assign "gensym80" (base "toStringL")))
    (stack-expand
     "gensym79"
     (bb
      ()
      (tail-call (local "gensym80") (local "printWithLabels_arg130")))
     (bb () (tail-call (env "printString4") (local "gensym79"))))))
  (fun
   "inputLine7"
   (arg "inputLine_arg134")
   (consts)
   (bb
    ()
    (stack-expand
     "$decltemp$38"
     (bb
      ((assign "gensym94" (base "stdin")))
      (tail-call (local "gensym94") (env "gensym125")))
     (bb
      ((assign "gensym91" (base "freadln"))
       (assign "gensym93" (base "stdin")))
      (stack-expand
       "gensym92"
       (bb () (tail-call (local "gensym93") (env "gensym125")))
       (bb () (tail-call (local "gensym91") (local "gensym92"))))))))
  (fun
   "main"
   (arg "$$authorityarg")
   (consts ("gensym124" (int 10)))
   (bb
    ((assign "gensym125" (base "$$authorityarg"))
     (mkclos
      (("gensym125" (local "gensym125")))
      (("fwriteln2" "fwriteln2")
       ("fwritelnWithLabels3" "fwritelnWithLabels3")
       ("printString4" "printString4")
       ("print5" "print5")
       ("printWithLabels6" "printWithLabels6")
       ("inputLine7" "inputLine7")))
     (@
      ("tests/rt/pos/core/fib10.trp" 1 9)
      (mkclos () (("fib40" "fib40")))))
    (@
     ("tests/rt/pos/core/fib10.trp" 5 4)
     (stack-expand
      "gensym123"
      (bb
       ()
       (@
        ("tests/rt/pos/core/fib10.trp" 5 4)
        (tail-call
         (@ ("tests/rt/pos/core/fib10.trp" 5 4) (local "fib40"))
         (@ ("tests/rt/pos/core/fib10.trp" 5 4) (local "gensym124")))))
      (bb
       ()
       (@
        (rt "CaseElimination")
        (ret (@ (rt "CaseElimination") (local "gensym123")))))))))))
