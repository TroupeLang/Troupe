(troupe-ir-sexp
 2
 (program
  (fun
   "main"
   (arg "$$authorityarg")
   (consts ("gensym5" (float 3.14)) ("gensym4" (float 2.5)))
   (bb
    ((assign "gensym6" (base "$$authorityarg"))
     (@
      ("tests/rt/pos/core/float_literals.trp" 3 15)
      (assign
       "gensym3"
       (bin
        Plus
        (@ ("tests/rt/pos/core/float_literals.trp" 3 13) (local "gensym5"))
        (@
         ("tests/rt/pos/core/float_literals.trp" 3 17)
         (local "gensym4")))))
     (@
      ("tests/rt/pos/core/float_literals.trp" 4 4)
      (assign "gensym2" (base "print"))))
    (@
     ("tests/rt/pos/core/float_literals.trp" 4 4)
     (stack-expand
      "gensym1"
      (bb
       ()
       (@
        ("tests/rt/pos/core/float_literals.trp" 4 4)
        (tail-call
         (@ ("tests/rt/pos/core/float_literals.trp" 4 4) (local "gensym2"))
         (@
          ("tests/rt/pos/core/float_literals.trp" 4 4)
          (local "gensym3")))))
      (bb
       ()
       (@
        (rt "CaseElimination")
        (ret (@ (rt "CaseElimination") (local "gensym1")))))))))))
