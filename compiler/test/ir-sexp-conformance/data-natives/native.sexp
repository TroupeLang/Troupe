(troupe-ir-sexp
 2
 (program
  (fun
   "main"
   (arg "$$authorityarg")
   (consts)
   (bb
    ((assign "a" (lib "native:Zeta" "zOp"))
     (assign "b" (lib "native:FFIDemo" "ffiDemoGreet"))
     (assign "c" (lib "native:FFIDemo" "ffiDemoAdd")))
    (ret (local "b")))))
 (natives "FFIDemo" "Zeta"))
