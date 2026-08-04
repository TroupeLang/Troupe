(troupe-ir-sexp
 2
 (program
  (fun
   "main"
   (arg "$$authorityarg")
   (consts
    ("gensym19" (dclabel #true (tag "attacker")))
    ("gensym9" (bool true))
    ("gensym7" (bool true))
    ("gensym5" (bool true))
    ("gensym3" (bool true)))
   (bb
    ((assign "gensym20" (base "$$authorityarg")))
    (@
     ("tests/rt/pos/ifc/dclabel-whitespace.trp" 3 18)
     (stack-expand
      "$decltemp$12"
      (bb
       ((@
         ("tests/rt/pos/ifc/dclabel-whitespace.trp" 9 5)
         (assign "gensym14" (base "print"))))
       (@
        ("tests/rt/pos/ifc/dclabel-whitespace.trp" 9 5)
        (tail-call
         (@
          ("tests/rt/pos/ifc/dclabel-whitespace.trp" 9 5)
          (local "gensym14"))
         (@
          ("tests/rt/pos/ifc/dclabel-whitespace.trp" 9 5)
          (local "gensym19")))))
      (bb
       ()
       (@
        (rt "CaseElimination")
        (stack-expand
         "$decltemp$14"
         (bb
          ((@
            ("tests/rt/pos/ifc/dclabel-whitespace.trp" 10 5)
            (assign "gensym13" (base "print"))))
          (@
           ("tests/rt/pos/ifc/dclabel-whitespace.trp" 10 5)
           (tail-call
            (@
             ("tests/rt/pos/ifc/dclabel-whitespace.trp" 10 5)
             (local "gensym13"))
            (@
             ("tests/rt/pos/ifc/dclabel-whitespace.trp" 10 5)
             (local "gensym19")))))
         (bb
          ()
          (@
           (rt "CaseElimination")
           (stack-expand
            "$decltemp$16"
            (bb
             ((@
               ("tests/rt/pos/ifc/dclabel-whitespace.trp" 11 5)
               (assign "gensym12" (base "print"))))
             (@
              ("tests/rt/pos/ifc/dclabel-whitespace.trp" 11 5)
              (tail-call
               (@
                ("tests/rt/pos/ifc/dclabel-whitespace.trp" 11 5)
                (local "gensym12"))
               (@
                ("tests/rt/pos/ifc/dclabel-whitespace.trp" 11 5)
                (local "gensym19")))))
            (bb
             ()
             (@
              (rt "CaseElimination")
              (stack-expand
               "$decltemp$18"
               (bb
                ((@
                  ("tests/rt/pos/ifc/dclabel-whitespace.trp" 12 5)
                  (assign "gensym11" (base "print"))))
                (@
                 ("tests/rt/pos/ifc/dclabel-whitespace.trp" 12 5)
                 (tail-call
                  (@
                   ("tests/rt/pos/ifc/dclabel-whitespace.trp" 12 5)
                   (local "gensym11"))
                  (@
                   ("tests/rt/pos/ifc/dclabel-whitespace.trp" 12 5)
                   (local "gensym19")))))
               (bb
                ()
                (@
                 (rt "CaseElimination")
                 (stack-expand
                  "$decltemp$20"
                  (bb
                   ((@
                     ("tests/rt/pos/ifc/dclabel-whitespace.trp" 13 5)
                     (assign "gensym10" (base "print"))))
                   (@
                    ("tests/rt/pos/ifc/dclabel-whitespace.trp" 13 5)
                    (tail-call
                     (@
                      ("tests/rt/pos/ifc/dclabel-whitespace.trp" 13 5)
                      (local "gensym10"))
                     (@
                      ("tests/rt/pos/ifc/dclabel-whitespace.trp" 13 5)
                      (local "gensym19")))))
                  (bb
                   ()
                   (@
                    (rt "CaseElimination")
                    (stack-expand
                     "$decltemp$22"
                     (bb
                      ((@
                        ("tests/rt/pos/ifc/dclabel-whitespace.trp" 14 5)
                        (assign "gensym8" (base "print"))))
                      (@
                       ("tests/rt/pos/ifc/dclabel-whitespace.trp" 14 5)
                       (tail-call
                        (@
                         ("tests/rt/pos/ifc/dclabel-whitespace.trp" 14 5)
                         (local "gensym8"))
                        (@
                         ("tests/rt/pos/ifc/dclabel-whitespace.trp" 14 5)
                         (local "gensym9")))))
                     (bb
                      ()
                      (@
                       (rt "CaseElimination")
                       (stack-expand
                        "$decltemp$24"
                        (bb
                         ((@
                           ("tests/rt/pos/ifc/dclabel-whitespace.trp" 15 5)
                           (assign "gensym6" (base "print"))))
                         (@
                          ("tests/rt/pos/ifc/dclabel-whitespace.trp" 15 5)
                          (tail-call
                           (@
                            ("tests/rt/pos/ifc/dclabel-whitespace.trp" 15 5)
                            (local "gensym6"))
                           (@
                            ("tests/rt/pos/ifc/dclabel-whitespace.trp" 15 5)
                            (local "gensym7")))))
                        (bb
                         ()
                         (@
                          (rt "CaseElimination")
                          (stack-expand
                           "$decltemp$26"
                           (bb
                            ((@
                              ("tests/rt/pos/ifc/dclabel-whitespace.trp" 16 5)
                              (assign "gensym4" (base "print"))))
                            (@
                             ("tests/rt/pos/ifc/dclabel-whitespace.trp" 16 5)
                             (tail-call
                              (@
                               ("tests/rt/pos/ifc/dclabel-whitespace.trp" 16 5)
                               (local "gensym4"))
                              (@
                               ("tests/rt/pos/ifc/dclabel-whitespace.trp" 16 5)
                               (local "gensym5")))))
                           (bb
                            ((@
                              ("tests/rt/pos/ifc/dclabel-whitespace.trp" 17 5)
                              (assign "gensym2" (base "print"))))
                            (@
                             ("tests/rt/pos/ifc/dclabel-whitespace.trp" 17 5)
                             (stack-expand
                              "gensym1"
                              (bb
                               ()
                               (@
                                ("tests/rt/pos/ifc/dclabel-whitespace.trp" 17 5)
                                (tail-call
                                 (@
                                  ("tests/rt/pos/ifc/dclabel-whitespace.trp" 17 5)
                                  (local "gensym2"))
                                 (@
                                  ("tests/rt/pos/ifc/dclabel-whitespace.trp" 17 5)
                                  (local "gensym3")))))
                              (bb
                               ()
                               (@
                                (rt "CaseElimination")
                                (ret
                                 (@
                                  (rt "CaseElimination")
                                  (local "gensym1")))))))))))))))))))))))))))))))))))
