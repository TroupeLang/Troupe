(troupe-ir-sexp
 2
 (program
  (@
   ("tests/rt/pos/synvar/option-match.trp" 5 9)
   (fun
    "gensym1"
    (arg
     (@ ("tests/rt/pos/synvar/option-match.trp" 5 18) "unwrap_arg24"))
    (consts
     ("gensym15" (int 1))
     ("gensym17" (bool false))
     ("gensym9"
      (string
       "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#NONE"))
     ("gensym11" (bool false))
     ("gensym41" (int 2))
     ("gensym43" (bool false))
     ("gensym35"
      (string
       "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#SOME"))
     ("gensym37" (bool false))
     ("gensym32" (string "pattern match failure in function unwrap")))
    (bb
     ()
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (stack-expand
       "gensym6"
       (bb
        ((assign "gensym18" (un IsTuple (local "unwrap_arg24"))))
        (stack-expand
         "gensym12"
         (bb
          ()
          (if
           (local "gensym18")
           (bb
            ((assign "gensym14" (un TupleLength (local "unwrap_arg24")))
             (assign
              "gensym13"
              (bin
               Eq
               (local "gensym14")
               (@ (rt "CaseElimination") (local "gensym15")))))
            (ret (local "gensym13")))
           (bb () (ret (local "gensym17")))))
         (bb
          ()
          (if
           (local "gensym12")
           (bb
            ((assign "gensym8" (proj-idx (local "unwrap_arg24") 0))
             (assign
              "gensym7"
              (bin
               Eq
               (local "gensym8")
               (@ (rt "CaseElimination") (local "gensym9")))))
            (ret (local "gensym7")))
           (bb () (ret (local "gensym11")))))))
       (bb
        ()
        (if
         (local "gensym6")
         (bb () (ret (env "unwrap_arg13")))
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 5 9)
           (stack-expand
            "gensym31"
            (bb
             ((assign "gensym44" (un IsTuple (local "unwrap_arg24"))))
             (stack-expand
              "gensym38"
              (bb
               ()
               (if
                (local "gensym44")
                (bb
                 ((assign "gensym40" (un TupleLength (local "unwrap_arg24")))
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
               (if
                (local "gensym38")
                (bb
                 ((assign "gensym34" (proj-idx (local "unwrap_arg24") 0))
                  (assign
                   "gensym33"
                   (bin
                    Eq
                    (local "gensym34")
                    (@ (rt "CaseElimination") (local "gensym35")))))
                 (ret (local "gensym33")))
                (bb () (ret (local "gensym37")))))))
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 5 9)
              (assert-else-error
               (@ ("tests/rt/pos/synvar/option-match.trp" 5 9) (local "gensym31"))
               (bb
                ((assign "gensym28" (proj-idx (local "unwrap_arg24") 1)))
                (ret (local "gensym28")))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 5 9)
                (local "gensym32")))))))))))))))
  (@
   ("tests/rt/pos/synvar/option-match.trp" 5 9)
   (fun
    "unwrap2"
    (arg
     (@ ("tests/rt/pos/synvar/option-match.trp" 5 16) "unwrap_arg13"))
    (consts)
    (bb
     ((@
       ("tests/rt/pos/synvar/option-match.trp" 5 9)
       (mkclos
        (("unwrap_arg13"
          (@
           ("tests/rt/pos/synvar/option-match.trp" 5 9)
           (local "unwrap_arg13"))))
        (("gensym1" "gensym1")))))
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (ret
       (@
        ("tests/rt/pos/synvar/option-match.trp" 5 9)
        (local "gensym1")))))))
  (fun
   "main"
   (arg "$$authorityarg")
   (consts
    ("gensym91" (int 0))
    ("gensym88" (int 42))
    ("gensym89"
     (string
      "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#SOME"))
    ("gensym82" (int 0))
    ("gensym80"
     (string
      "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#NONE"))
    ("gensym56" (string "seven")))
   (bb
    ((assign "gensym92" (base "$$authorityarg"))
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (mkclos () (("unwrap2" "unwrap2")))))
    (@
     (rt "CaseElimination")
     (stack-expand
      "$decltemp$12"
      (bb
       ((@
         ("tests/rt/pos/synvar/option-match.trp" 8 3)
         (assign "gensym83" (base "print"))))
       (@
        ("tests/rt/pos/synvar/option-match.trp" 8 10)
        (stack-expand
         "gensym85"
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 8 10)
           (tail-call
            (@ ("tests/rt/pos/synvar/option-match.trp" 8 10) (local "unwrap2"))
            (@
             ("tests/rt/pos/synvar/option-match.trp" 8 10)
             (local "gensym91")))))
         (bb
          ((@
            ("tests/rt/pos/synvar/option-match.trp" 8 20)
            (assign
             "gensym90"
             (tuple-variant
              (@
               ("tests/rt/pos/synvar/option-match.trp" 8 20)
               (local "gensym89"))
              (@
               ("tests/rt/pos/synvar/option-match.trp" 8 20)
               (local "gensym88"))))))
          (@
           ("tests/rt/pos/synvar/option-match.trp" 8 20)
           (stack-expand
            "gensym84"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 8 10)
              (tail-call
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 10)
                (local "gensym85"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 10)
                (local "gensym90")))))
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 8 3)
              (tail-call
               (@ ("tests/rt/pos/synvar/option-match.trp" 8 3) (local "gensym83"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 3)
                (local "gensym84")))))))))))
      (bb
       ()
       (@
        (rt "CaseElimination")
        (stack-expand
         "$decltemp$16"
         (bb
          ((@
            ("tests/rt/pos/synvar/option-match.trp" 9 3)
            (assign "gensym77" (base "print"))))
          (@
           ("tests/rt/pos/synvar/option-match.trp" 9 10)
           (stack-expand
            "gensym79"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 9 10)
              (tail-call
               (@ ("tests/rt/pos/synvar/option-match.trp" 9 10) (local "unwrap2"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 9 10)
                (local "gensym82")))))
            (bb
             ((@
               ("tests/rt/pos/synvar/option-match.trp" 9 19)
               (assign
                "gensym81"
                (tuple-variant
                 (@
                  ("tests/rt/pos/synvar/option-match.trp" 9 19)
                  (local "gensym80"))))))
             (@
              ("tests/rt/pos/synvar/option-match.trp" 9 10)
              (stack-expand
               "gensym78"
               (bb
                ()
                (@
                 ("tests/rt/pos/synvar/option-match.trp" 9 10)
                 (tail-call
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 10)
                   (local "gensym79"))
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 10)
                   (local "gensym81")))))
               (bb
                ()
                (@
                 ("tests/rt/pos/synvar/option-match.trp" 9 3)
                 (tail-call
                  (@ ("tests/rt/pos/synvar/option-match.trp" 9 3) (local "gensym77"))
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 3)
                   (local "gensym78")))))))))))
         (bb
          ((@
            ("tests/rt/pos/synvar/option-match.trp" 10 3)
            (assign "gensym54" (base "print"))))
          (@
           ("tests/rt/pos/synvar/option-match.trp" 10 35)
           (stack-expand
            "gensym53"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 10 3)
              (tail-call
               (@
                ("tests/rt/pos/synvar/option-match.trp" 10 3)
                (local "gensym54"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 10 3)
                (local "gensym56")))))
            (bb
             ()
             (@
              (rt "CaseElimination")
              (ret (@ (rt "CaseElimination") (local "gensym53")))))))))))))))))
