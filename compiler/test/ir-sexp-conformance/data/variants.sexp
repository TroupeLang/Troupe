(troupe-ir-sexp
 2
 (program
  (@
   ("tests/rt/pos/synvar/option-match.trp" 5 9)
   (fun
    "gensym103"
    (arg
     (@ ("tests/rt/pos/synvar/option-match.trp" 5 18) "unwrap_arg242"))
    (consts
     ("gensym117" (int 1))
     ("gensym119" (bool false))
     ("gensym111"
      (string
       "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#NONE"))
     ("gensym113" (bool false))
     ("gensym143" (int 2))
     ("gensym145" (bool false))
     ("gensym137"
      (string
       "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#SOME"))
     ("gensym139" (bool false))
     ("gensym134" (string "pattern match failure in function unwrap")))
    (bb
     ()
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (stack-expand
       "gensym108"
       (bb
        ((assign "gensym120" (un IsTuple (local "unwrap_arg242"))))
        (stack-expand
         "gensym114"
         (bb
          ()
          (if
           (local "gensym120")
           (bb
            ((assign "gensym116" (un TupleLength (local "unwrap_arg242")))
             (assign
              "gensym115"
              (bin
               Eq
               (local "gensym116")
               (@ (rt "CaseElimination") (local "gensym117")))))
            (ret (local "gensym115")))
           (bb () (ret (local "gensym119")))))
         (bb
          ()
          (if
           (local "gensym114")
           (bb
            ((assign "gensym110" (proj-idx (local "unwrap_arg242") 0))
             (assign
              "gensym109"
              (bin
               Eq
               (local "gensym110")
               (@ (rt "CaseElimination") (local "gensym111")))))
            (ret (local "gensym109")))
           (bb () (ret (local "gensym113")))))))
       (bb
        ()
        (if
         (local "gensym108")
         (bb () (ret (env "unwrap_arg141")))
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 5 9)
           (stack-expand
            "gensym133"
            (bb
             ((assign "gensym146" (un IsTuple (local "unwrap_arg242"))))
             (stack-expand
              "gensym140"
              (bb
               ()
               (if
                (local "gensym146")
                (bb
                 ((assign "gensym142" (un TupleLength (local "unwrap_arg242")))
                  (assign
                   "gensym141"
                   (bin
                    Eq
                    (local "gensym142")
                    (@ (rt "CaseElimination") (local "gensym143")))))
                 (ret (local "gensym141")))
                (bb () (ret (local "gensym145")))))
              (bb
               ()
               (if
                (local "gensym140")
                (bb
                 ((assign "gensym136" (proj-idx (local "unwrap_arg242") 0))
                  (assign
                   "gensym135"
                   (bin
                    Eq
                    (local "gensym136")
                    (@ (rt "CaseElimination") (local "gensym137")))))
                 (ret (local "gensym135")))
                (bb () (ret (local "gensym139")))))))
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 5 9)
              (assert-else-error
               (@
                ("tests/rt/pos/synvar/option-match.trp" 5 9)
                (local "gensym133"))
               (bb
                ((assign "gensym130" (proj-idx (local "unwrap_arg242") 1)))
                (ret (local "gensym130")))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 5 9)
                (local "gensym134")))))))))))))))
  (@
   ("tests/rt/pos/synvar/option-match.trp" 5 9)
   (fun
    "unwrap40"
    (arg
     (@ ("tests/rt/pos/synvar/option-match.trp" 5 16) "unwrap_arg141"))
    (consts)
    (bb
     ((@
       ("tests/rt/pos/synvar/option-match.trp" 5 9)
       (mkclos
        (("unwrap_arg141"
          (@
           ("tests/rt/pos/synvar/option-match.trp" 5 9)
           (local "unwrap_arg141"))))
        (("gensym103" "gensym103")))))
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (ret
       (@
        ("tests/rt/pos/synvar/option-match.trp" 5 9)
        (local "gensym103")))))))
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
      (tail-call (local "gensym56") (env "gensym191")))
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
      (tail-call (local "gensym94") (env "gensym191")))
     (bb
      ((assign "gensym91" (base "freadln"))
       (assign "gensym93" (base "stdin")))
      (stack-expand
       "gensym92"
       (bb () (tail-call (local "gensym93") (env "gensym191")))
       (bb () (tail-call (local "gensym91") (local "gensym92"))))))))
  (fun
   "main"
   (arg "$$authorityarg")
   (consts
    ("gensym190" (int 0))
    ("gensym187" (int 42))
    ("gensym188"
     (string
      "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#SOME"))
    ("gensym182" (int 0))
    ("gensym180"
     (string
      "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90#option#NONE"))
    ("gensym157" (string "seven")))
   (bb
    ((assign "gensym191" (base "$$authorityarg"))
     (mkclos
      (("gensym191" (local "gensym191")))
      (("fwriteln2" "fwriteln2")
       ("fwritelnWithLabels3" "fwritelnWithLabels3")
       ("printString4" "printString4")
       ("print5" "print5")
       ("printWithLabels6" "printWithLabels6")
       ("inputLine7" "inputLine7")))
     (@
      ("tests/rt/pos/synvar/option-match.trp" 5 9)
      (mkclos () (("unwrap40" "unwrap40")))))
    (@
     (rt "CaseElimination")
     (stack-expand
      "$decltemp$50"
      (bb
       ()
       (@
        ("tests/rt/pos/synvar/option-match.trp" 8 10)
        (stack-expand
         "gensym184"
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 8 10)
           (tail-call
            (@
             ("tests/rt/pos/synvar/option-match.trp" 8 10)
             (local "unwrap40"))
            (@
             ("tests/rt/pos/synvar/option-match.trp" 8 10)
             (local "gensym190")))))
         (bb
          ((@
            ("tests/rt/pos/synvar/option-match.trp" 8 20)
            (assign
             "gensym189"
             (tuple-variant
              (@
               ("tests/rt/pos/synvar/option-match.trp" 8 20)
               (local "gensym188"))
              (@
               ("tests/rt/pos/synvar/option-match.trp" 8 20)
               (local "gensym187"))))))
          (@
           ("tests/rt/pos/synvar/option-match.trp" 8 20)
           (stack-expand
            "gensym183"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 8 10)
              (tail-call
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 10)
                (local "gensym184"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 10)
                (local "gensym189")))))
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 8 3)
              (tail-call
               (@ ("tests/rt/pos/synvar/option-match.trp" 8 3) (local "print5"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 8 3)
                (local "gensym183")))))))))))
      (bb
       ()
       (@
        (rt "CaseElimination")
        (stack-expand
         "$decltemp$54"
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 9 10)
           (stack-expand
            "gensym179"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 9 10)
              (tail-call
               (@
                ("tests/rt/pos/synvar/option-match.trp" 9 10)
                (local "unwrap40"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 9 10)
                (local "gensym182")))))
            (bb
             ((@
               ("tests/rt/pos/synvar/option-match.trp" 9 19)
               (assign
                "gensym181"
                (tuple-variant
                 (@
                  ("tests/rt/pos/synvar/option-match.trp" 9 19)
                  (local "gensym180"))))))
             (@
              ("tests/rt/pos/synvar/option-match.trp" 9 10)
              (stack-expand
               "gensym178"
               (bb
                ()
                (@
                 ("tests/rt/pos/synvar/option-match.trp" 9 10)
                 (tail-call
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 10)
                   (local "gensym179"))
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 10)
                   (local "gensym181")))))
               (bb
                ()
                (@
                 ("tests/rt/pos/synvar/option-match.trp" 9 3)
                 (tail-call
                  (@ ("tests/rt/pos/synvar/option-match.trp" 9 3) (local "print5"))
                  (@
                   ("tests/rt/pos/synvar/option-match.trp" 9 3)
                   (local "gensym178")))))))))))
         (bb
          ()
          (@
           ("tests/rt/pos/synvar/option-match.trp" 10 35)
           (stack-expand
            "gensym155"
            (bb
             ()
             (@
              ("tests/rt/pos/synvar/option-match.trp" 10 3)
              (tail-call
               (@ ("tests/rt/pos/synvar/option-match.trp" 10 3) (local "print5"))
               (@
                ("tests/rt/pos/synvar/option-match.trp" 10 3)
                (local "gensym157")))))
            (bb
             ()
             (@
              (rt "CaseElimination")
              (ret (@ (rt "CaseElimination") (local "gensym155")))))))))))))))))
