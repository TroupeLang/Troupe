module AtomFolding ( visitProg )
where
import Basics
import Direct
import TroupePositionInfo (Located(..))

visitProg :: Prog -> Prog
visitProg (Prog imports (Atoms atms) groups tm) =
  Prog imports (Atoms atms) groups (visitLTerm atms tm)

-- | Visit a located term
visitLTerm :: [AtomName] -> LTerm -> LTerm
visitLTerm atms (Loc pos term) = Loc pos (visitTerm atms term)

visitTerm :: [AtomName] -> Term -> Term
visitTerm _ (Lit lit) = Lit lit
visitTerm atms (Var nm) =
  if elem nm atms
  then Lit (LAtom nm)
  else Var nm
visitTerm atms (Abs lam) =
  Abs (visitLambda atms lam)
visitTerm atms (Hnd (Handler lpat maybeLPat maybeLTerm lterm)) =
  Hnd (Handler (visitLPattern atms lpat)
       (fmap (visitLPattern atms) maybeLPat)
       (fmap (visitLTerm atms) maybeLTerm)
       (visitLTerm atms lterm))
visitTerm atms (App lt1 lts) =
  App (visitLTerm atms lt1) (map (visitLTerm atms) lts)
visitTerm atms (Let decls lterm) =
  Let (map visitDecl decls) (visitLTerm atms lterm)
  where
    visitDecl (ValDecl lpat lt) = ValDecl (visitLPattern atms lpat) (visitLTerm atms lt)
    visitDecl (FunDecs decs) =
      FunDecs (map visitLFunDecl decs)
    visitLFunDecl (Loc p (FunDecl nm lams)) =
      Loc p (FunDecl nm (map (visitLambda atms) lams))
visitTerm atms (Case lt declTermList) =
  Case (visitLTerm atms lt)
  (map (\(lpat, lterm) -> (visitLPattern atms lpat, visitLTerm atms lterm)) declTermList)
visitTerm atms (If lt1 lt2 lt3) =
  If (visitLTerm atms lt1) (visitLTerm atms lt2) (visitLTerm atms lt3)
visitTerm atms (Tuple lterms tag) =
  Tuple (map (visitLTerm atms) lterms) tag
visitTerm atms (Record fields) = Record (visitFields atms fields)
visitTerm atms (WithRecord le fields) =
    WithRecord (visitLTerm atms le) (visitFields atms fields)
visitTerm atms (ProjField lt f) =
    ProjField (visitLTerm atms lt) f
visitTerm atms (ProjIdx lt idx) =
    ProjIdx (visitLTerm atms lt) idx
visitTerm atms (List lterms) =
  List (map (visitLTerm atms) lterms)
visitTerm atms (ListCons lt1 lt2) =
  ListCons (visitLTerm atms lt1) (visitLTerm atms lt2)
visitTerm atms (Bin op lt1 lt2) =
  Bin op (visitLTerm atms lt1) (visitLTerm atms lt2)
visitTerm atms (Un op lt) =
  Un op (visitLTerm atms lt)
visitTerm atms (Seq lts) =
  Seq (map (visitLTerm atms) lts)
visitTerm atms (Error lt) =
  Error (visitLTerm atms lt)


visitFields :: [AtomName] -> LFields -> LFields
visitFields atms fs = map visitField fs
    where visitField (f, Nothing) = (f, Nothing)
          visitField (f, Just lt) = (f, Just (visitLTerm atms lt))

-- | Visit a located pattern
visitLPattern :: [AtomName] -> LDeclPattern -> LDeclPattern
visitLPattern atms (Loc pos pat) = Loc pos (visitPattern atms pat)

visitPattern :: [AtomName] -> DeclPattern -> DeclPattern
visitPattern atms pat@(VarPattern nm) =
  if elem nm atms
  then ValPattern (LAtom nm)
  else pat
visitPattern _ pat@(ValPattern _) = pat
visitPattern atms (AtPattern lp l) = AtPattern (visitLPattern atms lp) l
visitPattern _ Wildcard = Wildcard
visitPattern atms (TuplePattern lpats) = TuplePattern (map (visitLPattern atms) lpats)
visitPattern atms (ConsPattern lp1 lp2) = ConsPattern (visitLPattern atms lp1) (visitLPattern atms lp2)
visitPattern atms (ListPattern lpats) = ListPattern (map (visitLPattern atms) lpats)
visitPattern atms (RecordPattern fields mode) = RecordPattern (map visitField fields) mode
      where visitField pat@(_, Nothing) = pat
            visitField (f, Just lp) = (f, Just (visitLPattern atms lp))
visitPattern atms (ConPattern qname mpayload) =
  ConPattern qname (fmap (visitLPattern atms) mpayload)
visitPattern _ ErrorPattern = ErrorPattern

visitLambda :: [AtomName] -> Lambda -> Lambda
visitLambda atms (Lambda lpats lterm) =
  Lambda (map (visitLPattern atms) lpats) (visitLTerm atms lterm)
