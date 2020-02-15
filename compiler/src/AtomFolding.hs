module AtomFolding ( visitProg )
where
import Basics
import Direct
import Data.Maybe
import Control.Monad

visitProg :: Prog -> Prog
visitProg (Prog imports (Atoms atms) tm) =
  Prog imports (Atoms atms) (visitTerm atms tm)

visitTerm :: [AtomName] -> Term -> Term
visitTerm atms (Lit lit) = Lit lit
visitTerm atms (Var nm) =
  if (elem nm atms)
  then Lit (LAtom nm)
  else Var nm
visitTerm atms (Abs lam) =
  Abs (visitLambda atms lam)
visitTerm atms (Hnd (Handler pat maybePat maybeTerm term)) =
  Hnd (Handler (visitPattern atms pat)
       (liftM (visitPattern atms) maybePat)
       (liftM (visitTerm atms) maybeTerm)
       (visitTerm atms term))
visitTerm atms (App t1 ts) =
  App (visitTerm atms t1) (map (visitTerm atms) ts)
visitTerm atms (Let decls term) =
  Let (map visitDecl decls) (visitTerm atms term)
  where
    visitDecl (ValDecl pat t pos) = ValDecl (visitPattern atms pat) (visitTerm atms t) pos
    visitDecl (FunDecs decs) =
      FunDecs (map (\(FunDecl nm lams pos) -> (FunDecl nm (map (visitLambda atms) lams) pos)) decs)
visitTerm atms (Case t declTermList p) =
  Case (visitTerm atms t)
  (map (\(pat, term) -> ((visitPattern atms pat), (visitTerm atms term))) declTermList)
  p
visitTerm atms (If t1 t2 t3) =
  If (visitTerm atms t1) (visitTerm atms t2) (visitTerm atms t3)
visitTerm atms (Tuple terms) =
  Tuple (map (visitTerm atms) terms)
visitTerm atms (List terms) =
  List (map (visitTerm atms) terms)
visitTerm atms (ListCons t1 t2) =
  ListCons (visitTerm atms t1) (visitTerm atms t2)
visitTerm atms (Bin op t1 t2) =
  Bin op (visitTerm atms t1) (visitTerm atms t2)
visitTerm atms (Un op t) =
  Un op (visitTerm atms t)
visitTerm atms (Seq ts)   = 
  Seq $ map (visitTerm atms) ts
visitTerm atms (Error t) =
  Error (visitTerm atms t)

visitPattern :: [AtomName] -> DeclPattern -> DeclPattern
visitPattern atms pat@(VarPattern nm) =
  if (elem nm atms)
  then ValPattern (LAtom nm)
  else pat
visitPattern _ pat@(ValPattern _) = pat
visitPattern atms (AtPattern p l) = AtPattern (visitPattern atms p) l
visitPattern _ pat@Wildcard = pat
visitPattern atms (TuplePattern pats) = TuplePattern (map (visitPattern atms) pats)
visitPattern atms (ConsPattern p1 p2) = ConsPattern (visitPattern atms p1) (visitPattern atms p2)
visitPattern atms (ListPattern pats) = ListPattern (map (visitPattern atms) pats)

visitLambda :: [AtomName] -> Lambda -> Lambda
visitLambda atms (Lambda pats term) =
  (Lambda (map (visitPattern atms) pats) (visitTerm atms term))
