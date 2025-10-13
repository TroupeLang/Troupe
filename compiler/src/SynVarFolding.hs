module SynVarFolding (visitProg) where

import Basics
import Control.Monad
import Data.List (any, find)
import Direct

visitProg :: Prog -> Prog
visitProg (Prog imports (SyntacticVariants datatypes) tm) =
  let tcs = concat $ map snd datatypes 
  in Prog imports (SyntacticVariants datatypes) (visitTerm tcs tm)

visitTerm :: [SyntacticVariantConstructor] -> Term -> Term
visitTerm svs (Lit lit) = Lit lit
visitTerm svs (Var nm) =
  case find ((==) nm . fst) svs of
    Nothing -> Var nm
    Just (_t, []) -> Tuple [Lit (LString nm)] True -- Convert atom into a tuple
    Just (_t, _) ->
      let var = "v"
      in Abs (Lambda [VarPattern var] (Tuple [ Lit (LString nm)
                                           , Var var
                                           ] True))
visitTerm svs (Abs lam) =
  Abs (visitLambda svs lam)
visitTerm svs (Hnd (Handler pat maybePat maybeTerm term)) =
  Hnd (Handler (visitPattern svs pat)
       (liftM (visitPattern svs) maybePat)
       (liftM (visitTerm svs) maybeTerm)
       (visitTerm svs term))
visitTerm svs (App t1 ts) =
  App (visitTerm svs t1) (map (visitTerm svs) ts)
visitTerm svs (Let decls term) =
  Let (map visitDecl decls) (visitTerm svs term)
  where
    visitDecl (ValDecl pat t pos) = ValDecl (visitPattern svs pat) (visitTerm svs t) pos
    visitDecl (FunDecs decs) =
      FunDecs (map (\(FunDecl nm lams pos) -> (FunDecl nm (map (visitLambda svs) lams) pos)) decs)
visitTerm svs (Case t declTermList p) =
  Case (visitTerm svs t)
  (map (\(pat, term) -> ((visitPattern svs pat), (visitTerm svs term))) declTermList)
  p
visitTerm svs (If t1 t2 t3) =
  If (visitTerm svs t1) (visitTerm svs t2) (visitTerm svs t3)
visitTerm svs (Tuple terms tag) =
  Tuple (map (visitTerm svs) terms) tag
visitTerm svs (Record fields) = Record (visitFields svs fields)
visitTerm svs (WithRecord e fields) = 
    WithRecord (visitTerm svs e) (visitFields svs fields)
visitTerm svs (ProjField t f) =
    ProjField (visitTerm svs t) f
visitTerm svs (ProjIdx t idx) =
    ProjIdx (visitTerm svs t) idx
visitTerm svs (List terms) =
  List (map (visitTerm svs) terms)
visitTerm svs (ListCons t1 t2) =
  ListCons (visitTerm svs t1) (visitTerm svs t2)
visitTerm svs (Bin op t1 t2) =
  Bin op (visitTerm svs t1) (visitTerm svs t2)
visitTerm svs (Un op t) =
  Un op (visitTerm svs t)
visitTerm svs (Seq ts)   = 
  Seq $ map (visitTerm svs) ts
visitTerm svs (Error t) =
  Error (visitTerm svs t)

visitFields :: [SyntacticVariantConstructor]
            -> [(FieldName, Maybe Term)]
            -> [(FieldName, Maybe Term)]
visitFields svs fs  =  map visitField fs   
    where visitField (f, Nothing) = (f, Nothing) 
          visitField (f, Just t) = (f, Just (visitTerm svs t))

visitPattern :: [SyntacticVariantConstructor] -> DeclPattern -> DeclPattern
visitPattern svs pat@(VarPattern nm) =
  if any ((==) (nm, [])) svs
  then TuplePattern [ValPattern (LString nm)] -- Convert atom match into a tuple match
  else pat
visitPattern _ pat@(ValPattern _) = pat
visitPattern svs (AtPattern p l) = AtPattern (visitPattern svs p) l
visitPattern _ pat@Wildcard = pat
visitPattern svs (TuplePattern pats) = TuplePattern (map (visitPattern svs) pats)
visitPattern svs (ConsPattern p1 p2) = ConsPattern (visitPattern svs p1) (visitPattern svs p2)
visitPattern svs (ListPattern pats) = ListPattern (map (visitPattern svs) pats)
visitPattern svs (RecordPattern fields mode) = RecordPattern (map visitField fields) mode
      where visitField pat@(_, Nothing) = pat 
            visitField (f, Just p) = (f, Just (visitPattern svs p))
visitPattern svs (SyntacticVariantPattern nm pat) =
  TuplePattern [ ValPattern (LString nm), visitPattern svs pat ]
                 

visitLambda :: [SyntacticVariantConstructor] -> Lambda -> Lambda
visitLambda svs (Lambda pats term) =
  (Lambda (map (visitPattern svs) pats) (visitTerm svs term))

