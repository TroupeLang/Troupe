{-# LANGUAGE  FlexibleContexts  #-}
{-# LANGUAGE LambdaCase #-}
module CaseElimination ( trans )
where

import Basics
import qualified Direct as S
import DirectWOPats as T
import CompileMode
import TroupePositionInfo

import Control.Monad.Reader

trans :: CompileMode -> S.Prog -> T.Prog
trans mode (S.Prog imports atms tm) =
  let tm' = case mode of
              Normal ->
                  S.Let [ S.ValDecl (S.VarPattern "authority") (S.Var "$$authorityarg") _srcRT ]
                        tm
              Export -> tm
  in
    T.Prog imports (transAtoms atms) (transTerm tm')

transAtoms (S.Atoms atms) = T.Atoms atms

transLit (S.LInt n pi)    = T.LInt n pi
transLit (S.LString s) = T.LString s
transLit (S.LLabel s)  = T.LLabel s
transLit (S.LUnit)     = T.LUnit
transLit (S.LBool b)   = T.LBool b
transLit (S.LAtom a)   = T.LAtom a


transLambda_aux (S.Lambda pats t) =
  let args = map (("$arg" ++) . show) [1..(length pats)]
      argPat = zip (map Var args) pats
      t' = foldM compilePattern (transTerm t) (reverse argPat)
  in Lambda args <$> t'

transLambda :: S.Lambda -> Lambda
transLambda lam = do
  runReader (transLambda_aux lam)  (Error (Lit (LString "pattern match failed") )  NoPos )


{-- 2019-01-31 desugaring handlers; AA

Given `hn pat1 | pat2 when e1 => e2`, we desugar it to

fn (input) => 
  case input of 
      (pat1, pat2) => if e1 then (0, fn _ => e2)
                            else (1, ())
      _ => HNPATFAIL (1, ())


Here, HNPATSUCC and HNPATFAIL are two runtime functions. The semantics
is that before the handler is called, the runtime sets the thread
flag to the "HANDLER MODE" that will prevent side effects (including 
picking messages from the mailbox and sending messages to other threads).
Calling PATSUCC will bring the thread back to normal mode. 


--}


_srcRT = RTGen "CaseElimination"

transHandler :: S.Handler -> Lambda
transHandler (S.Handler pat1 mbpat2 guard body) =
  let argInput  = "$input"
      pat2 = case mbpat2 of
              Just pat2 -> pat2
              Nothing   -> S.Wildcard      
      lambdaPats = [S.VarPattern argInput] 
      callFailure = S.Tuple [S.Lit (S.LInt 1 _srcRT), S.Lit S.LUnit ]  
      body' =  S.Tuple[ S.Lit (S.LInt 0 _srcRT), S.Abs ( S.Lambda [S.Wildcard] body )  ]
      guardCheck = case guard of
         Nothing -> body'
         Just g -> S.If g body' callFailure
      lamBody = S.Case (S.Var argInput) [( S.TuplePattern [pat1, pat2], guardCheck), (S.Wildcard, callFailure)] _srcRT
      lambda = S.Lambda lambdaPats lamBody
  in transLambda lambda
  

-- 2018-09-28: AA: a bit of a hack: making sure that the last pattern is
-- compiled into an assertion instead of an ifthenelse
ifpat t1 t2 t3 = 
  case t3 of 
    Error t3' pos -> AssertElseError t1 t2 t3' pos
    _ -> If t1 t2 t3
  
  
-- 2023-06-21: FW: an alternative would be to add a pseudo pattern at the end of each pattern list,
-- which includes the error message and always compiles to an error term.
-- | Compile pattern matching to conditionals and assertions.
-- succ: term corresponding to a successful match
-- v: the term to be assigned to the pattern
-- The Reader monad stores the error term.
compilePattern :: T.Term -> (T.Term, S.DeclPattern) -> Reader T.Term T.Term
compilePattern succ (v, (S.AtPattern p l))  = do
  fail <- ask
  succ' <- compilePattern succ (v, p)
  return $ ifpat (Bin Eq (Un LevelOf v) (Lit (LLabel l))) succ' fail
compilePattern succ (v, (S.VarPattern var)) = return $ Let [T.ValDecl var v] succ
compilePattern succ (v, (S.ValPattern lit)) = do
  fail <- ask
  return $ ifpat (Bin Eq v (Lit (transLit lit))) succ fail
compilePattern succ (v, S.Wildcard) = return $ Let [T.ValDecl "$wildcard" v] succ
compilePattern succ (v, S.TuplePattern pats) = do
  fail <- ask
  -- Accessors for the value to be assigned to the patterns.
  let accessors = map (ProjIdx v) [0..(fromIntegral (length pats) - 1)]
  -- Compile the nested patterns, combining the resulting terms for the respective patterns so that the left-most is evaluated first.
  succ' <- foldM compilePattern succ (reverse (zip accessors pats))
  -- The expression for the tuple pattern checks whether the to-be-assigned value is a tuple with the correct length,
  -- and then executes the expression succ' which checks the nested patterns.
  return $ ifpat (Bin And (Un IsTuple v) (Bin Eq (Un TupleLength v) (Lit (LInt (toInteger (length pats)) _srcRT)))) succ' fail
-- TODO Generate more efficient code:
-- Decompose the list v according to the pattern with a DFS pass.
-- This would benefit from an "is empty" operation (to not having to use the RT-dispatched equals).
-- A potentially expensive length calculation is then unnecessary.
-- However, this is more complicated, as would need unique name generation, also for potentially nested list patterns.
compilePattern succ (v, S.ListPattern pats) = do
  fail <- ask
  -- Accessors for the value to be assigned to the patterns.
  let accessors = map (Un Head) $ iterate (Un Tail) v
  -- Compile the nested patterns, combining the resulting terms for the respective patterns so that the left-most is evaluated first.
  succ' <- foldM compilePattern succ (reverse (zip accessors pats)) -- pairs of pattern (the nested ones in the list) and term accessing the value at the corresponding index in the list term
  -- The expression for the list pattern checks whether the to-be-assigned value is a list with the correct length,
  -- and then executes the expression succ' which checks the nested patterns.
  return $ ifpat (Bin And (Un IsList v) (Bin Eq (Un ListLength v) (Lit (LInt (toInteger (length pats)) _srcRT)))) succ' fail
compilePattern succ (v, S.ConsPattern p1 p2) = do
  fail <- ask
  succ' <- compilePattern succ (Un Head v, p1)
  succ'' <- compilePattern succ' (Un Tail v, p2)
  -- TODO Avoid list length (potentially expensive). Implement similarly to the improved list pattern (see above).
  return $ ifpat (Bin And (Un IsList v) (Bin Gt (Un ListLength v) (Lit (LInt 0 _srcRT) ))) succ'' fail
compilePattern succ (v, S.RecordPattern fieldPatterns) = do
  fail <- ask  
  succ' <- foldM compileField succ  (reverse fieldPatterns)
  return $ ifpat (Un IsRecord v) succ' fail
    where ifHasField f k = do 
              succ' <- k 
              fail <- ask 
              let f' = Lit (LString f )
              return $ ifpat (Bin HasField v  f' ) succ' fail 

          compileField succ (f, Just p) = do 
              ifHasField f $ compilePattern succ (T.ProjField v f, p)
              
          compileField succ (f, Nothing) = do 
              ifHasField f $ compilePattern succ (T.ProjField v f, S.VarPattern f)
  


-- | Tranform a declaration, compiling patterns into terms.
-- When there are multiple patterns like in functions or a case expression,
-- they are folded into a nested term, with an error expression innermost (after the last check).
-- The error expression is therefore passed as state of a Reader monad.
transDecl :: S.Decl -> Term -> Term
transDecl (S.ValDecl pat t pos) succ = do
  let temp = "$decltemp$"
  Let [ValDecl temp (transTerm t)]
    $ runReader (compilePattern succ ((Var temp ), pat)) (Error (Lit (LString "pattern match failure in let declaration")) pos)
transDecl (S.FunDecs fundecs) succ =
  Let [FunDecs (map transFunDecl fundecs)] succ
  where
    argLength ((S.Lambda args _):_) = length args
    argLength [] = 0
    transFunDecl (S.FunDecl f lams pos) =
      let lams' = map (transLambda_aux . (\(S.Lambda args e) -> S.Lambda [S.TuplePattern args] e)) lams
          names = map (((f ++ "_pat") ++) . show) [1..(length lams)]
          args =  map (((f ++ "_arg") ++) . show) [1..(argLength lams)]
          args' =  Tuple (map Var args)
          zipped = zip names lams'
          (fst, decls) = foldr (\(n, l) (fail, decls) -> 
                ( (App (Var n) [args'])
                , (ValDecl n (Abs (runReader l fail))) 
                  :decls))                  
                        (Error (Lit (LString $ "pattern match failure in function " ++ f)) pos
                        , []) zipped
      in FunDecl f (Lambda args (Let (reverse decls) fst))

transTerm :: S.Term -> Term
transTerm (S.Lit lit) = T.Lit $ transLit lit
transTerm (S.Var v) = T.Var v
transTerm (S.Abs l) = T.Abs $ transLambda l
transTerm (S.Hnd h) = T.Abs $ transHandler h
transTerm (S.App t1 args) = T.App (transTerm t1) (map transTerm args)
transTerm (S.Let decls t) =
  foldr transDecl (transTerm t) decls
transTerm (S.Case t cases pos) = do
  let e = foldr (\(pat, succ) fail -> runReader (compilePattern (transTerm succ) (Var "casevar", pat)) fail) (Error (Lit (LString "pattern match failure in case expression")) pos) cases
  Let [ValDecl "casevar" (transTerm t)] e
transTerm (S.If t1 t2 t3) =
  If (transTerm t1) (transTerm t2) (transTerm t3)
transTerm (S.Tuple tms) = T.Tuple (map transTerm tms)
transTerm (S.Record fields) = T.Record (transFields fields)
transTerm (S.WithRecord e fields) = T.WithRecord (transTerm e) (transFields fields)
transTerm (S.ProjField t f) = T.ProjField (transTerm t) f
transTerm (S.ProjIdx t idx) = T.ProjIdx (transTerm t) idx
transTerm (S.List tms) = T.List (map transTerm tms)
transTerm (S.ListCons t1 t2) = T.ListCons (transTerm t1) (transTerm t2)
transTerm (S.Bin op t1 t2) = Bin op (transTerm t1) (transTerm t2)
transTerm (S.Un op t) = Un op (transTerm t)
transTerm (S.Seq ts) = transTerm $
    case reverse ts of
        [t] -> t
        body:ts_rev ->
          let decls = map (\t -> S.ValDecl S.Wildcard t NoPos) (reverse ts_rev)
          in  S.Let decls body
        []  -> error "impossible case: sequence of empty terms"

transTerm (S.Error _) = error "impossible case: error"

transFields = map $ \case
  (f, Nothing) -> (f, T.Var f)
  (f, Just t) -> (f, transTerm t)