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
  let args = map (("arg" ++) . show) [1..(length pats)]
      argPat = zip (map Var args) pats
      t' = foldM compilePattern (transTerm t) (reverse argPat)
  in Lambda args <$> t'

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
  let accessors = map ((Bin Index v) . (\n -> (Lit (LInt n _srcRT )))) [0..(toInteger ((length pats) - 1))]
  succ' <- foldM compilePattern succ (reverse (zip accessors pats))
  return $ ifpat (Bin And (Un IsTuple v) (Bin Eq (Un Length v) (Lit (LInt (toInteger (length pats)) _srcRT)))) succ' fail
compilePattern succ (v, S.ListPattern pats) = do
  fail <- ask
  let accessors = map ((Bin Index v) . (\n -> (Lit (LInt n _srcRT)))) [0..(toInteger ((length pats) - 1))]
  succ' <- foldM compilePattern succ (reverse (zip accessors pats))
  return $ ifpat (Bin And (Un IsList v) (Bin Eq (Un Length v) (Lit (LInt (toInteger (length pats)) _srcRT)))) succ' fail
compilePattern succ (v, S.ConsPattern p1 p2) = do
  fail <- ask
  succ' <- compilePattern succ (Un Head v, p1)
  succ'' <- compilePattern succ' (Un Tail v, p2)
  return $ ifpat (Bin And (Un IsList v) (Bin Gt (Un Length v) (Lit (LInt 0 _srcRT) ))) succ'' fail

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
transTerm (S.List tms) = T.List (map transTerm tms)
transTerm (S.ListCons t1 t2) = T.ListCons (transTerm t1) (transTerm t2)
transTerm (S.Bin op t1 t2) = Bin op (transTerm t1) (transTerm t2)
transTerm (S.Un op t) = Un op (transTerm t)
