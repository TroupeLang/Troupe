{-# LANGUAGE  FlexibleContexts  #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module CaseElimination ( trans )
where

import Basics
import qualified Direct as S
import Direct (RecordPatternMode(..))
import DirectWOPats as T
import CompileMode
import InternalError (internalError)
import TroupePositionInfo (Located(..), getLoc, unLoc, PosInf(..))

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (foldM)
import Data.List (nub, (\\))

type Trans = Except String

-- | Lower the Direct AST (with syntactic variants already folded away by
-- 'SynVarFolding') to the pattern-free IR. The declaration-group list is
-- empty and no 'S.ConPattern' remains by this point.
trans :: CompileMode -> S.Prog -> Trans T.Prog
trans compileMode (S.Prog imports _groups tm) = do
  let tm' = case compileMode of
        CompileMode.Library -> tm
        _                   ->
          -- Create Located wrappers for generated code
          let authPat = Loc _srcRT (S.VarPattern "authority")
              authVar = Loc NoPos (S.Var "$$authorityarg")
          in Loc NoPos (S.Let [ S.ValDecl authPat authVar ] tm)
  tm'' <- transLTerm tm'
  return (T.Prog imports tm'')

transLit :: S.Lit -> T.Lit
transLit (S.LNumeric n) = T.LNumeric (transNumeric n)
  where
    transNumeric (S.NumInt i) = NumInt i
    transNumeric (S.NumFloat f) = NumFloat f
transLit (S.LString s) = T.LString s
transLit (S.LLabel s)  = T.LLabel s
transLit (S.LDCLabel dc)  = T.LDCLabel dc
transLit (S.LUnit)     = T.LUnit
transLit (S.LBool b)   = T.LBool b


transLambda_aux :: S.Lambda -> ReaderT T.LTerm Trans Lambda
transLambda_aux (S.Lambda pats body) = do
  let args = map (("$arg" ++) . show) [1..(length pats)]
      -- Create Located variable names using positions from the original patterns
      argsWithPos = zipWith (\a lp -> Loc (getLoc lp) a) args pats
      argPat = zip (map (\a -> Loc NoPos (Var a)) args) pats
  body' <- lift (transLTerm body)
  result <- foldM compilePattern body' (reverse argPat)
  return (Lambda argsWithPos result)

transLambdaWithError :: S.Lambda -> T.LTerm -> Trans Lambda
transLambdaWithError lam errorTerm =
  runReaderT (transLambda_aux lam) errorTerm

transLambda :: S.Lambda -> Trans Lambda
transLambda lam =
  transLambdaWithError lam (Loc NoPos (Error (Loc NoPos (Lit (LString "pattern match failed")))))


{-- 2019-01-31 desugaring handlers; AA

-- 2025-05-10: AA: See Section on Input Handlers in
-- Troupe 2.0 system and security model writeup
-- as a guide towards a rewrite

Given `hn pat1 | pat2 when e1 => e2`, we desugar it to

fn (input) =>
  case input of
      (pat1, pat2) => if e1 then (0, fn _ => e2)
                            else (1, ())
      _ => HNPATFAIL (1, ())

.
Here, HNPATSUCC and HNPATFAIL are two runtime functions. The semantics
is that before the handler is called, the runtime sets the thread
flag to the "HANDLER MODE" that will prevent side effects (including
picking messages from the mailbox and sending messages to other threads).
Calling PATSUCC will bring the thread back to normal mode.


--}


_srcRT = RTGen "CaseElimination"

transHandler :: S.Handler -> Trans Lambda
transHandler (S.Handler pat1 mbpat2 guard body) = do
  let argInput  = "$input"
      -- Helper to create Located wrappers at RTGen position
      lp = Loc _srcRT  -- for patterns
      lt = Loc _srcRT  -- for terms
      pat2 = case mbpat2 of
              Just p2 -> p2
              Nothing -> lp S.Wildcard
      lambdaPats = [lp (S.VarPattern argInput)]
      callFailure = lt (S.Tuple [lt (S.Lit (S.LNumeric (S.NumInt 1))), lt (S.Lit S.LUnit)] False)
      body' = lt (S.Tuple [lt (S.Lit (S.LNumeric (S.NumInt 0))), lt (S.Abs (S.Lambda [lp S.Wildcard] body))] False)
      guardCheck = case guard of
         Nothing -> body'
         Just g -> lt (S.If g body' callFailure)
      lamBody = lt (S.Case (lt (S.Var argInput))
                           [(lp (S.TuplePattern [pat1, pat2]), guardCheck),
                            (lp S.Wildcard, callFailure)])
      lambda = S.Lambda lambdaPats lamBody
  transLambda lambda


-- 2018-09-28: AA: a bit of a hack: making sure that the last pattern is
-- compiled into an assertion instead of an ifthenelse
-- Now uses Located wrapper position for error position
ifpat :: PosInf -> T.LTerm -> T.LTerm -> T.LTerm -> T.LTerm
ifpat pos lt1 lt2 lt3 =
  case unLoc lt3 of
    Error lt3' -> Loc pos (AssertElseError lt1 lt2 lt3')
    _ -> Loc pos (If lt1 lt2 lt3)


-- 2023-06-21: FW: an alternative would be to add a pseudo pattern at the end of each pattern list,
-- which includes the error message and always compiles to an error term.
-- | Compile pattern matching to conditionals and assertions.
--
-- The failure term (held in the Reader environment) is spliced into a clause
-- exactly once: 'collectPattern' gathers a clause's refutable tests into an
-- ordered list of conjuncts and a binding wrapper, and 'compilePattern' joins
-- the conjuncts with a single short-circuit '&&' and emits one 'ifpat'. Because
-- the failure application then occurs once per clause (census 1), the downstream
-- shrinking beta-reducer can inline the per-clause dispatch lambdas, so a clause
-- no longer survives closure conversion as a per-call closure allocation.
--
-- succ: term corresponding to a successful match
-- lv: the Located term to be assigned to the pattern
-- lpat: the Located pattern (we unwrap to get both pattern and position)
-- The Reader monad stores the error term (now also Located).
compilePattern :: T.LTerm -> (T.LTerm, S.LDeclPattern) -> ReaderT T.LTerm Trans T.LTerm
compilePattern succ (lv, lpat) = do
  fail <- ask
  (tests, wrap) <- lift (collectPattern (lv, lpat))
  let pos = getLoc lv
  case tests of
    -- An irrefutable clause (no tests, e.g. a variable or wildcard pattern)
    -- emits no conditional at all, just its bindings.
    [] -> return (wrap succ)
    -- Otherwise a single 'ifpat' guarded by the conjunction of all tests, with
    -- the bindings wrapping the success continuation. 'ifpat' turns fail = Error
    -- into a single 'AssertElseError', so the last clause has exactly one error site.
    --
    -- The nested emission reported a refuted pattern at the failure term's own
    -- position (the clause's error message carries the clause position). Since the
    -- accessor values carry no source position of their own ('getLoc lv' is NoPos
    -- for a function argument), take the failure term's position for the assertion
    -- when it is an error, so the reported error location is unchanged.
    _  -> let cond = foldr1 (\a b -> Loc pos (Bin And a b)) tests
              errPos = case unLoc fail of
                         Error _ -> getLoc fail
                         _       -> pos
          in return (ifpat errPos cond (wrap succ) fail)

-- | Gather a pattern's refutable tests and its variable bindings.
--
-- Returns @(tests, wrap)@ where @tests@ are the shape/equality/label conjuncts
-- in the exact left-to-right order the nested emission evaluated them (parent
-- shape test before child tests; children left-to-right), and @wrap@ threads the
-- pattern's variable bindings (pure @Let@s of projections) around the success
-- continuation. The tests never mention the bound names, so hoisting every
-- binding inside the success branch preserves scope: guards and bodies still see
-- all bindings, and each test still sees only projections of the matched value.
collectPattern :: (T.LTerm, S.LDeclPattern) -> Trans ([T.LTerm], T.LTerm -> T.LTerm)
collectPattern (lv, Loc _ (S.AtPattern lp l)) = do
  let pos = getLoc lv
  (innerTests, innerWrap) <- collectPattern (lv, lp)
  -- The label test is just another conjunct, evaluated before the inner pattern.
  let levelTest = Loc pos (Bin Eq (Loc pos (Un LevelOf lv)) (Loc pos (Lit (LLabel l))))
  return (levelTest : innerTests, innerWrap)
collectPattern (lv, Loc _ (S.VarPattern var)) =
  let pos = getLoc lv
  in return ([], \s -> Loc pos (Let [T.ValDecl var lv] s))
collectPattern (lv, Loc _ (S.ValPattern lit)) =
  let pos = getLoc lv
  in return ([Loc pos (Bin Eq lv (Loc _srcRT (Lit (transLit lit))))], id)
collectPattern (lv, Loc _ S.Wildcard) =
  let pos = getLoc lv
  in return ([], \s -> Loc pos (Let [T.ValDecl "$wildcard" lv] s))
collectPattern (lv, Loc _ (S.TuplePattern pats)) = do
  let pos = getLoc lv
  -- Accessors for the value to be assigned to the patterns.
  let accessors = map (\idx -> Loc pos (ProjIdx lv idx)) [0..(fromIntegral (length pats) - 1)]
  -- Collect the nested patterns left-to-right so the left-most is evaluated first.
  childResults <- mapM collectPattern (zip accessors pats)
  -- Check first that the value is a tuple of the right length, then the children.
  let shapeTest = Loc pos (Bin And (Loc pos (Un IsTuple lv)) (Loc pos (Bin Eq (Loc pos (Un TupleLength lv)) (Loc _srcRT (Lit (LNumeric (NumInt (toInteger (length pats)))))))))
  return (shapeTest : concatMap fst childResults, foldr (.) id (map snd childResults))
-- TODO Generate more efficient code:
-- Decompose the list v according to the pattern with a DFS pass.
-- This would benefit from an "is empty" operation (to not having to use the RT-dispatched equals).
-- A potentially expensive length calculation is then unnecessary.
-- However, this is more complicated, as would need unique name generation, also for potentially nested list patterns.
collectPattern (lv, Loc _ (S.ListPattern pats)) = do
  let pos = getLoc lv
  -- Accessors for the value to be assigned to the patterns.
  let accessors = map (\lt -> Loc pos (Un Head lt)) $ iterate (\lt -> Loc pos (Un Tail lt)) lv
  -- Collect the nested patterns left-to-right so the left-most is evaluated first.
  childResults <- mapM collectPattern (zip accessors pats) -- pairs of pattern (the nested ones in the list) and term accessing the value at the corresponding index in the list term
  -- Check first that the value is a list of the right length, then the children.
  let shapeTest = Loc pos (Bin And (Loc pos (Un IsList lv)) (Loc pos (Bin Eq (Loc pos (Un ListLength lv)) (Loc _srcRT (Lit (LNumeric (NumInt (toInteger (length pats)))))))))
  return (shapeTest : concatMap fst childResults, foldr (.) id (map snd childResults))
collectPattern (lv, Loc _ (S.ConsPattern lp1 lp2)) = do
  let pos = getLoc lv
  (t1, w1) <- collectPattern (Loc pos (Un Head lv), lp1)
  (t2, w2) <- collectPattern (Loc pos (Un Tail lv), lp2)
  -- TODO Avoid list length (potentially expensive). Implement similarly to the improved list pattern (see above).
  -- Preserve the nested emission's order: shape test, then the tail pattern's
  -- tests, then the head pattern's tests.
  let shapeTest = Loc pos (Bin And (Loc pos (Un IsList lv)) (Loc pos (Bin Gt (Loc pos (Un ListLength lv)) (Loc _srcRT (Lit (LNumeric (NumInt 0)))))))
  return (shapeTest : (t2 ++ t1), w2 . w1)
collectPattern (_, Loc _ (S.ConPattern _ _)) =
  -- Constructor patterns are rewritten to tuple patterns by SynVarFolding
  -- before this pass runs, so none should remain here.
  internalError "unexpected constructor pattern after variant folding"
collectPattern (lv, Loc _ (S.RecordPattern fieldPatterns mode)) = do
  let pos = getLoc lv
  -- Check for duplicate field names
  let fieldNames = map fst fieldPatterns
  let duplicates = fieldNames \\ nub fieldNames
  if not (null duplicates)
    then throwError $ "Duplicate field names in record pattern: " ++ show duplicates
    else do
      fieldResults <- mapM collectField fieldPatterns
      let recordCheck = case mode of
            WildcardMatch ->
              -- Current behavior - just check it's a record and has the specified fields
              Loc pos (Un IsRecord lv)
            ExactMatch ->
              -- Check that the record has exactly the specified number of fields
              let expectedSize = length fieldPatterns
                  sizeCheck = Loc pos (Bin Eq (Loc pos (Un RecordSize lv)) (Loc _srcRT (Lit (LNumeric (NumInt (fromIntegral expectedSize))))))
              in Loc pos (Bin And (Loc pos (Un IsRecord lv)) sizeCheck)
      return (recordCheck : concatMap fst fieldResults, foldr (.) id (map snd fieldResults))
    where collectField (f, mlp) = do
              -- Check the field is present, then the field's pattern.
              let pos = getLoc lv
                  f' = Loc _srcRT (Lit (LString f))
                  hasFieldTest = Loc pos (Bin HasField lv f')
                  fieldLp = case mlp of
                              Just lp -> lp
                              Nothing -> Loc _srcRT (S.VarPattern f)
              (innerTests, innerWrap) <- collectPattern (Loc pos (T.ProjField lv f), fieldLp)
              return (hasFieldTest : innerTests, innerWrap)



-- | Tranform a declaration, compiling patterns into terms.
-- When there are multiple patterns like in functions or a case expression,
-- they are folded into a nested term, with an error expression innermost (after the last check).
-- The error expression is therefore passed as state of a Reader monad.
transDecl :: S.Decl -> LTerm -> Trans LTerm
transDecl (S.ValDecl lpat lt) succ = do
  let temp = "$decltemp$"
      patPos = getLoc lpat
  t' <- transLTerm lt
  result <- runReaderT (compilePattern succ ((Loc patPos (Var temp)), lpat)) (Loc patPos (Error (Loc patPos (Lit (LString "pattern match failure in let declaration")))))
  return $ Loc patPos (Let [ValDecl temp t'] result)
transDecl S.ErrorDecl _ =
  -- As with 'S.ErrorPattern' above: a recovery placeholder that a successful
  -- parse never yields.
  internalError "unexpected error-recovery declaration in a parsed program"
transDecl (S.FunDecs fundecs) succ = do
  fundecs' <- mapM transLFunDecl fundecs
  return (Loc _srcRT (Let [FunDecs fundecs'] succ))
  where
    argLength ((S.Lambda args _):_) = length args
    argLength [] = 0
    -- Extract positions from the patterns in the first lambda (from Located wrappers)
    argPositions lams = case lams of
      (S.Lambda pats _):_ -> map getLoc pats
      [] -> []
    transLFunDecl (Loc pos (S.FunDecl f lams)) = do
      let lams' = map (transLambda_aux . (\(S.Lambda args e) -> S.Lambda [Loc _srcRT (S.TuplePattern args)] e)) lams
          names = map (((f ++ "_pat") ++) . show) [1..(length lams)]
          args =  map (((f ++ "_arg") ++) . show) [1..(argLength lams)]
          -- Create Located variable names with positions from original patterns
          extractedPositions = argPositions lams
          argsWithPos = zipWith (\a p -> Loc p a) args (extractedPositions ++ repeat _srcRT)
          args' =  Loc pos (Tuple (map (\a -> Loc pos (Var a)) args) False)
          errorMsg = Loc pos (Error (Loc pos (Lit (LString $ "pattern match failure in function " ++ f))))
      (fst, decls) <- foldr (\(n, l) acc -> do
            (fail, decls) <- acc
            lam <- runReaderT l fail
            return ( (Loc pos (App (Loc pos (Var n)) [args']))
                   , (ValDecl n (Loc pos (Abs lam))) : decls)
          ) (return (errorMsg, [])) (zip names lams')
      return (FunDecl f (Lambda argsWithPos (Loc pos (Let (reverse decls) fst))) pos)

-- | Transform a Located Term by extracting position and embedding it in Located wrapper
transLTerm :: S.LTerm -> Trans T.LTerm
transLTerm (Loc pos term) = transTerm pos term

-- | Transform a Term given its position (from the Located wrapper)
-- Now produces Located terms
transTerm :: PosInf -> S.Term -> Trans T.LTerm
transTerm pos (S.Lit lit) =
  return $ Loc pos (T.Lit (transLit lit))
transTerm pos (S.Var v) = return $ Loc pos (T.Var v)
transTerm pos (S.Abs l) = do
  l' <- transLambda l
  return $ Loc pos (T.Abs l')
transTerm pos (S.Hnd h) = do
  h' <- transHandler h
  return $ Loc pos (T.Abs h')
transTerm pos (S.App lt1 largs) = do
  t1' <- transLTerm lt1
  args' <- mapM transLTerm largs
  return $ Loc pos (T.App t1' args')
transTerm _ (S.Let decls lt) = do
  t' <- transLTerm lt
  foldr (\decl acc -> do
          acc' <- acc
          transDecl decl acc'
        ) (return t') decls
transTerm pos (S.Case lt cases) = do
  t' <- transLTerm lt
  cases' <- mapM (\(lpat, lsucc) -> do
                    succ' <- transLTerm lsucc
                    return (lpat, succ')
                  ) cases
  let e = foldr (\(lpat, succ') fail ->
            case runExcept (runReaderT (compilePattern succ' (Loc pos (Var "casevar"), lpat)) fail) of
              Right result -> result
              Left err -> error err
          ) (Loc pos (Error (Loc pos (Lit (LString "pattern match failure in case expression"))))) cases'
  return $ Loc pos (Let [ValDecl "casevar" t'] e)
transTerm pos (S.If lt1 lt2 lt3) = do
  t1' <- transLTerm lt1
  t2' <- transLTerm lt2
  t3' <- transLTerm lt3
  return $ Loc pos (If t1' t2' t3')
transTerm pos (S.Tuple ltms tag) = do
  tms' <- mapM transLTerm ltms
  return $ Loc pos (T.Tuple tms' tag)
transTerm pos (S.Record fields) = do
  fields' <- transFields pos fields
  return $ Loc pos (T.Record fields')
transTerm pos (S.WithRecord le fields) = do
  e' <- transLTerm le
  fields' <- transFields pos fields
  return $ Loc pos (T.WithRecord e' fields')
transTerm pos (S.ProjField lt f) = do
  t' <- transLTerm lt
  return $ Loc pos (T.ProjField t' f)
transTerm pos (S.ProjIdx lt idx) = do
  t' <- transLTerm lt
  return $ Loc pos (T.ProjIdx t' idx)
transTerm pos (S.List ltms) = do
  tms' <- mapM transLTerm ltms
  return $ Loc pos (T.List tms')
transTerm pos (S.ListCons lt1 lt2) = do
  t1' <- transLTerm lt1
  t2' <- transLTerm lt2
  return $ Loc pos (T.ListCons t1' t2')
transTerm pos (S.Bin op lt1 lt2) = do
  t1' <- transLTerm lt1
  t2' <- transLTerm lt2
  return $ Loc pos (Bin op t1' t2')
transTerm pos (S.Un op lt) = do
  t' <- transLTerm lt
  return $ Loc pos (Un op t')
transTerm _ (S.Seq lts) =
    case reverse lts of
        [lt] -> transLTerm lt
        lbody:lts_rev -> do
          let decls = map (\lt -> S.ValDecl (Loc _srcRT S.Wildcard) lt) (reverse lts_rev)
          transLTerm (Loc NoPos (S.Let decls lbody))
        []  -> throwError "impossible case: sequence of empty terms"

transTerm _ (S.Error _) = throwError "impossible case: error"

transFields :: PosInf -> S.LFields -> Trans T.LFields
transFields pos = mapM $ \case
  (f, Nothing) -> return (f, Loc pos (T.Var f))
  (f, Just lt) -> do
    t' <- transLTerm lt
    return (f, t')
