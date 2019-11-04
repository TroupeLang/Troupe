{-- First-order interpreter --}

{--
Author: Aslan Askarov , aslan@askarov.net
Creation date: 6 November 2017

--}
{-# LANGUAGE TupleSections #-}
module FOInterp (eval, Runtime (..), EvalError (..), Value (..), LValue (..))

where

import Basics
import Direct
import qualified Data.Map.Lazy as Map
import qualified Monitor as Mon
import qualified LabelModel as Label
import Data.List
import Control.Monad.Except
import Control.Monad.State.Lazy as State

-- import Debug

data EvalError
  = TimeOut
  | TypeError
  | UnboundIdentifier
  | IllegalFlow

class Monad m => Runtime m where
  printline :: String -> m ()
  inputline :: m String

instance Runtime IO where
  printline = putStrLn
  inputline = getLine

type EvalState = (Integer, Integer)  -- fresh var counter, number of evals
type ThrowsError a = ExceptT EvalError a
type S m = StateT EvalState (Mon.MonitorT (ThrowsError m))

data Value
    = PrimVal Lit
    | AtomVal AtomName
    | TupleVal [LValue]
    | ListVal [LValue]
    | Closure Env Lambda
    | PrimFun String

data LValue = LVal Value Label.Label

liftMonitor :: Monad m => Mon.MonitorT (ThrowsError m) a -> (ThrowsError m) a
liftMonitor v = do
  res <- Mon.runMonitor v
  case res of
    Left _ -> throwError IllegalFlow
    Right v' -> return v'

performRuntime :: Runtime m => m a -> S m a
performRuntime = lift . lift . lift

monitor :: Runtime m => Mon.Event -> S m ()
monitor e = lift $ Mon.transition e

basePrint:: Runtime m => LValue -> S m LValue
basePrint (LVal (PrimVal (LInt x)) l) = do
    performRuntime $ printline (show x)
    return $ (LVal (PrimVal LUnit) l)
basePrint (LVal _ l)  = return (LVal (PrimVal LUnit) l)

baseInput :: Runtime m => LValue -> S m LValue
baseInput (LVal (PrimVal (LUnit)) l) = do
  inp <- performRuntime $ inputline
  return (LVal (PrimVal (LInt (read inp))) Label.top)

instance Show EvalError where
  show (TimeOut)           = "evaluation timed out"
  show (TypeError)         = "type error occurred"
  show (UnboundIdentifier) = "unbound identifier"
  show (IllegalFlow)       = "illegal flow"

instance Show LValue where
  show (LVal v l) = show v ++ " {" ++ show l ++ "}"

instance Show Value where
  show (AtomVal s) = s
  show (PrimVal (LInt i)) = show i
  show (PrimVal (LBool b)) = show b
  show (PrimVal LUnit)  = "()"
  show (TupleVal ts) =
    "(" ++  Data.List.intercalate "," (map show ts) ++")"

  show (ListVal ts) =
    "[" ++ Data.List.intercalate "," (map show ts) ++"]"

  show (Closure _ _) = "<<closure>>"
  show (PrimFun f) = f



-- 2018-05-30: TODO: support libraries in the interpreter

eval :: Runtime m => Prog -> m (Either EvalError LValue)
eval (Prog _ (Atoms atoms) t) =
  let initEnv = Env (Map.fromList
                          (map (\a -> (a, (LVal (AtomVal a) Label.bottom))) atoms)
                    )
      baseEnv = ext initEnv  ("print" , (LVal (PrimFun "print") Label.bottom))
      baseEnv' = ext baseEnv ("input" , (LVal (PrimFun "input") Label.bottom))
      initState = (0, 0)
  in runExceptT $ liftMonitor $ State.evalStateT (evTerm t baseEnv') initState

newtype Env = Env (Map.Map VarName LValue)

envLookup :: Env -> VarName -> LValue
envLookup (Env varsMap) k =
  case Map.lookup k varsMap of
    Just v -> v
    _ -> error ("identifier not found " ++ show k)


ext :: Env -> (VarName, LValue) -> Env
ext (Env varsMap) (x,v) =
  Env ( Map.insert x v varsMap )

extMany :: Env -> [(VarName, LValue)] -> Env
extMany env []       = env
extMany env (xx:xs)  = ext (extMany env xs) xx


extPattern :: Env -> (DeclPattern, LValue) -> Env
extPattern env dec_val =
  case runExcept (extPatternErr env dec_val) of
    Left s -> error s
    Right env' -> env'


type EnvErr = Except String Env

extPatternErr :: Env -> (DeclPattern, LValue) -> EnvErr
extPatternErr env (VarPattern x, v) = return $ ext env (x,v)
extPatternErr env (Wildcard, _) = return env
extPatternErr env (ValPattern x, (LVal (PrimVal y) _)) | x == y = return env
extPatternErr env (TuplePattern patterns, (LVal (TupleVal ts) l)) =
  extManyPatternsErr env (zipWith (\ pat (LVal v lv) -> (pat, (LVal v (Label.lub l lv))))
                           patterns ts)


extPatternErr env (ListPattern patterns, (LVal (ListVal ts) l)) | length patterns == length ts =
 extManyPatternsErr env (zip patterns ts)

extPatternErr env (ConsPattern patHd patTl, (LVal (ListVal (x:xs)) l)) =
  extManyPatternsErr env [(patHd, x), (patTl, (LVal (ListVal xs) l))]


extPatternErr _ _ = throwError "pattern mismatch"


extManyPatternsErr :: Env -> [(DeclPattern, LValue)] -> EnvErr
extManyPatternsErr env [] = return env
extManyPatternsErr env (xx:xs) = do
    env' <- extManyPatternsErr env xs
    extPatternErr env' xx

--------------------------------------------------

evalBin :: BinOp -> Lit -> Lit -> Lit
evalBin Plus   ( LInt a)  ( LInt b)  = LInt ( a + b )
evalBin Minus  ( LInt a)  ( LInt b)  = LInt ( a - b )
evalBin Mult   ( LInt a)  ( LInt b)  = LInt ( a * b )
evalBin Div    ( LInt a)  ( LInt b)  = LInt ( a `div` b )
evalBin Le     ( LInt a)  ( LInt b)  = LBool ( a <= b )
evalBin Lt     ( LInt a)  ( LInt b)  = LBool ( a < b )
evalBin Ge     ( LInt a)  ( LInt b)  = LBool ( a >= b )
evalBin Gt     ( LInt a)  ( LInt b)  = LBool ( a > b )
evalBin Eq  a b                      = LBool ( a == b )
evalBin Neq a b                      = LBool ( a /= b )
evalBin And ( LBool a)    ( LBool b) = LBool ( a && b )
evalBin Or  ( LBool a)    ( LBool b) = LBool ( a || b )
evalBin a b c = error $ "invalid binop: " ++ (show a) ++ (show b) ++ (show c)

evLit :: Runtime m => Lit -> S m LValue
evLit (LInt i)  = LVal (PrimVal (LInt i)) <$> lift Mon.programCounter
evLit (LBool b) = LVal (PrimVal (LBool b)) <$> lift Mon.programCounter
evLit LUnit     = LVal (PrimVal LUnit) <$> lift Mon.programCounter

evTerm :: Runtime m => Term -> Env -> S m LValue
evTerm t env = do

  -- we incremenent the step counter, can be useful for debugging
  modify ( \(fresh, nsteps) -> (fresh, nsteps + 1) )

  -- note that our eval lives in IO; if needed we can put something on
  -- console, for debugging: uncommenting the line below should work

  -- lift (putStrLn (debugName t))

  let evT term = evTerm term env
  pc <- lift $ Mon.programCounter

  case t of
    Lit lit -> evLit lit
    Var x   -> return $ envLookup env x

    Tuple ts -> do
        ts' <- mapM evT ts
        return $ LVal (TupleVal ts') pc


    List ts -> do
         ts' <- mapM evT ts
         return $ LVal (ListVal ts') pc


    ListCons e0 e1 -> do
      v1 <- evT e0

      v2 <- evT e1
      case v2 of
         LVal (ListVal vv) l -> return $ LVal (ListVal (v1:vv)) $ Label.lub pc l
         _ -> error "RHS of cons operation does not evaluate to a list"


    If e0 e1 e2 ->
      evT e0 >>= \(LVal v0 l) -> do
      monitor (Mon.Branch l)
      res <- case v0 of
               PrimVal (LBool True)  -> evT e1
               PrimVal (LBool False) -> evT e2
               _ -> error "if guard is not a boolean"
      monitor Mon.Join
      return res
    Case e pat_terms -> do
      v <- evT e
      let step [] = error "no matching case expression"
          step ((decl, term):pat_terms') =
            case runExcept (extPatternErr env (decl, v)) of
              Left _     -> step pat_terms'
              Right env' -> evTerm term env'
      step pat_terms

    Abs lam -> return $ evLambda lam env pc

    Bin op e1 e2 -> do
      v1 <- evT e1
      v2 <- evT e2
      return $
        case (v1, v2) of
          (LVal (PrimVal a) l1, LVal (PrimVal b) l2) -> LVal (PrimVal (evalBin op a b)) $ Label.lub l1 l2
          _ -> error "binop match"

    Let decls body -> do
       env' <- evDecls decls env
       evTerm body env'

    App e args -> do

      let applyOne :: Runtime m => LValue -> Term -> S m LValue
          applyOne (LVal (Closure _ (Lambda [] _)) _) _ = error "no-argument closure in function application"
          applyOne (LVal (Closure envClos (Lambda (formal:formals) body)) funlab) e' = do
            lv@(LVal v labv) <- evT e'
            let env' = extPattern envClos (formal, lv)
            case formals of
              [] -> do
                monitor (Mon.Branch funlab)
                res <- evTerm body env'
                monitor (Mon.Join)
                return res
              _  -> return $ LVal (Closure env' (Lambda formals body)) funlab
          applyOne (LVal (PrimFun f) lfun) e' = do
            v <- evT e'
            monitor (Mon.Branch lfun)
            let res = (evalPrimFun f v)
            monitor (Mon.Join)
            res
          applyOne _ _ = error "LHS of application does not evaluate to a closure or a base function"

      let applyMany :: Runtime m => LValue -> [Term] -> S m LValue
          applyMany v (arg:restArgs) = do
            u <- applyOne v arg
            case restArgs of
              [] -> return u
              _  -> applyMany u restArgs

          applyMany _ [] = error "empty argument list in function application"


      v <- evT e
      applyMany v args

evalPrimFun :: Runtime m => String -> LValue -> S m LValue
evalPrimFun ("print") = basePrint
evalPrimFun ("input") = baseInput


freshSymbol :: Runtime m => S m VarName
freshSymbol  = do
  (k, c) <- get
  put (k + 1, c)
  return $ "$" ++ show k



funPatternsToCases :: String -> [Lambda] -> Lambda


funPatternsToCases _ [x] = x
funPatternsToCases prefix ll =
  let (Lambda argPats _) = head ll
      nArgs = length argPats
      freshVars = map (\i -> prefix ++ "." ++ show i ) [1..nArgs]
      newArgs = map VarPattern freshVars
      tupleTerms = map Var freshVars
      newTerm =
            Case (Tuple tupleTerms)
                ( map (\(Lambda pats term) -> (TuplePattern pats, term)) ll)
  in Lambda newArgs newTerm


evDecl :: Runtime m => Env -> Decl -> S m Env
evDecl env decl = do
  pc <- lift $ Mon.programCounter
  case decl of
    ValDecl pat t -> do
      v <- evTerm t env
      return $ extPattern env (pat, v)
    FunDecs fdecs -> do
      temp <- freshSymbol
      -- 2017-11-30: check how much changes need to be done here.
      let env' = let aux (FunDecl f ll) = (f, evLambda (funPatternsToCases temp ll) env' pc)
                 in extMany env $ map aux fdecs
      return env'


evDecls :: Runtime m => [Decl] -> Env -> S m Env
evDecls decls env =
  foldM evDecl env decls

evLambda :: Lambda -> Env -> Label.Label -> LValue
evLambda lam env pc = LVal (Closure env lam) pc
