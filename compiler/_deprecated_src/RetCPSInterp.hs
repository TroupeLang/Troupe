{-- 2018-02-15: note that this interpreter leaks memory; AA --}

module RetCPSInterp (cpseval, cpsEvalStep)

where

import qualified Basics
import Basics(BinOp(..),UnaryOp(..))
import Control.Monad.Except
import qualified Core as C
import RetCPS
import qualified Data.Map as Map
import qualified Data.List
import qualified Control.Monad.State.Lazy as State

type EvalState = Integer

data ContValue = ContClosure Env VarName ContValue KTerm
               | HaltClosure

data EvalError
  = TimeOut
  | TypeError
  | UnboundIdentifier String
  | IllegalFlow

class Monad m => Runtime m where
  printline :: String -> m ()
  inputline :: m String

instance Runtime IO where
  printline = putStrLn
  inputline = getLine

type ThrowsError = Except EvalError
type S = State.StateT EvalState ThrowsError

data Value
    = PrimVal C.Lit
    | AtomVal C.AtomName
    | TupleVal [Value]
    | ListVal [Value]
    | Closure Env KLambda
    | PrimFun String

instance Show Value where
  show (AtomVal s) = s
  show (PrimVal (C.LInt i)) = show i
  show (PrimVal (C.LBool b)) = show b
  show (PrimVal C.LUnit)  = "()"
  show (TupleVal ts) =
    "(" ++  Data.List.intercalate "," (map show ts) ++")"

  show (ListVal ts) =
    "[" ++ Data.List.intercalate "," (map show ts) ++"]"

  show (Closure _ _) = "<<closure>>"
  show (PrimFun f) = f

instance Show EvalError where
  show TimeOut = "evaluation timed out"
  show TypeError = "type error"
  show (UnboundIdentifier msg) = "unbound identifier: " ++ msg
  show IllegalFlow = "illegal flow"


{-- notice that our environment contains two separate mappings,
    for now it keeps things cleaner and closer to what
    happens in Kennedy's paper; 2017-10-13 --}

-- 2018-02-15: we are adding the explicit return mapping

newtype Env = Env ((Map.Map VarName Value), ContValue)

find :: (Ord a, Show a) => a -> Map.Map a b -> S b
find x map =
  case Map.lookup x map of
    Nothing -> throwError $ UnboundIdentifier (show x)
    Just v -> return v

eval :: Env -> KTerm -> S Value
eval env t = do
  State.modify (+1)
  eval' env t


eval' :: Env -> KTerm -> S Value
-- eval' (Env (vars, _)) (Halt x) = find x vars

eval' env (LetSimple x sterm kterm) = do
  xval <- simpleEval env sterm
  let env' = ext_vars env (x, xval)
  eval env' kterm

eval' env (LetRet (Cont vname kt1) kt2) =
  let env' = mk_ret env (env, vname, kt1)
  in eval env' kt2


eval' (Env (vars, kret)) (KontReturn vname) = do
  let (ContClosure env var kret' body) = kret
  arg <- find vname vars
  let env' = ext_ret (ext_vars env (var, arg)) kret'
  eval env' body

-- eval' env@(Env (_,_)) (LetRet kname body) = do
--   kterm <- find kname konts
--   let env' = ext_ret env (Just kterm)
--   eval env' body


eval' (Env (vars, ret)) (ApplyFun fname arg) = do
  (Closure env (Unary var body)) <- find fname vars
  argval <- find arg vars
  let env' = ext_vars env (var, argval)
  let env'' = ext_ret env' ret
  eval env'' body

eval' env@(Env (vars,_)) (If x k1 k2) = do
  val <- find x vars
  case val of
    (PrimVal (C.LBool True)) -> eval env k1
    (PrimVal (C.LBool False)) -> eval env k2
    _ -> error "guards must be boolean"


eval' env (LetFun fundefs kbody) =
  let env' = foldl (\env (Fun fname klam) ->
                       ext_vars env (fname, evKLambda env' klam))
             env
             fundefs
  in eval env' kbody


-- eval _ _ = error "not implemented"

evKLambda :: Env -> KLambda -> Value
evKLambda env lam =
    Closure env lam

ext_vars :: Env -> (VarName, Value) -> Env
ext_vars (Env (vars, ret)) (x, v) =
  Env (Map.insert x v vars, ret)

-- ext_konts :: Env -> (Env, VarName, KTerm) -> Env
-- ext_konts (Env (vars, ret))  =
--   Env (vars, Map.insert k u ret)

mk_ret :: Env -> (Env, VarName, KTerm) -> Env
mk_ret (Env (vars, ret)) (env, x, t) =
  Env (vars, ContClosure env x ret t)

ext_ret :: Env -> ContValue -> Env
ext_ret (Env (vars, _)) k =
  Env (vars, k)

-- ext_vars_konts ::
--   Env -> (VarName, Value) -> (KontName, ContValue) -> Env
-- ext_vars_konts env xv kk =
--   ext_konts (ext_vars env xv) kk

valEval :: Env -> SVal -> Value
valEval _ (Lit (C.LInt n)) = PrimVal (C.LInt n)
valEval _ (Lit C.LUnit) = PrimVal C.LUnit
valEval _ (Lit (C.LBool b)) = PrimVal (C.LBool b)
valEval env (KAbs klam) = evKLambda env klam


evalBin :: BinOp -> C.Lit -> C.Lit -> C.Lit
evalBin Plus   ( C.LInt a)  ( C.LInt b)  = C.LInt ( a + b )
evalBin Minus  ( C.LInt a)  ( C.LInt b)  = C.LInt ( a - b )
evalBin Mult   ( C.LInt a)  ( C.LInt b)  = C.LInt ( a * b )
evalBin Div    ( C.LInt a)  ( C.LInt b)  = C.LInt ( a `div` b )
evalBin Le     ( C.LInt a)  ( C.LInt b)  = C.LBool ( a <= b )
evalBin Lt     ( C.LInt a)  ( C.LInt b)  = C.LBool ( a < b )
evalBin Ge     ( C.LInt a)  ( C.LInt b)  = C.LBool ( a >= b )
evalBin Gt     ( C.LInt a)  ( C.LInt b)  = C.LBool ( a > b )
evalBin Eq  a b                          = C.LBool ( a == b )
evalBin Neq a b                          = C.LBool ( a /= b )
evalBin And    ( C.LBool a) ( C.LBool b) = C.LBool ( a && b )
evalBin Or     ( C.LBool a) ( C.LBool b) = C.LBool ( a || b )
evalBin op v1 v2 = error ("type mismatch: " ++ show op ++ " " ++ (show v1) ++ " " ++ (show v2))

evalUn :: UnaryOp -> Value -> S Value
evalUn IsList (ListVal _) = return $ PrimVal (C.LBool True)
evalUn IsTuple (TupleVal _) = return $ PrimVal (C.LBool True)
evalUn Head (ListVal (fst:_)) = return fst
evalUn Tail (ListVal (_:rst)) = return $ ListVal rst
evalUn Fst (TupleVal (fst:_)) = return fst
evalUn Snd (TupleVal (_:rst:_)) = return rst
evalUn Length (ListVal lst) = return $ PrimVal $ C.LInt $ toInteger $ length lst
evalUn Length (TupleVal lst) = return $ PrimVal $ C.LInt $ toInteger $ length lst
evalUn _ _ = throwError TypeError

simpleEval :: Env -> SimpleTerm -> S Value
simpleEval env (ValSimpleTerm v) = return $ valEval env v
simpleEval (Env (vars, _)) (Tuple ls) = TupleVal <$> mapM (flip find vars) ls
simpleEval (Env (vars, _)) (List ls) = ListVal <$> mapM (flip find vars) ls
simpleEval (Env (vars, _)) (ListCons h t) = do
  h' <- find h vars
  (ListVal t') <- find t vars
  return $ ListVal (h' : t')
simpleEval (Env (vars, _)) (Bin Index x y) = do
  x' <- find x vars
  y' <- find y vars
  case (x', y') of
    (ListVal lst, PrimVal (C.LInt n)) -> return $ lst !! (fromInteger n)
    (TupleVal lst, PrimVal (C.LInt n)) -> return $ lst !! (fromInteger n)
    (v1, v2) -> throwError TypeError
simpleEval (Env (vars, _)) (Bin op x y) = do
  x' <- find x vars
  y' <- find y vars
  case (x', y') of
    (PrimVal a, PrimVal b) -> return $ PrimVal (evalBin op a b)
    (v1, v2) -> throwError TypeError
simpleEval (Env (vars, _)) (Un op x) = do
  x' <- find x vars
  evalUn op x'

initEnv :: Env
initEnv =
  let empty = Map.empty
  in Env (empty, HaltClosure)

initState :: EvalState
initState = 0

cpseval :: Runtime m => Prog -> m (Either EvalError Value)
cpseval (Prog (C.Atoms atoms) kt) = do
  let initEnv = Env (Map.fromList
                          (map (\a -> (VN a, (AtomVal a))) atoms)
                    ,HaltClosure
                    )
      errVal = State.evalStateT (eval initEnv kt) initState
  return $ runExcept errVal

cpsEvalStep :: KTerm -> (Either EvalError (Value, EvalState))
cpsEvalStep t =
  runExcept $ State.runStateT (eval initEnv t) initState
