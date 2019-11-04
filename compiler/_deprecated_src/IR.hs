{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
module IR where

import qualified Basics
import           RetCPS                    (VarName (..))


import qualified Core                      as C
import qualified RetClosureConv            as RCC
import qualified RetCPS                    as CPS

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
-- import Data.List.Unique -- cabal install Unique
import qualified Data.ByteString           as BS
import           Data.Serialize            (Serialize)
import qualified Data.Serialize            as Serialize
import           GHC.Generics              (Generic)

import           CompileMode
import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP

data VarAccess
  = VarLocal VarName
  | VarEnv EnvName VarName

  deriving (Eq, Show, Generic)

{--
data KontAccess
  = KontLocal RCC.KontName
  | KontEnv EnvName RCC.KontName
  deriving (Eq, Show, Generic)
--}
type Ident = String

newtype HKN = HKN Ident deriving (Eq, Show, Ord, Generic)
newtype HFN  = HFN Ident deriving (Eq, Show, Ord, Generic)
newtype EnvName = EnvName Ident deriving (Eq, Show, Generic)


data IRExpr
  = Bin Basics.BinOp VarAccess VarAccess
  | Un Basics.UnaryOp VarAccess
  | Tuple [VarAccess]
  | List [VarAccess]
  | ListCons VarAccess VarAccess
  | Const C.Lit
  | Base Basics.VarName
  | Lib Basics.LibName Basics.VarName
  deriving (Eq, Show, Generic)



-- 2018-02-18: It may be nice to enforce some structure on the IR by
-- construction so that we know for sure that this structure is
-- preserved by our compilation; moreover we will disregard any
-- untrusted input that does not adhere to this structure; most
-- likely, this structure is going to be needed for the soundness of
-- the monitor, whenever we get there; AA


-- The main thing that we achieve through this structure is that
-- it disallows any subsequent infrastructures following an IF
-- statement.

-- Note also that our definition of a basic block is a little
-- unconventional as the IF terminator may include nested basic
-- blocks, but for our purposes, this is not a problem; and we avoid
-- having to introduce labels, labeled blocks, and labeled jumps.
-- Perhaps, we can pick a better name that Basic Block for this in the
-- future; 2018-02-18; AA

data IRBBTree = BB [IRInst] IRTerminator deriving (Eq, Generic)


data IRTerminator
  = TailCall VarAccess VarAccess
  | Ret VarAccess
  | If VarAccess IRBBTree IRBBTree
  | AssertOrError VarAccess IRBBTree VarAccess
  | LibExport VarAccess
  | Error VarAccess
  deriving (Eq,Generic)



-- the reason IF is included as a terminator is that in the body of an
-- IR function, there should be no more instructions following IF,
-- and that there should be a proper place to jump at the end of
-- each IF


data IRInst
  = AssignVar VarName IRExpr
  | MkFunClosure VarName EnvName HFN
  | SetRet HKN EnvName
  | MkEnv EnvName [(VarName, VarAccess)]
 deriving (Eq, Generic)


-- functions and kontinuations include their first basic blocks; other
-- basic blocks may be nested within the IF terminator -- see comment
-- above on that; 2018-02-18; AA

data KontFunDef = KontFunDef HKN EnvName VarName IRBBTree deriving (Eq,Generic)
data FunDef     = FunDef HFN EnvName VarName IRBBTree deriving (Eq,Generic)


data IRProgram = IRProgram C.Atoms [KontFunDef] [FunDef]
  deriving (Generic)

--------------------------------------------------
-- Dependency calculation
--------------------------------------------------
class ComputesDependencies a where
 dependencies :: a -> Writer [DependencyName]  ()

data DependencyName
    = DependencyFun HFN
    | DependencyKont HKN

instance ComputesDependencies IRInst where
  dependencies (MkFunClosure _ _ hfn) = tell [DependencyFun hfn]
  dependencies (SetRet hkn _)         = tell [DependencyKont hkn]
  dependencies _                      = return ()

instance ComputesDependencies IRBBTree where
  dependencies (BB insts trm) =
        do mapM_ dependencies insts
           dependencies trm

instance ComputesDependencies IRTerminator where
  dependencies (If _ bb1 bb2) = mapM_ dependencies [bb1, bb2]
  dependencies (AssertOrError _ bb1 _) = dependencies bb1
  dependencies _              = return ()

instance ComputesDependencies KontFunDef where
  dependencies (KontFunDef _ _ _ bb) = dependencies bb

instance ComputesDependencies FunDef where
  dependencies (FunDef _ _ _ bb) = dependencies bb

deps :: ComputesDependencies a => a -> ([HKN], [HFN])
deps a = let (_, dd) = runWriter (dependencies a)
             (ffs, kks) = partition isFun dd
             kks' = map (\(DependencyKont hkn) -> hkn) kks
             ffs' = map (\(DependencyFun hfn) -> hfn) ffs
         in (sortUniq kks', sortUniq ffs')
           where isFun (DependencyFun _)  = True
                 isFun (DependencyKont _) = False

ppDeps :: ComputesDependencies a => a -> PP.Doc
ppDeps a = let (kks, ffs) = deps a
               k' = map (PP.quotes . ppId) kks
               f' = map (PP.quotes . ppId) ffs
           in (PP.brackets.PP.hsep) (PP.punctuate (text ",") (k' ++ f'))


sortUniq :: Ord a => [a] -> [a]
sortUniq = nub.sort

--------------------------------------------------
-- Default Serialize instance declarations; aa; 2018-03-03
-- We need `deriving(Generic)` for these
--------------------------------------------------

instance Serialize IRProgram
instance Serialize IRTerminator
instance Serialize KontFunDef
instance Serialize FunDef
instance Serialize VarAccess
-- instance Serialize KontAccess
instance Serialize HKN
instance Serialize HFN
instance Serialize EnvName
instance Serialize IRExpr
instance Serialize IRInst
instance Serialize IRBBTree

--------------------------------------------------
-- Serialization
--------------------------------------------------

data SerializationUnit
  = KontSerialization KontFunDef
  | FunSerialization FunDef
  | AtomsSerialization C.Atoms
  deriving (Generic)

instance Serialize SerializationUnit




--
serializeFunDef :: FunDef -> BS.ByteString
serializeFunDef fdef = Serialize.runPut ( Serialize.put (FunSerialization fdef) )

serializeKontDef :: KontFunDef -> BS.ByteString
serializeKontDef kdef = Serialize.runPut ( Serialize.put (KontSerialization kdef) )

serializeAtoms :: C.Atoms -> BS.ByteString
serializeAtoms atoms = Serialize.runPut (Serialize.put (AtomsSerialization atoms))

deserializeAtoms :: BS.ByteString -> Either String C.Atoms
deserializeAtoms bs = Serialize.runGet (Serialize.get) bs

deserialize :: BS.ByteString -> Either String SerializationUnit
deserialize bs =
  case Serialize.runGet (Serialize.get) bs of
    Left s -> Left s
    Right x@(KontSerialization kdecl) ->
      if wfKont kdecl then (Right x)
      else Left "ir not well-formed"
    Right x@(FunSerialization fdecl) ->
      if wfFun fdecl then (Right x)
      else Left "ir not well-formed"
    Right x -> Right x

-- TODO; aa; 2018-03-03: the above is boilerplate; work out the details.




-------------------------------------------------------------
-- Conversion from the Closure Converted CPS form to the IR
-------------------------------------------------------------
transVarAccessToIR :: RCC.VarAccess -> VarAccess
transVarAccessToIR (RCC.VarLocal vn) = VarLocal vn
transVarAccessToIR (RCC.VarEnv vn)   = VarEnv (EnvName "env") vn





toIRExpr :: RCC.SimpleTTerm -> IRExpr
toIRExpr (RCC.Bin op va1 va2) = Bin op (transVarAccessToIR va1) (transVarAccessToIR va2)
toIRExpr (RCC.Un op va) = Un op (transVarAccessToIR va)
toIRExpr (RCC.Tuple vas) = Tuple (map transVarAccessToIR vas)
toIRExpr (RCC.List vas) = List (map transVarAccessToIR vas)
toIRExpr (RCC.ListCons va1 va2) = ListCons (transVarAccessToIR va1) (transVarAccessToIR va2)
toIRExpr (RCC.Const n) = Const n
toIRExpr (RCC.BaseVar v) = Base v
toIRExpr (RCC.LibVar l v) = Lib l v




--------------------------------------------------
-- We use Writer monad for emitting functions
--------------------------------------------------


consBB:: IRInst -> IRBBTree -> IRBBTree
consBB i (BB insts t) = BB (i:insts) t

data HoistedDecl
  = HoistedKontDecl KontFunDef
  | HoistedFunDecl FunDef
  deriving (Generic)
instance Serialize HoistedDecl



toIR :: RCC.TTerm -> RWS CompileMode [HoistedDecl] () IRBBTree
toIR (RCC.LetVarSimple vname st tt) = do
  bb <- toIR tt
  return $ (AssignVar vname (toIRExpr st)) `consBB` bb



toIR (RCC.LetFunClosure closname (VN env) (VN fun) tt) = do
  bb <- toIR tt
  return $ (MkFunClosure closname (EnvName env) (HFN fun)) `consBB` bb


toIR (RCC.LetEnvVar (VN env) vars tt) = do
  bb <- toIR tt
  return $ (MkEnv (EnvName env) (map (\(a,b) -> (a, transVarAccessToIR b)) vars) ) `consBB` bb


toIR (RCC.LetRet (RCC.Cont kname@(RCC.K s) varg tbody) (VN env) tt) = do
  bb <- toIR tbody
  tell [HoistedKontDecl (KontFunDef (HKN ( s)) (EnvName "env") varg bb)]
  tt' <- toIR tt
  return $ (SetRet (HKN (s)) (EnvName env)) `consBB` tt'



toIR (RCC.LetFun fdefs tt) = do
  let ppFun (RCC.Fun (VN fname) varg fbody) = do
        bb <- toIR fbody
        tell $ [HoistedFunDecl (FunDef (HFN fname) (EnvName "env") varg bb)]
  mapM_ ppFun fdefs
  toIR tt



toIR (RCC.KontReturn vaccess) =
  return $ BB [] $ Ret (transVarAccessToIR vaccess)


toIR (RCC.ApplyFun fname vargaccess) =
  return $ BB [] $ TailCall (transVarAccessToIR fname) (transVarAccessToIR vargaccess)



toIR (RCC.If vaccess tt1 tt2) = do
  bb1 <- toIR tt1
  bb2 <- toIR tt2

  return $ BB [] $ If (transVarAccessToIR vaccess) bb1 bb2


toIR (RCC.AssertElseError vaccess tt1 zaccess) = do
  bb <- toIR tt1
  return $ BB [] $ AssertOrError (transVarAccessToIR vaccess) bb (transVarAccessToIR zaccess)


-- We translate Halts to the top level return; 2018-02-19; AA
-- If we are compiling a library then we compile it to the special `export` keyword; 2018-07-02; AA


toIR (RCC.Halt vaccess) = do
  compileMode <- ask
  let constructor =
        case compileMode of
          Normal -> Ret
          Export -> LibExport
  return $ BB [] $ constructor (transVarAccessToIR vaccess)


toIR (RCC.Error vaccess) =
  return $ BB [] $ Error (transVarAccessToIR vaccess)

-- prependOptIns bb Nothing = bb
-- prependOptIns (BB insts term) (Just i) = BB (i:insts) term


toProgIR compileMode (RCC.Prog atoms@(C.Atoms a) tt ) =
  let ( bb, _, hoisted) = runRWS (toIR tt) compileMode ()
      (argumentName, toplevel) =
        case compileMode of
          Normal -> ("$$authorityarg", "main") -- passing authority through the argument to main 
          Export -> ("$$dummy", "export")

      -- obs that our 'main' may have two names depending on the compilation mode; 2018-07-02; AA
      main = FunDef (HFN toplevel) (EnvName "$$env") (VN argumentName) bb
      (konts, funs) = separateHoisted hoisted ([],[])
      irProg = IRProgram atoms konts (funs++[main])
  in if wfIRProg irProg then irProg
     else error "the generated IR is not well-formed"
          -- 2018-02-18: in deserialization pipeline
          -- well-formednesss errors should be handled properly

  where separateHoisted [] (ks, fs) = (reverse ks, reverse fs)
        separateHoisted ((HoistedKontDecl k):rest) (ks, fs) = separateHoisted rest (k:ks, fs)
        separateHoisted ((HoistedFunDecl f):rest) (ks, fs) = separateHoisted rest (ks, f:fs)


--------------------------------------------------
-- Well-formedness
--------------------------------------------------

-- 2018-02-18: we should check ourselves that we are generating a
-- well-formed IR, but this is probably going to be useful for
-- serialization/monitoring.

-- The checks so far are reduced to simple assertion that
-- no identifier is assigned twice;

-- Things to add in the future:
-- [ ] Check that no function/kont is declared twice
--

class WellFormedIRCheck a where
  wfir :: a -> WFCheck ()

type WFCheck a = ExceptT String (State [Ident] ) a

checkId :: Ident -> WFCheck ()
checkId x = do
  ids <- lift get
  if x `elem` ids then throwError x
  else do
    (lift . put) (x:ids)
    return ()

instance WellFormedIRCheck IRInst where
 wfir (AssignVar (VN x) e) = do checkId x
                                wfir e
-- wfir (MkKontClosure (RCC.K x) _ _) = checkId x
 wfir (MkFunClosure (VN x) _ _) = checkId x
 wfir (MkEnv (EnvName x) _) = checkId x
 wfir (SetRet _ (EnvName x)) = return ()

instance WellFormedIRCheck IRTerminator where
  wfir (If _ bb1 bb2) = do
    wfir bb1
    wfir bb2
  wfir (AssertOrError _ bb _) = wfir bb
  wfir _ = return ()


-- 2019-03-22: checking that the closures environments
-- are created and used in a very restrictive manner 
checkMkEnv :: [IRInst] -> WFCheck () 
checkMkEnv insts = checkMkEnv' Nothing (reverse insts)

checkMkEnv' :: Maybe Basics.VarName -> [IRInst] -> WFCheck ()
checkMkEnv' Nothing [] = return () 
checkMkEnv' Nothing ((MkFunClosure _ (EnvName y)  _):insts) = 
  checkMkEnv' (Just y) insts 
checkMkEnv' Nothing ((SetRet _ (EnvName x)): insts) = 
  checkMkEnv' (Just x) insts
checkMkEnv' (Just x) ((MkEnv (EnvName y) _):insts) = 
  if x == y then 
    checkMkEnv' Nothing insts 
  else 
    throwError x
checkMkEnv' (Just x) ((MkFunClosure _ (EnvName y) _):insts) = 
  if x == y then 
    checkMkEnv' (Just x) insts
  else 
    throwError x
checkMkEnv' (Just x) _ = throwError x
checkMkEnv' Nothing (_:insts) = checkMkEnv' Nothing insts

instance WellFormedIRCheck IRBBTree where
  wfir (BB insts tr) = do
    mapM_ wfir insts
    checkMkEnv insts
    wfir tr

instance WellFormedIRCheck IRExpr where
  wfir (Base fname) =
    -- OBS: AA: 2018-07-24: This is the only
    -- place where we check the base functions
    -- (but this should be sufficient though). Note
    -- that it is important to do this check at the level
    -- of the IR because we otherwise may get a malformed
    -- code over wire. Such malformed code would result
    -- in a JS output returing a runtime error (which should
    -- generally be avoided)
     if  fname `elem` [ "print"
                     , "printWithLabels"
                     , "printString"
                     , "writeString"
                     , "receive"
                     , "spawn"
                     , "send"
                     , "node"
                     , "self"
                     , "save"
                     , "restore"
                     , "rcv"
                     , "mkSecret"
                     , "adv"
                     , "register"
                     , "whereis"
                     , "declassify"
                     , "pinipush"
                     , "pinipop"
                     , "attenuate"
                     , "raiseTrust"
                     , "$$authorityarg"
                     , "monitorlocal"
                     , "mkuuid"
                     , "sleep"
                     , "sandbox"
                     , "inputLine"
                     , "question"
                     , "toStringL"
                     , "toString"
                     , "intToString"
                     , "stringToInt"
                     , "exit"
                     , "debugpc"
                     ]
        then return ()
        else fail $ "bad base function: " ++ fname
  wfir _ = return ()



-- todo; 2018-02-18; not checking atoms at the moment
-- they may need to be checked too...
wfIRProg :: IRProgram -> Bool
wfIRProg (IRProgram _ konts funs) =
  and $ (map wfKont konts) ++  (map wfFun funs)

wfKont (KontFunDef (HKN kn) (EnvName env) (VN arg) bb) =
          checkFromBB [kn, env, arg] bb
wfFun (FunDef (HFN fn) (EnvName env) (VN arg) bb) =
          checkFromBB [fn, env, arg] bb

checkFromBB initState bb =
          case evalState (runExceptT (wfir bb)) initState of
            Right _ -> True
            Left s  ->  False   -- todo: better exception handling here in the future;
                               -- 2018-02-18; aa


--------------------------------------------------
-- PRETTY PRINTING
--------------------------------------------------

ppProg (IRProgram atoms konts funs) =
  vcat $ (map ppKontFunDef konts)++(map ppFunDef funs)

instance Show IRProgram where
  show = PP.render.ppProg


ppKontFunDef (KontFunDef hkn env arg insts)
  = vcat [ text "kont" <+> ppFunCall (ppId hkn) [ppId env, ppId arg] <+> text "{"
         , nest 2 (ppBB insts)
         , text "}"]

ppFunDef (FunDef hfn env arg insts)
  = vcat [ text "func" <+> ppFunCall (ppId hfn) [ppId env, ppId arg] <+> text "{"
         , nest 2 (ppBB insts)
         , text "}"]




ppKontName :: RCC.KontName -> PP.Doc
ppKontName (RCC.K kn) = text "K" PP.<> text kn

ppVarName :: VarName -> PP.Doc
ppVarName (VN vn) = text vn


ppVarAccess :: VarAccess -> PP.Doc
ppVarAccess (VarLocal vn) = ppVarName vn
ppVarAccess (VarEnv (EnvName env) vn) = text env PP.<> text "." PP.<> (ppVarName vn)

{--
ppKontAccess :: KontAccess -> PP.Doc
ppKontAccess (KontLocal kn) = ppKontName  kn
ppKontAccess (KontEnv (EnvName env) kn) = text env PP.<> text "." PP.<>  (ppKontName kn)
--}


ppIRExpr :: IRExpr -> PP.Doc
ppIRExpr (Bin Basics.Index va1 va2) =
  ppVarAccess va1 <> text "[" <> ppVarAccess va2 <> text "]"
ppIRExpr (Bin binop va1 va2) =
  ppVarAccess va1 <+> text (textOfBinOp binop) <+> ppVarAccess va2
ppIRExpr (Un op v) =
  text (textOfUnOp op) <> PP.parens (ppVarAccess v)
ppIRExpr (Tuple vars) =
  PP.parens $ PP.hsep $ PP.punctuate (text ",") (map ppVarAccess vars)
ppIRExpr (List vars) =
  PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppVarAccess vars)
ppIRExpr (ListCons v1 v2) =
  text "cons" <>  ( PP.parens $ ppVarAccess v1 <> text "," <> ppVarAccess v2)
ppIRExpr (Const (C.LUnit)) = text "__unit"
ppIRExpr (Const lit) = CPS.ppLit lit
ppIRExpr (Base v) = if v == "$$authorityarg" -- special casing; hack; 2018-10-18: AA
                      then text v 
                      else text v <> text "$base"
ppIRExpr (Lib (Basics.LibName l) v) = text l <> text "." <> text v


ppIR :: IRInst -> PP.Doc
ppIR (AssignVar vn st) = ppId vn <+> text "=" <+> ppIRExpr st

ppIR (MkFunClosure vn env hfn)
  = hsep [ ppId vn
         , text "="
         , ppFunCall (text "mkFClos") [ppId env, ppId hfn]]
ppIR (SetRet ka env) = ppFunCall (text "setret") [ppId ka, ppId env]


ppIR (MkEnv env vars) =
  let vs = hsepc $ ppEnvIds vars
  in
    hsep [ ppId env
         , text "="
         , ppFunCall (text "mkEnv") [  vs ]]

  where ppEnvIds ls =
          map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
        hsepc ls = PP.hsep (PP.punctuate (text ",") ls)



ppTr (AssertOrError va ir va2)
  = text "assert" <+> PP.parens (ppId va) <+>
    text "{" $$
    nest 2 (ppBB ir) $$
    text "}" $$
    text "elseError" <+> (ppId va2)


ppTr (If va ir1 ir2)
  = text "if" <+> PP.parens (ppId va) <+>
    text "{" $$
    nest 2 (ppBB ir1) $$
    text "}" $$
    text "else {" $$
    nest 2 (ppBB ir2) $$
    text "}"
ppTr (TailCall va1 va2) = ppFunCall (text "tail") [ppId va1, ppId va2]
ppTr (Ret va)  = ppFunCall (text "ret") [ppId va]
ppTr (LibExport va) = ppFunCall (text "export") [ppId va]
ppTr (Error va)  = (text "error") <> (ppId va)
-- ppTr (Halt va) = ppFunCall (text "halt") [ppId va]


ppBB (BB insts tr) = vcat $ (map ppIR insts) ++ [ppTr tr]




class Identifier a where
  ppId :: a ->  PP.Doc

instance Identifier VarName where
  ppId = ppVarName

instance Identifier RCC.KontName where
  ppId = ppKontName

instance Identifier VarAccess where
  ppId = ppVarAccess

{--
instance Identifier KontAccess where
  ppId = ppKontAccess
--}

instance Identifier EnvName where
  ppId (EnvName n) = text n

instance Identifier HKN where
  ppId (HKN n) = text n

instance Identifier HFN where
  ppId (HFN n) = text n

ppArgs args = PP.parens( PP.hcat (PP.punctuate (text ",") (args)))

ppFunCall fn args = fn <+> ppArgs args


textOfBinOp Basics.Plus     = "rt.plus"
textOfBinOp Basics.Minus    = "rt.minus"
textOfBinOp Basics.Mult     = "rt.mult"
textOfBinOp Basics.Div      = "rt.div"
textOfBinOp Basics.Eq       = "rt.eq"
textOfBinOp Basics.Neq      = "rt.neq"
textOfBinOp Basics.Le       = "rt.le"
textOfBinOp Basics.Lt       = "rt.lt"
textOfBinOp Basics.Ge       = "rt.ge"
textOfBinOp Basics.Gt       = "rt.gt"
textOfBinOp Basics.And      = "rt.and"
textOfBinOp Basics.Or       = "rt.or"
textOfBinOp Basics.Index    = "rt.index"
textOfBinOp Basics.RaisedTo = "rt.raisedTo"
textOfBinOp Basics.FlowsTo  = "rt.flowsTo"
textOfBinOp Basics.Concat   = "rt.stringConcat"

textOfUnOp Basics.IsList  = "rt.islist"
textOfUnOp Basics.IsTuple = "rt.istuple"
textOfUnOp Basics.Head    = "rt.head"
textOfUnOp Basics.Tail    = "rt.tail"
textOfUnOp Basics.Fst     = "rt.fst"
textOfUnOp Basics.Snd     = "rt.snd"
textOfUnOp Basics.Length  = "rt.length"
textOfUnOp Basics.LevelOf = "rt.levelOf"
textOfUnOp Basics.UnMinus = "rt.unaryMinus"

