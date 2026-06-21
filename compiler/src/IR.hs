-- 2019-03-22: closure converted IR based on ANF
--

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module IR where

import           Consts
import qualified Basics
import           RetCPS                    (VarName (..))


import qualified Core                      as C
import qualified RetCPS                    as CPS
import           Core (ppLit)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import Control.Monad (when)
import           Data.List
import qualified Data.ByteString           as BS
import           Data.Serialize            (Serialize)
import qualified Data.Serialize            as Serialize
import           GHC.Generics              (Generic)

import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo (Located(..), getLoc, unLoc, noLoc, atLoc, PosInf(..), GetPosInfo(..))
import           PrettyPrint (PP, PPConfig, runPP, runPPDefault, ppLocated, vcatMapPP, ShowDebug(..))
import           DCLabels

------------------------------------------------------------
-- Located type aliases
------------------------------------------------------------

type LIRInst = Located IRInst
type LIRTerminator = Located IRTerminator
type LFunDef = Located FunDef
type LIRExpr = Located IRExpr
-- | Located VarAccess - carries source position for variable references
type LVarAccess = Located VarAccess
-- | Located VarName - carries source position for variable bindings
type LVarName = Located VarName

-- | Describes a variable containing a labelled value.
data VarAccess
  -- | Local variable with a labelled value.
  = VarLocal VarName
  -- | Variable defined in the closure.
  | VarEnv VarName
  -- | Variable refering to the very function being declared.
  | VarFunSelfRef
  deriving (Eq, Show, Generic)

type Ident = String

newtype HFN  = HFN Ident deriving (Eq, Show, Ord, Generic)

-- | Fields without location info (for backward compatibility)
type Fields =  [(Basics.FieldName, VarAccess)]
-- | Fields with location info for variable references
type LFields = [(Basics.FieldName, LVarAccess)]

-- | IRExpr uses LVarAccess for variable references to preserve source positions
data IRExpr
  = Bin Basics.BinOp LVarAccess LVarAccess
  | Un Basics.UnaryOp LVarAccess
  | Tuple [LVarAccess]
  | Record LFields
  | WithRecord LVarAccess LFields
  | ProjField LVarAccess Basics.FieldName
  -- | Projection of a tuple field at the given index. The maximum allowed index
  -- is 2^31-1 (2147483647).
  | ProjIdx LVarAccess Word
  | List [LVarAccess]
  -- | List cons of a value to a list.
  | ListCons LVarAccess LVarAccess
  -- | Note: This instruction is not generated from source. Constants are stored in function definitions (see 'FunDef').
  | Const C.Lit
  -- | Predefined base function names.
  | Base Basics.VarName
  -- | Returns the definition (variable) with the given name
  -- from the given library.
  | Lib Basics.LibName Basics.VarName
  deriving (Eq, Show, Generic)

-- | A block of instructions followed by a terminator, which can contain further 'IRBBTree's.
-- Instructions and terminator are wrapped in Located for position tracking.
data IRBBTree = BB [LIRInst] LIRTerminator deriving (Eq, Show, Generic)

-- | IRTerminator represents control flow endings of a basic block.
-- Uses LVarAccess (Located VarAccess) to preserve source positions for variable references.
data IRTerminator
  -- | Call the function referred to by the first variable with the argument in the second variable.
  = TailCall LVarAccess LVarAccess
  -- | Return from the current Call with the given variable as return value.
  | Ret LVarAccess
  | If LVarAccess IRBBTree IRBBTree
  -- | Check whether the value of the first variable is true. If yes, continue with the given tree.
  -- If not, terminate the current thread with a runtime error, printing the message stored in the second variable (which is asserted to be a string).
  -- The error source location comes from the Located wrapper (LIRTerminator).
  | AssertElseError LVarAccess IRBBTree LVarAccess
  -- | Make the library available under the given variable.
  | LibExport LVarAccess
  -- | Terminate the current thread with a runtime error, printing the message stored in the variable (which is asserted to be a string).
  -- The error source location comes from the Located wrapper (LIRTerminator).
  | Error LVarAccess
  -- | Execute the first BB, store the returned result in the given variable
  -- and then execute the second BB, which can refer to this variable and
  -- where PC is reset to the level before entering the first BB.
  -- Represents a "let x = ... in ..." format.
  | StackExpand VarName IRBBTree IRBBTree
  deriving (Eq,Show,Generic)


-- | IRInst represents instructions within a basic block.
-- Positions are tracked via Located wrapper (LIRInst).
data IRInst
  = Assign VarName IRExpr
  -- | A closure instruction consists of
  -- - A list of variables that need to be in the environment
  -- - A list of closures with their name and the corresponding compiler-generated name of the function
  | MkFunClosures [(VarName, LVarAccess)] [(VarName, HFN)]
 deriving (Eq, Show, Generic)



-- | A literal together with the variable name the constant is accessed through.
type Consts = [(VarName, C.Lit)]

-- | Function definition
-- The function definition position is on the Located wrapper (LFunDef).
-- Argument position is now on the LVarName wrapper.
data FunDef = FunDef
                    HFN         -- name of the function
                    LVarName    -- argument (name + position)
                    Consts      -- constants used in the function
                    IRBBTree    -- body
                deriving (Eq,Generic)

-- An IR program is just a collection of atoms declarations
-- and function definitions (wrapped with Located for position tracking)
data IRProgram = IRProgram C.Atoms [LFunDef] deriving (Generic)

-----------------------------------------------------------
-- Dependency calculation
-----------------------------------------------------------

-- For dependencies, we only need the function dependencies

class ComputesDependencies a where
  dependencies :: a -> Writer ([HFN], [Basics.LibName], [Basics.AtomName])  ()

instance ComputesDependencies IRInst where
   dependencies (MkFunClosures _ fdefs) =
        mapM_ (\(_, hfn) -> tell ([hfn],[],[])) fdefs
   dependencies (Assign _ (Lib libname _)) =
        tell ([], [libname],[])
   dependencies (Assign _ (Const (C.LAtom a))) =
        tell ([], [], [a])

   dependencies _ = return ()

-- Instance for Located wrapper - extract and delegate
instance ComputesDependencies a => ComputesDependencies (Located a) where
  dependencies (Loc _ a) = dependencies a

instance ComputesDependencies IRBBTree where
    dependencies (BB insts trm) =
        do mapM_ dependencies insts
           dependencies trm

instance ComputesDependencies IRTerminator where
    dependencies (If _ bb1 bb2) = mapM_ dependencies [bb1, bb2]
    dependencies (AssertElseError _ bb1 _) = dependencies bb1
    dependencies (StackExpand _ t1 t2) = dependencies t1  >> dependencies t2

    dependencies _              = return ()

instance ComputesDependencies FunDef where
  dependencies (FunDef _ _ _ bb) = dependencies bb


ppDepsAsJSON :: ComputesDependencies a => a -> (PP.Doc , PP.Doc, PP.Doc)
ppDepsAsJSON a = let (ffs_0,lls_0, atoms_0) = execWriter  (dependencies a)
                     (ffs, lls, aas) = (nub ffs_0, nub lls_0, nub atoms_0)

                     format dd =
                       let tt = map (PP.doubleQuotes . ppId) dd
                       in (PP.brackets.PP.hsep) (PP.punctuate PP.comma tt)
                 in ( format ffs, format lls , format aas )

ppDeps a = ppDepsAsJSON a


-----------------------------------------------------------
-- Serialization instances
-----------------------------------------------------------

instance Serialize IRProgram
instance Serialize IRTerminator
instance Serialize FunDef
instance Serialize VarAccess
instance Serialize HFN
instance Serialize IRExpr
instance Serialize IRInst
instance Serialize IRBBTree

-----------------------------------------------------------
-- Serialization 
-----------------------------------------------------------
data SerializationUnit
  = FunSerialization FunDef
  | AtomsSerialization C.Atoms
  | ProgramSerialization IRProgram
  deriving (Generic)

instance Serialize SerializationUnit


serializeFunDef :: FunDef -> BS.ByteString
serializeFunDef fdef = Serialize.runPut ( Serialize.put (FunSerialization fdef) )

serializeAtoms :: C.Atoms -> BS.ByteString
serializeAtoms atoms = Serialize.runPut (Serialize.put (AtomsSerialization atoms))

deserializeAtoms :: BS.ByteString -> Either String C.Atoms
deserializeAtoms bs = Serialize.runGet (Serialize.get) bs

deserialize :: BS.ByteString -> Either String SerializationUnit
deserialize bs =
  case Serialize.runGet (Serialize.get) bs of
    Left s -> Left s
    Right x@(FunSerialization fdecl) ->
      case runExcept (wfFun fdecl) of 
        Right  _ -> Right x 
        Left s -> Left  "ir not well-formed"
      -- if wfFun fdecl then (Right x)
      -- else Left "ir not well-formed"
    Right x -> Right x

-----------------------------------------------------------
-- Well-formedness
-----------------------------------------------------------

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
 wfir (Assign (VN x) e) = do checkId x
                             wfir e
 wfir (MkFunClosures _ fdefs) = mapM_ (\((VN x), _) -> checkId x) fdefs

-- Instance for Located wrapper - extract and delegate
instance WellFormedIRCheck a => WellFormedIRCheck (Located a) where
  wfir (Loc _ a) = wfir a

instance WellFormedIRCheck IRTerminator where
  wfir (If _ bb1 bb2) = do
    wfir bb1
    wfir bb2
  wfir (AssertElseError _ bb _) = wfir bb
  wfir (StackExpand (VN x) bb1 bb2) = do
    checkId x
    wfir bb1
    wfir bb2

  wfir _ = return ()


instance WellFormedIRCheck IRBBTree where
  wfir (BB insts tr) = do
    mapM_ wfir insts
    wfir tr

instance WellFormedIRCheck IRExpr where
  wfir (Base fname) =
    -- OBS: AA: 2018-07-24: This is the only
    -- place where we check the base functions
    -- (but this should be sufficient though). Note
    -- that it is important to do this check at the level
    -- of the IR because we otherwise may get a malformed
    -- code over wire. Such malformed code would result
    -- in a JS output returning a runtime error (which should
    -- generally be avoided)
     if  fname `elem`[
                       "$$authorityarg"
                     , "adv"
                     , "ladv"
                     , "attenuate"
                     , "_blockThread"
                     , "blockdecl"
                     , "blockdeclto"
                     , "blockdown"
                     , "blockdownto"
                     , "blockendorse"
                     , "blockendorseto"
                     , "cert"
                     , "charCodeAtWithDefault"
                     , "coalesce"
                     , "consume"
                     , "_debug"
                     , "debugMbox"
                     , "debugpc"
                     , "debugValue"
                     , "declassify"
                     , "declassifyType"
                     , "downgrade"
                     , "downgradeType"
                     , "exit"
                     , "endorse"
                     , "endorseType"
                     , "floor"
                     , "flowsTo"                     
                     , "freadln"
                     , "fwrite"
                     , "getTime"
                     , "getCliArgs"
                     , "getType"
                     , "getNanoTime"
                     , "_getSystemProcess"
                     , "guard"
                     , "intToString"                     
                     , "listToTuple"
                     , "lowermbox"
                     , "levelOf"
                     , "glb"
                     , "lub"
                     , "mkuuid"
                     , "mkSecret"
                     , "monitorlocal"
                     , "newlabel"                     
                     , "node"
                     , "_pc"
                     , "_bl"
                  -- , "pcpop"
                     , "peek"
                     , "pinipush"
                     , "pinipushto"
                     , "pinipop"
                    --  , "pcpush"                      
                     , "raisembox"
                     , "raiseTrust"
                     , "random"
                     , "receive"
                     , "recordExtend"
                     , "recordToList"
                     , "register"
                     , "_resetScheduler"
                     , "rcv"
                     , "rcvp"
                     , "sandbox"
                     , "save"
                     , "send"
                     , "self"
                     , "_servicetest"
                     , "_setProcessDebuggingName"
                     , "_setFailureRate"
                     , "sleep"
                     , "spawn"
                     , "sqrt"
                     , "substring"
                     , "stdin"
                     , "stdout"
                     , "stderr"
                     , "stringToInt"
                     , "strlen"
                     , "restore"
                     , "toStringL"
                     , "toString"
                     , "whereis"
                                      
                     ]
        then return ()
        else throwError $ "bad base function: " ++ fname
  wfir (ProjIdx _ idx) =
    when (idx > (fromIntegral Consts.llvm_maxIndex :: Word)) $
      throwError $ "ProjIdx: illegal index: " ++ show idx ++ " (max index: " ++ show Consts.llvm_maxIndex ++ ")"

  wfir _ = return ()



wfIRProg :: IRProgram -> Except String ()
wfIRProg (IRProgram (C.Atoms atms) funs) = do
  let duplicates = atms \\ nub atms
  when (not (null duplicates)) $
    throwError $ "Duplicate atom names: " ++ show (nub duplicates)
  mapM_ wfLFun funs

-- | Check well-formedness of a Located FunDef
wfLFun :: LFunDef -> Except String ()
wfLFun (Loc _ fdef) = wfFun fdef

wfFun :: FunDef -> Except String ()
wfFun (FunDef (HFN fn) (Loc _ (VN arg)) consts bb) =
    let initVars =[ fn,arg] ++ [i  | VN i <-  fst (unzip consts)]
        act = do
            mapM checkId initVars
            wfir bb
    in

    case evalState (runExceptT act) [] of
      Right _ -> return ()
      Left s -> throwError s


{--
checkFromBB initState bb =
          case evalState (runExceptT (wfir bb)) initState of
            Right _ -> True
            Left s  -> error s -- False   -- todo: better exception handling here in the future;
                               -- 2018-02-18; aa
--}
-----------------------------------------------------------
-- PRETTY PRINTING
-----------------------------------------------------------

ppProg :: IRProgram -> PP PP.Doc
ppProg (IRProgram atoms funs) =
  vcatMapPP ppLFunDef funs

instance Show IRProgram where
  show = PP.render . runPPDefault . ppProg

instance ShowDebug IRProgram where
  showDebugWith cfg = PP.render . runPP cfg . ppProg

ppConsts :: [(VarName, C.Lit)] -> PP.Doc
ppConsts consts =
  vcat $ map ppConst consts
    where ppConst (x, lit) = hsep [ ppId x , text "=", ppLit lit ]

ppLFunDef :: LFunDef -> PP PP.Doc
ppLFunDef = ppLocated ppFunDef

ppFunDef :: FunDef -> PP PP.Doc
ppFunDef (FunDef hfn (Loc _ arg) consts insts) = do
  bbDoc <- ppBB insts
  pure $ vcat [ text "func" <+> ppFunCall (ppId hfn) [ppId arg] <+> text "{"
              , nest 2 (ppConsts consts)
              , nest 2 bbDoc
              , text "}"]



-- | Pretty print a Located VarAccess (extracts VarAccess and prints)
ppLVA :: LVarAccess -> PP PP.Doc
ppLVA = ppLocated (pure . ppId)

ppIRExpr :: IRExpr -> PP PP.Doc
ppIRExpr (Bin binop lva1 lva2) = do
  d1 <- ppLVA lva1
  d2 <- ppLVA lva2
  pure $ d1 <+> text (show binop) <+> d2
ppIRExpr (Un op lv) = do
  d <- ppLVA lv
  pure $ text (show op) PP.<> PP.parens d
ppIRExpr (Tuple vars) = do
  ds <- mapM ppLVA vars
  pure $ PP.parens $ PP.hsep $ PP.punctuate (text ",") ds
ppIRExpr (List vars) = do
  ds <- mapM ppLVA vars
  pure $ PP.brackets $ PP.hsep $ PP.punctuate (text ",") ds
ppIRExpr (ListCons lv1 lv2) = do
  d1 <- ppLVA lv1
  d2 <- ppLVA lv2
  pure $ text "cons" PP.<> (PP.parens $ d1 PP.<> text "," PP.<> d2)
ppIRExpr (Const (C.LUnit)) = pure $ text "__unit"
ppIRExpr (Const lit) = pure $ ppLit lit
ppIRExpr (Base v) = pure $ if v == "$$authorityarg" -- special casing; hack; 2018-10-18: AA
                      then text v
                      else text v PP.<> text "$base"
ppIRExpr (Lib (Basics.LibName l) v) = pure $ text l PP.<> text "." PP.<> text v
ppIRExpr (Record fields) = do
  fDoc <- qqLFields fields
  pure $ PP.braces fDoc
ppIRExpr (WithRecord lv fields) = do
  lvDoc <- ppLVA lv
  fDoc <- qqLFields fields
  pure $ PP.braces $ PP.hsep [lvDoc, text "with", fDoc]
ppIRExpr (ProjField lv f) = do
  d <- ppLVA lv
  pure $ d PP.<> PP.text "." PP.<> PP.text f
ppIRExpr (ProjIdx lv idx) = do
  d <- ppLVA lv
  pure $ d PP.<> PP.text "." PP.<> PP.text (show idx)

-- | Pretty print LFields (fields with Located VarAccess)
qqLFields :: LFields -> PP PP.Doc
qqLFields fields = do
  fieldDocs <- mapM ppField fields
  pure $ PP.hsep $ PP.punctuate (text ",") fieldDocs
    where
      ppField (name, lv) = do
        lvDoc <- ppLVA lv
        pure $ PP.hcat [PP.text name, PP.text "=", lvDoc]

ppLIR :: LIRInst -> PP PP.Doc
ppLIR = ppLocated ppIR

ppIR :: IRInst -> PP PP.Doc
ppIR (Assign vn st) = do
  exprDoc <- ppIRExpr st
  pure $ ppId vn <+> text "=" <+> exprDoc

ppIR (MkFunClosures varmap fdefs) =
    let vs = hsepc $ ppEnvIds varmap
        ppFdefs = map (\((VN x), HFN y) ->  text x <+> text "= mkClos" <+> text y ) fdefs
     in pure $ text "with env:=" <+> PP.brackets vs $$ nest 2 (vcat ppFdefs)
    where ppEnvIds ls =
            map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
          hsepc ls = PP.hsep (PP.punctuate (text ",") ls)


ppLTr :: LIRTerminator -> PP PP.Doc
ppLTr = ppLocated ppTr

ppTr :: IRTerminator -> PP PP.Doc
ppTr (StackExpand vn bb1 bb2) = do
  bb1Doc <- ppBB bb1
  bb2Doc <- ppBB bb2
  pure $ (ppId vn <+> text "= call" $$ nest 2 bb1Doc) $$ bb2Doc


ppTr (AssertElseError lva ir lva2) = do
  irDoc <- ppBB ir
  lvaDoc <- ppLVA lva
  lva2Doc <- ppLVA lva2
  pure $ text "assert" <+> PP.parens lvaDoc <+>
    text "{" $$
    nest 2 irDoc $$
    text "}" $$
    text "elseError" <+> lva2Doc


ppTr (If lva ir1 ir2) = do
  ir1Doc <- ppBB ir1
  ir2Doc <- ppBB ir2
  lvaDoc <- ppLVA lva
  pure $ text "if" <+> PP.parens lvaDoc <+>
    text "{" $$
    nest 2 ir1Doc $$
    text "}" $$
    text "else {" $$
    nest 2 ir2Doc $$
    text "}"
ppTr (TailCall lva1 lva2) = do
  d1 <- ppLVA lva1
  d2 <- ppLVA lva2
  pure $ ppFunCall (text "tail") [d1, d2]
ppTr (Ret lva) = do
  d <- ppLVA lva
  pure $ ppFunCall (text "ret") [d]
ppTr (LibExport lva) = do
  d <- ppLVA lva
  pure $ ppFunCall (text "export") [d]
ppTr (Error lva) = do
  d <- ppLVA lva
  pure $ (text "error") PP.<> d


ppBB :: IRBBTree -> PP PP.Doc
ppBB (BB insts tr) = do
  instDocs <- mapM ppLIR insts
  trDoc <- ppLTr tr
  pure $ vcat $ instDocs ++ [trDoc]



-----------------------------------------------------------
-- Utils
-----------------------------------------------------------
ppVarName :: VarName -> PP.Doc
ppVarName (VN vn) = text vn

ppVarAccess :: VarAccess -> PP.Doc
ppVarAccess (VarLocal vn) = ppVarName vn
ppVarAccess (VarEnv vn) = text "$env." PP.<> (ppVarName vn)
ppVarAccess (VarFunSelfRef) = text "<fun-self-ref>"

class Identifier a where
  ppId :: a ->  PP.Doc


instance Identifier VarName where
  ppId = ppVarName

instance Identifier VarAccess where
  ppId = ppVarAccess

-- | Instance for Located wrapper - extracts content and prints it
instance Identifier a => Identifier (Located a) where
  ppId (Loc _ a) = ppId a

instance Identifier HFN where
  ppId (HFN n) = text n

instance Identifier Basics.LibName where 
  ppId (Basics.LibName s) = text s

instance Identifier Basics.AtomName where 
  ppId = text


ppArgs args = PP.parens( PP.hcat (PP.punctuate PP.comma args))

ppFunCall fn args = fn <+> ppArgs args



