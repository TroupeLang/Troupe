-- 2019-03-22: closure converted IR based on ANF 
--

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module IR where

import qualified Basics
import           RetCPS                    (VarName (..))


import qualified Core                      as C
import qualified RetCPS                    as CPS

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

import           CompileMode
import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo

-- | Describes a variable containing a labelled value.
data VarAccess
  -- | Local variable with a labelled value.
  = VarLocal VarName
  -- | Variable defined in the closure.
  | VarEnv VarName
  deriving (Eq, Show, Generic)

type Ident = String

newtype HFN  = HFN Ident deriving (Eq, Show, Ord, Generic)

type Fields =  [(Basics.FieldName, VarAccess)]
data IRExpr
  = Bin Basics.BinOp VarAccess VarAccess
  | Un Basics.UnaryOp VarAccess
  | Tuple [VarAccess]
  | Record Fields
  | WithRecord VarAccess Fields 
  | ProjField VarAccess Basics.FieldName
  -- | Projection of a tuple field at the given index. The maximum allowed index
  -- is 2^53-1 (9007199254740991), the maximum representable number with 52 bits (unsigned),
  -- as the runtime uses the IEEE 754 double-precision number format with a mantissa of 52 bits.
  | ProjIdx VarAccess Word
  | List [VarAccess]
  -- | List cons of a value to a list.
  | ListCons VarAccess VarAccess
  -- | Note: This instruction is not generated from source. Constants are stored in function definitions (see 'FunDef').
  | Const C.Lit
  -- | Predefined base function names.
  | Base Basics.VarName
  -- | Returns the definition (variable) with the given name
  -- from the given library.
  | Lib Basics.LibName Basics.VarName
  deriving (Eq, Show, Generic)

-- | A block of instructions followed by a terminator, which can contain further 'IRBBTree's.
data IRBBTree = BB [IRInst] IRTerminator deriving (Eq, Show, Generic)

data IRTerminator
  -- | Call the function referred to by the first variable with the argument in the second variable.
  = TailCall VarAccess VarAccess
  -- | Return from the current Call with the given variable as return value.
  | Ret VarAccess
  | If VarAccess IRBBTree IRBBTree
  -- | Check whether the value of the first variable is true. If yes, continue with the given tree.
  -- If not, terminate the current thread with a runtime error, printing the message stored in the second variable (which is asserted to be a string) with the given PosInf.
  | AssertElseError VarAccess IRBBTree VarAccess PosInf
  -- | Make the library available under the given variable.
  | LibExport VarAccess
  -- | Terminate the current thread with a runtime error, printing the message stored in the variable (which is asserted to be a string) with the given PosInf.
  | Error VarAccess PosInf
  -- | Execute the first BB, store the returned result in the given variable
  -- and then execute the second BB, which can refer to this variable and
  -- where PC is reset to the level before entering the first BB.
  -- Represents a "let x = ... in ..." format.
  | Call VarName IRBBTree IRBBTree
  deriving (Eq,Show,Generic)


data IRInst
  = Assign VarName IRExpr
  -- | A closure instruction consists of
  -- - A list of variables that need to be in the environment
  -- - A list of closures with their name and the corresponding compiler-generated name of the function
  | MkFunClosures [(VarName, VarAccess)] [(VarName, HFN)]
  
 deriving (Eq, Show, Generic)



-- | A literal together with the variable name the constant is accessed through.
type Consts = [(VarName, C.Lit)]
-- Function definition
data FunDef = FunDef 
                    HFN         -- name of the function
                    VarName     -- name of the argument
                    Consts      -- constants used in the function  
                    IRBBTree    -- body
                deriving (Eq,Generic)

-- An IR program is just a collection of atoms declarations 
-- and function definitions
data IRProgram = IRProgram C.Atoms [FunDef] deriving (Generic)

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

instance ComputesDependencies IRBBTree where
    dependencies (BB insts trm) = 
        do mapM_ dependencies insts 
           dependencies trm

instance ComputesDependencies IRTerminator where 
    dependencies (If _ bb1 bb2) = mapM_ dependencies [bb1, bb2]
    dependencies (AssertElseError _ bb1 _ _) = dependencies bb1
    dependencies (Call _ t1 t2) = dependencies t1  >> dependencies t2

    dependencies _              = return ()
instance ComputesDependencies FunDef where
  dependencies (FunDef _ _ _ bb) = dependencies bb


ppDeps :: ComputesDependencies a => a -> (PP.Doc , PP.Doc, PP.Doc)
ppDeps a = let (ffs_0,lls_0, atoms_0) = execWriter  (dependencies a)               
               (ffs, lls, aas) = (nub ffs_0, nub lls_0, nub atoms_0)

               format dd =
                   let tt = map (PP.quotes . ppId) dd in 
                   (PP.brackets.PP.hsep) (PP.punctuate PP.comma tt)
            in ( format ffs, format lls , format aas )            


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
 

instance WellFormedIRCheck IRTerminator where
  wfir (If _ bb1 bb2) = do
    wfir bb1
    wfir bb2
  wfir (AssertElseError _ bb _ _) = wfir bb
  wfir (Call (VN x) bb1 bb2 ) = do 
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
                     , "arrayCreate"
                     , "arrayGet"
                     , "arraySet"
                     , "attenuate"
                     , "_blockThread"
                     , "blockdecl"
                     , "blockdeclto"
                     , "ceil"
                     , "consume"
                     , "_debug"
                     , "debugpc"
                     , "declassify"
                     , "exit"
                     , "floor"
                     , "flowsTo"                     
                     , "fprintln"
                     , "fprintlnWithLabels"
                     , "fwrite"                     
                     , "getTime"
                     , "getNanoTime"
                     , "getStdout"
                     , "guard"
                     , "inputLine"
                     , "intToString"                     
                     , "lowermbox"                    
                     , "levelOf"
                     , "mkuuid"
                     , "mkSecret"
                     , "monitorlocal"
                     , "newlabel"                     
                     , "node"
                     , "_pc"
                     , "pcpop"
                     , "peek"
                     , "pinipush"
                     , "pinipushto"
                     , "pinipop"
                     , "pcpush"                      
                     , "question"
                     , "raisembox"
                     , "raiseTrust"
                     , "random"
                     , "receive"
                     , "recordExtend"
                     , "register"
                     , "_resetScheduler"
                     , "rcv"
                     , "rcvp"
                     , "round"
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
                     , "stringToInt"
                     , "restore"
                     , "toStringL"
                     , "toString"
                     , "whereis"                 
                                      
                     ]
        then return ()
        else throwError $ "bad base function: " ++ fname
  wfir (ProjIdx _ idx) =
    when (idx > 9007199254740991) $ -- 2^53-1
      throwError $ "ProjIdx: illegal index: " ++ show idx ++ " (max index: 9007199254740991)"

  wfir _ = return ()



-- todo; 2018-02-18; not checking atoms at the moment
-- they may need to be checked too...

wfIRProg :: IRProgram -> Except String ()
wfIRProg (IRProgram _ funs) = mapM_ wfFun funs

wfFun :: FunDef -> Except String () 
wfFun (FunDef (HFN fn) (VN arg) consts bb) = 
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

ppProg (IRProgram atoms funs) =
  vcat $ (map ppFunDef funs)

instance Show IRProgram where
  show = PP.render.ppProg

ppConsts consts = 
  vcat $ map ppConst consts 
    where ppConst (x, lit) = hsep [ ppId x , text "=", CPS.ppLit lit ]

ppFunDef (FunDef hfn  arg consts insts)
  = vcat [ text "func" <+> ppFunCall (ppId hfn) [ppId arg] <+> text "{"
         , nest 2 (ppConsts consts)
         , nest 2 (ppBB insts)
         , text "}"]



ppIRExpr :: IRExpr -> PP.Doc
ppIRExpr (Bin binop va1 va2) =
  ppId va1 <+> text (show binop) <+> ppId va2
ppIRExpr (Un op v) =
  text (show op) <> PP.parens (ppId v)
ppIRExpr (Tuple vars) =
  PP.parens $ PP.hsep $ PP.punctuate (text ",") (map ppId vars)
ppIRExpr (List vars) =
  PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppId vars)
ppIRExpr (ListCons v1 v2) =
  text "cons" <>  ( PP.parens $ ppId v1 <> text "," <> ppId v2)
ppIRExpr (Const (C.LUnit)) = text "__unit"
ppIRExpr (Const lit) = CPS.ppLit lit
ppIRExpr (Base v) = if v == "$$authorityarg" -- special casing; hack; 2018-10-18: AA
                      then text v 
                      else text v <> text "$base"
ppIRExpr (Lib (Basics.LibName l) v) = text l <> text "." <> text v
ppIRExpr (Record fields) = PP.braces $ qqFields fields
ppIRExpr (WithRecord x fields) = PP.braces $ PP.hsep[ ppId x, text "with", qqFields fields]
ppIRExpr (ProjField x f) = 
  (ppId x) PP.<> PP.text "." PP.<> PP.text f
ppIRExpr (ProjIdx x idx) = 
  (ppId x) PP.<> PP.text "." PP.<> PP.text (show idx)
    
qqFields fields =
  PP.hsep $ PP.punctuate (text ",") (map ppField fields)
    where 
      ppField (name, v) = 
        PP.hcat [PP.text name, PP.text "=", ppId v]

ppIR :: IRInst -> PP.Doc
ppIR (Assign vn st) = ppId vn <+> text "=" <+> ppIRExpr st

ppIR (MkFunClosures varmap fdefs) = 
    let vs = hsepc $ ppEnvIds varmap
        ppFdefs = map (\((VN x), HFN y) ->  text x <+> text "= mkClos" <+> text y ) fdefs 
     in text "with env:=" <+> PP.brackets vs $$ nest 2 (vcat ppFdefs)
    where ppEnvIds ls =
            map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
          hsepc ls = PP.hsep (PP.punctuate (text ",") ls)

    

ppTr (Call vn bb1 bb2) = (ppId vn <+> text "= call" $$ nest 2 (ppBB bb1)) $$ (ppBB bb2)


ppTr (AssertElseError va ir va2 _) 
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
ppTr (Error va _)  = (text "error") <> (ppId va)


ppBB (BB insts tr) = vcat $ (map ppIR insts) ++ [ppTr tr]



-----------------------------------------------------------
-- Utils
-----------------------------------------------------------
ppVarName :: VarName -> PP.Doc
ppVarName (VN vn) = text vn

ppVarAccess :: VarAccess -> PP.Doc
ppVarAccess (VarLocal vn) = ppVarName vn
ppVarAccess (VarEnv vn) = text "$env." PP.<> (ppVarName vn)


class Identifier a where
  ppId :: a ->  PP.Doc


instance Identifier VarName where
  ppId = ppVarName

instance Identifier VarAccess where
  ppId = ppVarAccess

instance Identifier HFN where
  ppId (HFN n) = text n

instance Identifier Basics.LibName where 
  ppId (Basics.LibName s) = text s

instance Identifier Basics.AtomName where 
  ppId = text


ppArgs args = PP.parens( PP.hcat (PP.punctuate PP.comma args))

ppFunCall fn args = fn <+> ppArgs args



