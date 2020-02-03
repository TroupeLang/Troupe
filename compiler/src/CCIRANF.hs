-- 2019-03-22: closure converted IR based on ANF 
--

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
module CCIRANF where

import qualified Basics
import           RetCPS                    (VarName (..))


import qualified Core                      as C
import qualified RetCPS                    as CPS

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import qualified Data.ByteString           as BS
import           Data.Serialize            (Serialize)
import qualified Data.Serialize            as Serialize
import           GHC.Generics              (Generic)

import           CompileMode
import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo

data VarAccess
  = VarLocal VarName
  | VarEnv VarName
  deriving (Eq, Show, Generic)

type Ident = String

newtype HFN  = HFN Ident deriving (Eq, Show, Ord, Generic)

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


data IRBBTree = BB [IRInst] IRTerminator deriving (Eq, Generic)

data IRTerminator
  = TailCall VarAccess VarAccess
  | Ret VarAccess
  | If VarAccess IRBBTree IRBBTree
  | AssertElseError VarAccess IRBBTree VarAccess PosInf
  | LibExport VarAccess
  | Error VarAccess PosInf
  | Call VarName IRBBTree IRBBTree
  deriving (Eq,Generic)


data IRInst
  = Assign VarName IRExpr
  | MkFunClosures [(VarName, VarAccess)] [(VarName, HFN)]
  
 deriving (Eq, Generic)

-- Function definition
data FunDef = FunDef 
                    HFN         -- name of the function
                    VarName     -- name of the argument
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
  dependencies :: a -> Writer ([HFN], [Basics.LibName])  ()

instance ComputesDependencies IRInst where 
   dependencies (MkFunClosures _ fdefs) = 
        mapM_ (\(_, hfn) -> tell ([hfn],[])) fdefs
   dependencies (Assign _ (Lib libname _)) = 
        tell ([], [libname])
                                       
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
  dependencies (FunDef _ _ bb) = dependencies bb


ppDeps :: ComputesDependencies a => a -> (PP.Doc , PP.Doc)
ppDeps a = let (ffs_0,lls_0) = execWriter  (dependencies a)               
               (ffs, lls) = (nub ffs_0, nub lls_0)

               format dd =
                   let tt = map (PP.quotes . ppId) dd in 
                   (PP.brackets.PP.hsep) (PP.punctuate PP.comma tt)
            in ( format ffs, format lls )            


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
      if wfFun fdecl then (Right x)
      else Left "ir not well-formed"
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
    -- in a JS output returing a runtime error (which should
    -- generally be avoided)
     if  fname `elem`[ 
                       "$$authorityarg"
                     , "adv"
                     , "attenuate"
                     , "debugpc"
                     , "declassify"
                     , "exit"
                     , "flowsTo"
                     , "getTime"
                     , "inputLine"
                     , "intToString"
                     , "levelOf"
                     , "mkuuid"
                     , "mkSecret"
                     , "monitorlocal"
                     , "newlabel"
                     , "node"
                     , "pcpop"
                     , "pinipush"
                     , "pinipop"
                     , "pcpush"
                     , "print"
                     , "printWithLabels"
                     , "printString"
                     , "question"
                     , "raiseTrust"
                     , "receive"
                     , "register"
                     , "rcv"
                     , "rcvp"
                     , "sandbox"
                     , "save"
                     , "send"
                     , "self"
                     , "sleep"
                     , "spawn"
                     , "stringToInt"
                     , "restore"
                     , "toStringL"
                     , "toString"
                     , "whereis"
                     , "writeString"
                     ]
        then return ()
        else fail $ "bad base function: " ++ fname
  wfir _ = return ()



-- todo; 2018-02-18; not checking atoms at the moment
-- they may need to be checked too...

wfIRProg :: IRProgram -> Bool
wfIRProg (IRProgram _ funs) =
  and $ (map wfFun funs)

wfFun (FunDef (HFN fn) (VN arg) bb) =
          checkFromBB [fn, arg] bb

checkFromBB initState bb =
          case evalState (runExceptT (wfir bb)) initState of
            Right _ -> True
            Left s  -> False   -- todo: better exception handling here in the future;
                               -- 2018-02-18; aa

-----------------------------------------------------------
-- PRETTY PRINTING
-----------------------------------------------------------

ppProg (IRProgram atoms funs) =
  vcat $ (map ppFunDef funs)

instance Show IRProgram where
  show = PP.render.ppProg

ppFunDef (FunDef hfn  arg insts)
  = vcat [ text "func" <+> ppFunCall (ppId hfn) [ppId arg] <+> text "{"
         , nest 2 (ppBB insts)
         , text "}"]



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

ppArgs args = PP.parens( PP.hcat (PP.punctuate PP.comma args))

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

