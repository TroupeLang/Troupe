{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Raw where 

import qualified Basics
import           RetCPS (VarName (..))
import           IR ( Identifier(..)
                    , VarAccess(..), HFN (..), Fields (..), Ident
                    , ppId,ppFunCall,ppArgs
                    )
import qualified IR (textOfBinOp,textOfUnOp,FunDef (..))


import qualified Core                      as C
import qualified RetCPS                    as CPS
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map 


import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import qualified Data.ByteString           as BS

import           CompileMode
import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo


newtype RawVar = RawVar Ident deriving (Eq, Show, Ord)
instance Identifier RawVar where ppId (RawVar x) = text x


type ConstMap = Map RawVar C.Lit 

instance Identifier Assignable where 
  ppId (AssignableRaw x) = ppId x 
  ppId (AssignableLVal x) = ppId x 
  ppId (Env) = text "$env"

data LValField = FieldValue | FieldValLev | FieldTypLev deriving (Eq, Ord)
instance Show LValField where 
    show FieldValue = "val"
    show FieldValLev = "lev"
    show FieldTypLev = "tlev"

data MonComponent = MonPC | MonBlock | R0_Val | R0_Lev | R0_TLev
     deriving (Eq, Show, Ord)
instance Identifier MonComponent where 
  ppId MonPC = text "<pc>"
  ppId MonBlock = text "<block>"
  ppId R0_Val = text "<r0_val>"
  ppId R0_Lev = text "<r0_lev>"
  ppId R0_TLev = text "<r0_tlev>"


data RawExpr
  = Bin Basics.BinOp RawVar RawVar
  | Un Basics.UnaryOp RawVar
  | ProjectLVal VarAccess LValField
  | ProjectState MonComponent
  | Tuple [VarAccess]
  | Record Fields
  | WithRecord RawVar Fields 
  | Proj RawVar Basics.FieldName
  | List [VarAccess]
  | ListCons VarAccess RawVar
  | Const C.Lit
  | Lib Basics.LibName Basics.VarName  
  deriving (Eq, Show)


data RawBBTree = BB [RawInst] RawTerminator deriving (Eq, Show)

data RawTerminator
  = TailCall RawVar
  | Ret 
  | If RawVar RawBBTree RawBBTree
  | LibExport VarAccess
  | Error RawVar PosInf
  | Call RawBBTree RawBBTree
  deriving (Eq, Show)


data RawType 
  = RawNumber| RawUnit | RawBoolean | RawString | RawFunction 
  | RawLocalObj| RawHandler| RawList | RawTuple| RawRecord
  | RawNode| RawProcessId| RawCapability| RawLevel
  | RawAuthority | RawTopAuthority| RawEnv
    deriving (Eq, Show)
  


data ComplexExpr 
  = Base Basics.VarName
  | ComplexBin Basics.BinOp VarAccess VarAccess
  | ConstructLVal RawVar RawVar RawVar
  | ComplexRaw RawExpr
    deriving (Eq, Show)


data List2OrMore a = List2OrMore a a [a] deriving (Eq, Show)

data RawInst
  = AssignRaw RawVar RawExpr
  | AssignLVal VarName ComplexExpr
  | SetState MonComponent RawVar 
  | AssertType RawVar RawType  
  | AssertEqTypes (Maybe (List2OrMore RawType)) RawVar RawVar  -- the list includes an optional list of okay types
  | SetBranchFlag 
  | MkFunClosures [(VarName, VarAccess)] [(VarName, HFN)]
   deriving (Eq, Show)


type Consts = [(RawVar, C.Lit )]

-- Function definition
data FunDef = FunDef 
                    HFN          -- name of the function          
                    Consts   
                    RawBBTree    -- body
                    IR.FunDef    -- original definition for serialization
                deriving (Eq)

-- An IR program is just a collection of atoms declarations 
-- and function definitions
data RawProgram = RawProgram C.Atoms [FunDef] 


-----------------------------------------------------------
-- Serialization 
-----------------------------------------------------------
data RawUnit 
  = FunRawUnit FunDef 
  | AtomRawUnit C.Atoms 
  | ProgramRawUnit RawProgram 



-----------------------------------------------------------
-- AUX DECLARATIONS
-----------------------------------------------------------



data Assignable = AssignableRaw Raw.RawVar 
                | AssignableLVal VarName
                | Env
                deriving (Eq, Ord, Show)


data RegularInstructionKind 
  = RegConstructor 
  | RegDestructor
  | RegOther 
      deriving (Eq, Ord, Show)

data InstructionType 
  = RegularInstruction RegularInstructionKind
  | LabelSpecificInstruction
    deriving (Eq, Ord, Show)

instructionType i = case i of 
  AssignRaw _ (Bin Basics.LatticeJoin _ _) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonPC) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonBlock) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_Lev)  -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_TLev) -> LabelSpecificInstruction
  AssignLVal _ (ConstructLVal _ _ _) -> RegularInstruction RegConstructor
  AssignRaw _ (ProjectLVal _ _) -> RegularInstruction RegDestructor
  SetBranchFlag -> RegularInstruction RegConstructor
  SetState s _ -> 
    case s of 
      R0_Val -> RegularInstruction RegConstructor 
      R0_Lev -> RegularInstruction RegConstructor
      R0_TLev -> RegularInstruction RegConstructor
      _ -> LabelSpecificInstruction
  _ -> RegularInstruction RegOther



-----------------------------------------------------------
-- PRETTY PRINTING
-----------------------------------------------------------

ppProg (RawProgram atoms funs) =
  vcat $ (map ppFunDef funs)

instance Show RawProgram where
  show = PP.render.ppProg

ppFunDef ( FunDef hfn consts insts  _ )
  = vcat [ text "func" <+> ppFunCall (ppId hfn) [] <+> text "{"
         , nest 2 (ppConsts consts )
         , nest 2 (ppBB insts)
         , text "}"]



ppRawExpr :: RawExpr -> PP.Doc
ppRawExpr (Bin Basics.Index va1 va2) =
  ppId va1 <> text "[" <> ppId va2 <> text "]"
ppRawExpr (Bin binop va1 va2) =
  ppId va1 <+> text (textOfBinOp binop) <+> ppId va2
ppRawExpr (Un op v) =
  text (textOfUnOp op) <> PP.parens (ppId v)
ppRawExpr (Tuple vars) =
  PP.parens $ PP.hsep $ PP.punctuate (text ",") (map ppId vars)
ppRawExpr (List vars) =
  PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppId vars)
ppRawExpr (ListCons v1 v2) =
  text "cons" <> (PP.parens $ ppId v1 <> text "," <> ppId v2)
ppRawExpr (Const C.LUnit) = text "__unit"
ppRawExpr (Const lit) = CPS.ppLit lit
-- ppRawExpr (Base v) = if v == "$$authorityarg" -- special casing; hack; 2018-10-18: AA
--                       then text v 
--                       else text v <> text "$base"
ppRawExpr (Lib (Basics.LibName l) v) = text l <> text "." <> text v
ppRawExpr (Record fields) = PP.braces $ qqFields fields
ppRawExpr (WithRecord x fields) = PP.braces $ PP.hsep[ ppId x, text "with", qqFields fields]
ppRawExpr (Proj x f) = 
  (ppId x) PP.<> PP.text "." PP.<> PP.text f
ppRawExpr (ProjectLVal v f) = 
  (ppId v) PP.<> text "." PP.<>  PP.text (show f)
ppRawExpr (ProjectState cmp) = ppId cmp


ppComplexExpr (Base v) = text v 
ppComplexExpr (ComplexBin binop va1 va2) =
  ppId va1 <+> text (textOfBinOp binop) <+> ppId va2
ppComplexExpr (ConstructLVal v lv lt) = 
  text "= LVal" <+> PP.parens ( ppId v  <+> text "," <+> 
                                ppId lv <+> text "," <+> 
                                ppId lt)
ppComplexExpr (ComplexRaw raw) = ppRawExpr raw
    
qqFields fields =
  PP.hsep $ PP.punctuate (text ",") (map ppField fields)
    where 
      ppField (name, v) = 
        PP.hcat [PP.text name, PP.text "=", ppId v]

ppIR :: RawInst -> PP.Doc
ppIR SetBranchFlag = text "<setbranchflag>"
ppIR (AssignRaw vn st) = ppId vn <+> text "=" <+> ppRawExpr st
ppIR (AssignLVal vn expr) = 
  ppId vn <+> text "=" <+> ppComplexExpr expr
-- ppIR (ConstructLVal x v lv lt) = 
--   ppId x <+> text 
ppIR (AssertType x t) = text "assert" <+> ppId x <+> text "has type" <+> text (show t)
ppIR (AssertEqTypes Nothing x y) = text "assertEqTypes" <+> ppId x <+> ppId y
ppIR (AssertEqTypes (Just (List2OrMore a1 a2 as)) x y) = text "assertEqTypes" <+> (PP.hsep (map (text.show) (a1:a2:as))) <+> ppId x <+> ppId y
ppIR (SetState comp v) = 
  ppId comp <+> text "<-" <+> ppId v

ppIR (MkFunClosures varmap fdefs) = 
    let vs = hsepc $ ppEnvIds varmap
        ppFdefs = map (\((VN x), HFN y) ->  text x <+> text "= mkClos" <+> text y ) fdefs 
     in text "with env:=" <+> PP.brackets vs $$ nest 2 (vcat ppFdefs)
    where ppEnvIds ls =
            map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
          hsepc ls = PP.hsep (PP.punctuate (text ",") ls)

    
-- ppIR (LevelOperations _ insts) = 
--  text "level operation" $$ nest 2 (vcat (map ppIR insts))

ppTr (Call bb1 bb2) = (text "call" $$ nest 4 (ppBB bb1)) $$ (ppBB bb2)


-- ppTr (AssertElseError va ir va2 _) 
--   = text "assert" <+> PP.parens (ppId va) <+>
--     text "{" $$
--     nest 2 (ppBB ir) $$
--     text "}" $$
--     text "elseError" <+> (ppId va2)


ppTr (If va ir1 ir2)
  = text "if" <+> PP.parens (ppId va) <+>
    text "{" $$
    nest 4 (ppBB ir1) $$
    text "}" $$
    text "else {" $$
    nest 4 (ppBB ir2) $$
    text "}"
ppTr (TailCall va1 ) = ppFunCall (text "tail") [ppId va1]
ppTr (Ret)  = text "ret"
ppTr (LibExport va) = ppFunCall (text "export") [ppId va]
ppTr (Error va _)  = (text "error") <> (ppId va)


ppBB (BB insts tr) = vcat $ (map ppIR insts) ++ [ppTr tr]

ppConsts consts = 
  vcat $ map ppConst consts 
    where ppConst (x, lit) = hsep [ ppId x , text "=", CPS.ppLit lit ]



textOfBinOp Basics.LatticeJoin  = "rt.join"
textOfBinOp Basics.LatticeMeet  = "<meet>"
textOfBinOp Basics.Plus = "+"
textOfBinOp Basics.Minus= "-"
textOfBinOp Basics.Mult = "*"
textOfBinOp Basics.Div= "/"
textOfBinOp Basics.Mod = "%"
textOfBinOp Basics.Le= "<="
textOfBinOp Basics.Lt= "<"
textOfBinOp Basics.Ge= ">="
textOfBinOp Basics.Gt= ">"
textOfBinOp Basics.And= "&&"
textOfBinOp Basics.Or= "||"
textOfBinOp Basics.IntDiv= "rt.intdiv"
textOfBinOp Basics.BinAnd = "&"
textOfBinOp Basics.BinOr = "|"
textOfBinOp Basics.BinXor = "^"
textOfBinOp Basics.BinShiftLeft = "<<"
textOfBinOp Basics.BinShiftRight = ">>"
textOfBinOp Basics.BinZeroShiftRight = ">>>"




textOfBinOp Basics.Index = "rt.raw_index"
textOfBinOp x = IR.textOfBinOp x


textOfUnOp Basics.IsTuple = "rt.raw_istuple"
textOfUnOp Basics.Length = "rt.raw_length"
textOfUnOp x = IR.textOfUnOp x