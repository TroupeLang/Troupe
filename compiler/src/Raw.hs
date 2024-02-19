{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Raw where 

import qualified Basics
import           RetCPS (VarName (..))
import           IR ( Identifier(..)
                    , VarAccess(..), HFN (..), Fields (..), Ident
                    , ppId,ppFunCall,ppArgs
                    )
import qualified IR (FunDef (..))


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


-- | Variable names used for plain (unlabelled) values.
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



data RawType
  = RawNumber| RawUnit | RawBoolean | RawString | RawFunction
  | RawLocalObj| RawHandler| RawList | RawTuple| RawRecord
  | RawNode| RawProcessId| RawCapability| RawLevel
  | RawAuthority | RawTopAuthority| RawEnv
    deriving (Eq, Show)


-- | A runtime assertion stopping the current thread if the condition is not satisfied.
data RTAssertion
  = AssertType RawVar RawType
  -- | Assert that the types of the given 'RawVar's are equal and (if provided) included in the given list of types. 
  -- (Probably better design: possibly empty list of types (where empty means any types allowed))
  -- | AssertEqTypes (Maybe (List2OrMore RawType)) RawVar RawVar
  | AssertTypesBothStringsOrBothNumbers RawVar RawVar
  | AssertTupleLengthGreaterThan RawVar Word
  | AssertRecordHasField RawVar Basics.FieldName
   deriving (Eq, Show)

-- data List2OrMore a = List2OrMore a a [a] deriving (Eq, Show)

-- | Note about categorization of Raw expressions: There are two main types of expressions:
-- those computing a single raw value, and those computing a labelled value (see the return type
-- of the corresponding runtime operation). Operations also differ in whether they take simple or
-- labelled values as parameters.
-- We could categorize RawExpr into different datatypes, but that would also mean to
-- split up Basics.UnaryOp and Basics.BinOp, and the overall benefit is unclear. They still
-- have to be treated separately in IR2Raw, which works on the structure provided by IR, and
-- it is there where instructions to handle the result are generated (AssignRaw and AssignLVal).
-- What would be possible is to introduce a pre-processing which translates IR expressions into
-- categorized expressions, which could then slightly simplify handling at IR2Raw.
data RawExpr
  = Bin Basics.BinOp RawVar RawVar
  | Un Basics.UnaryOp RawVar
  | ProjectLVal VarAccess LValField
  | ProjectState MonComponent
  | Tuple [VarAccess]
  | Record Fields
  | WithRecord RawVar Fields 
  | ProjField RawVar Basics.FieldName
  | ProjIdx RawVar Word
  | List [VarAccess]
  -- | Cons operation with the new head (labelled value) and the list (simple value).
  | ListCons VarAccess RawVar
  | Const C.Lit
  -- | Reference to a definition in a library
  | Lib Basics.LibName Basics.VarName
  | Base Basics.VarName
  -- | Make a labelled value out of the given 'RawVar's (value, value label, type label).
  | ConstructLVal RawVar RawVar RawVar
  deriving (Eq, Show)


data RawInst
  -- | Assign the result of the given simple expression (an unlabelled value) to the given raw variable.
  -- There is no type-level distinction of 'RawExpr' which produce a labelled value and those producing
  -- an unlabelled value, because this is more convenient for how these are generated in IR2Raw.
  = AssignRaw RawVar RawExpr
  -- | Assign the result of the given complex expression (a labelled value) to a variable with the given name.
  | AssignLVal VarName RawExpr
  -- | Set a monitor component. Provided variable must contain a label (this is not checked).
  | SetState MonComponent RawVar
  -- | Indicates that the current block invoked a branch instruction.
  -- Is inserted before an "if".
  -- See stack/execution model.
  | SetBranchFlag
  -- | The sparse bit is tracking whether data in the current closure is bounded by PC.
  -- If this condition is invalidated by introducing new labels (like with the raisedTo instruction),
  -- this instruction must be added to ensure that the required join operations happen.
  | InvalidateSparseBit
  | MkFunClosures [(VarName, VarAccess)] [(VarName, HFN)]
  | RTAssertion RTAssertion
   deriving (Eq, Show)

-- | A block of instructions followed by a terminator, which can contain further 'RawBBTree's.
data RawBBTree = BB [RawInst] RawTerminator deriving (Eq, Show)

data RawTerminator
  = TailCall RawVar
  | Ret
  | If RawVar RawBBTree RawBBTree
  | LibExport VarAccess
  | Error RawVar PosInf
  -- | Execute the first BB and then execute the second BB where
  -- PC is reset to the level before entering the first BB.
  | Call RawBBTree RawBBTree
  deriving (Eq, Show)

ppRTAssertionCode f a = f (text $ "rt.rawAssert" ++ rtFun) args
  where (rtFun, args) = case a of
          AssertType x t -> (case t of
            RawNumber -> "IsNumber"
            RawBoolean -> "IsBoolean"
            RawString -> "IsString"
            RawFunction -> "IsFunction"
            RawList -> "IsList"
            RawTuple -> "IsTuple"
            RawRecord -> "IsRecord"
            RawLevel -> "IsLevel"
            _ -> error $ "type assertion not implemented for " ++ show t
            , [ppId x])
          AssertTypesBothStringsOrBothNumbers x y -> ("PairsAreStringsOrNumbers", [ppId x, ppId y])
          AssertTupleLengthGreaterThan x n -> ("TupleLengthGreaterThan", [ppId x, text (show n)])
          AssertRecordHasField x f -> ("RecordHasField", [ppId x, PP.quotes $ text f])



ppRTAssertion :: RTAssertion -> PP.Doc
ppRTAssertion = ppRTAssertionCode ppFunCall

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

-- | Used to determine in how far instructions can be reordered.
data InstructionType 
  = RegularInstruction RegularInstructionKind
  | LabelSpecificInstruction
    deriving (Eq, Ord, Show)

instructionType :: RawInst -> InstructionType
instructionType i = case i of 
  AssignRaw _ (Bin Basics.LatticeJoin _ _) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonPC) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonBlock) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_Lev)  -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_TLev) -> LabelSpecificInstruction
  AssignLVal _ (ConstructLVal _ _ _) -> RegularInstruction RegConstructor
  AssignRaw _ (ProjectLVal _ _) -> RegularInstruction RegDestructor
  SetBranchFlag -> RegularInstruction RegConstructor
  InvalidateSparseBit -> RegularInstruction RegOther
  SetState s _ -> 
    case s of 
      R0_Val -> RegularInstruction RegConstructor 
      R0_Lev -> RegularInstruction RegConstructor
      R0_TLev -> RegularInstruction RegConstructor
      MonPC -> LabelSpecificInstruction
      MonBlock -> LabelSpecificInstruction
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
ppRawExpr (Bin binop va1 va2) =
  ppId va1 <+> text (show binop) <+> ppId va2
ppRawExpr (Un op v) =
  text (show op) <> PP.parens (ppId v)
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
ppRawExpr (ProjField x f) =
  PP.text "ProjField" PP.<+> (ppId x) PP.<+> PP.text f
ppRawExpr (ProjIdx x idx) =
  PP.text "ProjIdx" PP.<+> (ppId x) PP.<+> PP.text (show idx)
ppRawExpr (ProjectLVal v f) = 
  (ppId v) PP.<> text "." PP.<>  PP.text (show f)
ppRawExpr (ProjectState cmp) = ppId cmp


ppRawExpr (Base v) = text v
ppRawExpr (ConstructLVal v lv lt) =
  text "LVal" <+> PP.parens ( ppId v  <+> text "," <+>
                                ppId lv <+> text "," <+>
                                ppId lt)
    
qqFields fields =
  PP.hsep $ PP.punctuate (text ",") (map ppField fields)
    where 
      ppField (name, v) = 
        PP.hcat [PP.text name, PP.text "=", ppId v]

ppIR :: RawInst -> PP.Doc
ppIR SetBranchFlag = text "<setbranchflag>"
ppIR (AssignRaw vn st) = ppId vn <+> text "=" <+> ppRawExpr st
ppIR (AssignLVal vn expr) = 
  ppId vn <+> text "=" <+> ppRawExpr expr
-- ppIR (ConstructLVal x v lv lt) = 
--   ppId x <+> text 
ppIR (RTAssertion a) = ppRTAssertion a
ppIR (SetState comp v) = 
  ppId comp <+> text "<-" <+> ppId v
ppIR InvalidateSparseBit = text "<invalidate sparse bit>"

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
ppTr (Error va _)  = (text "error ") <> (ppId va)


ppBB (BB insts tr) = vcat $ (map ppIR insts) ++ [ppTr tr]

ppConsts consts = 
  vcat $ map ppConst consts 
    where ppConst (x, lit) = hsep [ ppId x , text "=", CPS.ppLit lit ]


