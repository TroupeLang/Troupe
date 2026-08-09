{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Raw where 

import qualified Basics
import           RetCPS (VarName (..))
import           IR ( Identifier(..)
                    , HFN (..), Ident
                    , LVarAccess
                    , ppId,ppFunCall
                    )
import qualified IR (FunDef (..))


import qualified Core                      as C
import           Core (ppLit)
import Data.Map.Lazy (Map)

import           Text.PrettyPrint.HughesPJ (hsep, nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo (Located(..))
import           PrettyPrint (PP, runPP, runPPDefault, ppLocated, vcatMapPP, ShowDebug(..))


-- | Variable names used for plain (unlabelled) values.
newtype RawVar = RawVar Ident deriving (Eq, Show, Ord)
instance Identifier RawVar where ppId (RawVar x) = text x

-- Located type aliases
type LRawInst = Located RawInst
type LRawTerminator = Located RawTerminator
type LFunDef = Located FunDef

-- | Fields with Located VarAccess - preserves source positions for field values
type LFields = [(Basics.FieldName, LVarAccess)]

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
  | RawDCLabel
  | RawNode| RawProcessId| RawCapability| RawLevel
  | RawAuthority | RawTopAuthority| RawEnv
    deriving (Eq, Show)


isSubtypeOf :: RawType -> RawType -> Bool
RawDCLabel `isSubtypeOf` RawLevel = True  -- both are AbstractLevel at run time
sub        `isSubtypeOf` super    = sub == super


-- | A runtime assertion stopping the current thread if the condition is not satisfied.
data RTAssertion
  = AssertType RawVar RawType
  -- | Assert that the types of the given 'RawVar's are equal and (if provided) included in the given list of types. 
  -- (Probably better design: possibly empty list of types (where empty means any types allowed))
  -- | AssertEqTypes (Maybe (List2OrMore RawType)) RawVar RawVar
  | AssertTypesBothStringsOrBothNumbers RawVar RawVar
  | AssertTupleLengthGreaterThan RawVar Word
  | AssertRecordHasField RawVar Basics.FieldName
  | AssertNotZero RawVar
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
--
-- NOTE: RawExpr now uses LVarAccess (Located VarAccess) to preserve source positions
-- for variable references that come from labelled values.
data RawExpr
  = Bin Basics.BinOp UseNativeBinop RawVar RawVar
  | Un Basics.UnaryOp RawVar
  | ProjectLVal LVarAccess LValField
  | ProjectState MonComponent
  | Tuple [LVarAccess] Basics.SynVariantTag
  | Record LFields
  | WithRecord RawVar LFields
  | ProjField RawVar Basics.FieldName
  | ProjIdx RawVar Word
  | List [LVarAccess]
  -- | Cons operation with the new head (labelled value) and the list (simple value).
  | ListCons LVarAccess RawVar
  | Const C.Lit
  -- | Reference to a definition in a library
  | Lib Basics.LibName Basics.VarName
  | Base Basics.VarName
  -- | Make a labelled value out of the given 'RawVar's (value, value label, type label).
  | ConstructLVal RawVar RawVar RawVar
  deriving (Eq, Show)

-- | For equality and inequality, we generally defer to the runtime. However 
-- when we know that the operation involves simple types we can generate 
-- faster code, avoiding calling the runtime functions
-- 
newtype UseNativeBinop = UseNativeBinop Bool
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
  -- | Set the branch flag of the current frame when the label in the given
  -- variable (the function-value label of a call) does not flow to the current
  -- PC, i.e. exactly when the call's PC raise is an actual raise. A call on a
  -- secret-labelled closure is a control transfer selected by data above the
  -- context — a branch for the branch-balance discipline — so an unbalanced
  -- change of the mailbox clearance inside such a call must be caught by the
  -- same balance check at the return. Label-silent calls (in particular every
  -- public call) do not flag. Emitted only for tail calls, before the
  -- corresponding PC raise; the flag decision is taken at runtime.
  | SetBranchFlagOnCallRaise RawVar
  -- | The sparse bit is tracking whether data in the current closure is bounded by PC.
  -- If this condition is invalidated by introducing new labels (like with the raisedTo instruction),
  -- this instruction must be added to ensure that the required join operations happen.
  | InvalidateSparseBit
  -- | Create function closures. Uses LVarAccess to preserve source positions for environment bindings.
  | MkFunClosures [(VarName, LVarAccess)] [(VarName, HFN)]
  | RTAssertion RTAssertion
  -- | Source position annotation for source map generation.
  -- This instruction generates no code but carries position info that was preserved
  -- when an instruction was eliminated during optimization (e.g., copy propagation).
  -- The RawVar is the variable that the eliminated instruction was assigning to,
  -- for debugging purposes.
  | SourcePosAnnotation RawVar
   deriving (Eq, Show)

-- | A block of instructions followed by a terminator, which can contain further 'RawBBTree's.
data RawBBTree = BB [LRawInst] LRawTerminator deriving (Eq, Show)

data RawTerminator
  = TailCall RawVar
  | Ret
  | If RawVar RawBBTree RawBBTree
  -- | Uses LVarAccess to preserve source position for the exported value
  | LibExport LVarAccess
  | Error RawVar
  -- | Execute the first BB and then execute the second BB where
  -- PC is reset to the level before entering the first BB.
  | StackExpand RawBBTree RawBBTree
  deriving (Eq, Show)


-- TODO: 2025-09-19; AA -- this is a bit too hacky 
                        -- we should not be referencing runtime functions 
                        -- by concatenating their names 
-- | Emit an assertion call. @extraArgs@ are appended after the assertion's own
-- arguments; the code generator uses this to pass the operation's source
-- position, which the runtime records for error reporting on a failed
-- assertion (see 'ppRTAssertion' for the position-free debug rendering).
ppRTAssertionCode f extraArgs a = f (text $ "rt.rawAssert" ++ rtFun) (args ++ extraArgs)
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
          AssertRecordHasField x f -> ("RecordHasField", [ppId x, PP.doubleQuotes $ text f])
          AssertNotZero x -> ("NotZero", [ppId x])



ppRTAssertion :: RTAssertion -> PP.Doc
ppRTAssertion = ppRTAssertionCode ppFunCall []

type Consts = [(RawVar, C.Lit )]

-- Function definition (position is on the Located wrapper when used as LFunDef)
data FunDef = FunDef
                    HFN          -- name of the function
                    Consts
                    RawBBTree    -- body
                    IR.FunDef    -- original definition for serialization
                deriving (Eq)

-- An IR program is just a collection of function definitions
data RawProgram = RawProgram [LFunDef]


-----------------------------------------------------------
-- Serialization
-----------------------------------------------------------
data RawUnit
  = FunRawUnit LFunDef
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
  AssignRaw _ (Bin Basics.LatticeJoin _ _ _) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonPC) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState MonBlock) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_Lev) -> LabelSpecificInstruction
  AssignRaw _ (ProjectState R0_TLev) -> LabelSpecificInstruction
  AssignLVal _ (ConstructLVal _ _ _) -> RegularInstruction RegConstructor
  AssignRaw _ (ProjectLVal _ _) -> RegularInstruction RegDestructor
  SetBranchFlag -> RegularInstruction RegConstructor
  -- RegOther, not RegConstructor: instOrder must never move the PC-setting
  -- label instruction of the call's raise ahead of this check (the check reads
  -- the pre-raise PC), and no reordering rule crosses a RegOther from the left.
  SetBranchFlagOnCallRaise _ -> RegularInstruction RegOther
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

ppProg :: RawProgram -> PP PP.Doc
ppProg (RawProgram funs) =
  vcatMapPP ppLFunDef funs

instance Show RawProgram where
  show = PP.render . runPPDefault . ppProg

instance ShowDebug RawProgram where
  showDebugWith cfg = PP.render . runPP cfg . ppProg

ppFunDef :: FunDef -> PP PP.Doc
ppFunDef ( FunDef hfn consts insts _ ) = do
  bbDoc <- ppBB insts
  pure $ vcat [ text "func" <+> ppFunCall (ppId hfn) [] <+> text "{"
              , nest 2 (ppConsts consts )
              , nest 2 bbDoc
              , text "}"]

ppLFunDef :: LFunDef -> PP PP.Doc
ppLFunDef = ppLocated ppFunDef



ppRawExpr :: RawExpr -> PP.Doc
ppRawExpr (Bin binop _ va1 va2) = -- TODO: 2025-07-31; also print the fast flag 
  ppId va1 <+> text (show binop) <+> ppId va2
ppRawExpr (Un op v) =
  text (show op) <> PP.parens (ppId v)
ppRawExpr (Tuple vars tag) =
  -- Mark a syntactic-variant tuple distinctly (matching the ir-sexp
  -- "tuple-variant" naming); a plain tuple keeps the bare @(a, b)@ form.
  (if tag then text "tuple-variant" else PP.empty)
    <> (PP.parens $ PP.hsep $ PP.punctuate (text ",") (map ppId vars))
ppRawExpr (List vars) =
  PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppId vars)
ppRawExpr (ListCons v1 v2) =
  text "cons" <> (PP.parens $ ppId v1 <> text "," <> ppId v2)
ppRawExpr (Const C.LUnit) = text "__unit"
ppRawExpr (Const lit) = ppLit lit
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
    
-- | Pretty print LFields (fields with Located VarAccess)
qqFields :: LFields -> PP.Doc
qqFields fields =
  PP.hsep $ PP.punctuate (text ",") (map ppField fields)
    where
      ppField (name, lv) =
        PP.hcat [PP.text name, PP.text "=", ppId lv]

ppIR :: RawInst -> PP PP.Doc
ppIR SetBranchFlag = pure $ text "<setbranchflag>"
ppIR (SetBranchFlagOnCallRaise r) = pure $ text "<setbranchflag-on-call-raise>" <+> ppId r
ppIR (AssignRaw vn st) = pure $ ppId vn <+> text "=(raw)" <+> ppRawExpr st
ppIR (AssignLVal vn expr) =
  pure $ ppId vn <+> text "=(lval)" <+> ppRawExpr expr
-- ppIR (ConstructLVal x v lv lt) =
--   ppId x <+> text
ppIR (RTAssertion a) = pure $ ppRTAssertion a
ppIR (SetState comp v) =
  pure $ ppId comp <+> text "<-" <+> ppId v
ppIR InvalidateSparseBit = pure $ text "<invalidate sparse bit>"
ppIR (SourcePosAnnotation r) = pure $ text "<source-pos>" <+> ppId r

ppIR (MkFunClosures varmap fdefs) =
    let vs = hsepc $ ppEnvIds varmap
        ppFdefs = map (\((VN x), HFN y) ->  text x <+> text "= mkClos" <+> text y ) fdefs
     in pure $ text "with env:=" <+> PP.brackets vs $$ nest 2 (vcat ppFdefs)
    where ppEnvIds ls =
            map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
          hsepc ls = PP.hsep (PP.punctuate (text ",") ls)

-- | Pretty print a Located RawInst
ppLRawInst :: LRawInst -> PP PP.Doc
ppLRawInst = ppLocated ppIR


-- ppIR (LevelOperations _ insts) =
--  text "level operation" $$ nest 2 (vcat (map ppIR insts))

ppTr :: RawTerminator -> PP PP.Doc
ppTr (StackExpand bb1 bb2) = do
  bb1Doc <- ppBB bb1
  bb2Doc <- ppBB bb2
  pure $ (text "call" $$ nest 4 bb1Doc) $$ bb2Doc


-- ppTr (AssertElseError va ir va2 _)
--   = text "assert" <+> PP.parens (ppId va) <+>
--     text "{" $$
--     nest 2 (ppBB ir) $$
--     text "}" $$
--     text "elseError" <+> (ppId va2)


ppTr (If va ir1 ir2) = do
  ir1Doc <- ppBB ir1
  ir2Doc <- ppBB ir2
  pure $ text "if" <+> PP.parens (ppId va) <+>
    text "{" $$
    nest 4 ir1Doc $$
    text "}" $$
    text "else {" $$
    nest 4 ir2Doc $$
    text "}"
ppTr (TailCall va1) = pure $ ppFunCall (text "tail") [ppId va1]
ppTr Ret  = pure $ text "ret"
ppTr (LibExport va) = pure $ ppFunCall (text "export") [ppId va]
ppTr (Error va)  = pure $ (text "error ") PP.<> (ppId va)

-- | Pretty print a Located RawTerminator
ppLRawTr :: LRawTerminator -> PP PP.Doc
ppLRawTr = ppLocated ppTr

ppBB :: RawBBTree -> PP PP.Doc
ppBB (BB insts tr) = do
  instDocs <- mapM ppLRawInst insts
  trDoc <- ppLRawTr tr
  pure $ vcat $ instDocs ++ [trDoc]

ppConsts consts = 
  vcat $ map ppConst consts 
    where ppConst (x, lit) = hsep [ ppId x , text "=", ppLit lit ]


