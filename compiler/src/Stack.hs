{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Stack 
where


import           RetCPS (VarName (..))
import           IR ( Identifier(..)
                    , HFN (..)
                    , LVarAccess
                    , ppId,ppFunCall
                    )
import qualified IR (FunDef (..))
import Raw (RawExpr (..), RawVar (..), MonComponent(..),
            ppRawExpr, Assignable (..), Consts, ppConsts, RTAssertion(..), ppRTAssertion)

import qualified Core                      as C

import           Text.PrettyPrint.HughesPJ (nest, text, vcat, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ as PP
import           TroupePositionInfo (Located(..))
import           PrettyPrint (PP, runPP, runPPDefault, ppLocated, vcatMapPP, ShowDebug(..))

-- Located type aliases
type LStackInst = Located StackInst
type LStackTerminator = Located StackTerminator
type LFunDef = Located FunDef


data StackBBTree = BB [LStackInst] LStackTerminator deriving (Eq, Show)



data StackTerminator
  = TailCall RawVar
  | Ret
  | If RawVar StackBBTree StackBBTree
  -- | Uses LVarAccess to preserve source position for the exported value
  | LibExport LVarAccess
  | Error RawVar
  | StackExpand StackBBTree StackBBTree
  deriving (Eq, Show)



type StackPos = Int
data EscapesBlock = NotEscaping
            | Escaping StackPos
            deriving (Eq, Show)


data RawAssignType = AssignConst | AssignLet | AssignMut deriving (Eq, Ord, Show)


data StackInst
  = AssignRaw RawAssignType RawVar RawExpr
  | LabelGroup [LStackInst]  -- Note: LabelGroup contains Located instructions
  | AssignLVal VarName RawExpr
  | FetchStack Assignable StackPos
  | StoreStack Assignable StackPos
  | SetState MonComponent RawVar
  | SetBranchFlag
  | InvalidateSparseBit
  -- | Create function closures. Uses LVarAccess to preserve source positions for environment bindings.
  | MkFunClosures [(VarName, LVarAccess)] [(VarName, HFN)]
  | RTAssertion RTAssertion
  -- | Source position annotation for source map generation.
  -- Generates no code but emits a source map marker at the current position.
  | SourcePosAnnotation RawVar
   deriving (Eq, Show)

-- Function definition (position is on the Located wrapper when used as LFunDef)
data FunDef = FunDef
                    HFN            -- name of the function
                    Int            -- frame size
                    Raw.Consts     -- constant literals
                    StackBBTree    -- body
                    IR.FunDef      -- original definition for serialization
                deriving (Eq)

-- An IR program is just a collection of function definitions
data StackProgram = StackProgram [LFunDef]

data StackUnit
  = FunStackUnit LFunDef
  | ProgramStackUnit StackProgram

-----------------------------------------------------------
-- PRETTY PRINTING
-----------------------------------------------------------

ppProg :: StackProgram -> PP PP.Doc
ppProg (StackProgram funs) =
  vcatMapPP ppLFunDef funs

instance Show StackProgram where
  show = PP.render . runPPDefault . ppProg

instance ShowDebug StackProgram where
  showDebugWith cfg = PP.render . runPP cfg . ppProg

ppFunDef :: FunDef -> PP PP.Doc
ppFunDef ( FunDef hfn _ consts insts _ )
  = do bbDoc <- ppBB insts
       pure $ vcat [ text "func" <+> ppFunCall (ppId hfn) [] <+> text "{"
                   , nest 2 (ppConsts consts)
                   , nest 2 bbDoc
                   , text "}"]

ppLFunDef :: LFunDef -> PP PP.Doc
ppLFunDef = ppLocated ppFunDef


qqFields fields =
  PP.hsep $ PP.punctuate (text ",") (map ppField fields)
    where
      ppField (name, v) =
        PP.hcat [PP.text name, PP.text "=", ppId v]

ppEsc esc =
  case esc of
    NotEscaping -> PP.empty
    Escaping x -> PP.text "*" <+> PP.text (show x )


ppIR :: StackInst -> PP PP.Doc
ppIR SetBranchFlag = pure $ text "<setbranchflag>"
ppIR InvalidateSparseBit = pure $ text "<invalidate sparse bit>"
ppIR (AssignRaw _ vn st) = pure $ ppId vn <+> text "=" <+> ppRawExpr st
ppIR (AssignLVal vn expr) =
  pure $ ppId vn <+> text "=" <+> ppRawExpr expr
ppIR (RTAssertion a) = pure $ ppRTAssertion a

ppIR (SetState comp v) =
  pure $ ppId comp <+> text "<-" <+> ppId v
ppIR (FetchStack x i) =
  pure $ ppId x <+> text "<- $STACK[" PP.<> text (show i) PP.<> text "]"
ppIR (StoreStack x i) =
  pure $ text "$STACK[" PP.<> text (show i) PP.<> text "] = " <+> ppId x


ppIR (MkFunClosures varmap fdefs) =
    let vs = hsepc $ ppEnvIds varmap
        ppFdefs = map (\((VN x), HFN y) ->  text x <+> text "= mkClos" <+> text y ) fdefs
     in pure $ text "with env:=" <+> PP.brackets vs $$ nest 2 (vcat ppFdefs)
    where ppEnvIds ls =
            map (\(a,b) -> (ppId a) PP.<+> text "->" <+> ppId b ) ls
          hsepc ls = PP.hsep (PP.punctuate (text ",") ls)


ppIR (LabelGroup insts) = do
  instDocs <- mapM ppLStackInst insts
  pure $ text "group" $$ nest 2 (vcat instDocs)
ppIR (SourcePosAnnotation r) = pure $ text "<source-pos>" <+> ppId r

ppLStackInst :: LStackInst -> PP PP.Doc
ppLStackInst = ppLocated ppIR

ppTr :: StackTerminator -> PP PP.Doc
ppTr (StackExpand bb1 bb2) = do
  bb1Doc <- ppBB bb1
  bb2Doc <- ppBB bb2
  pure $ (text "= call" $$ nest 2 bb1Doc) $$ bb2Doc


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
    nest 2 ir1Doc $$
    text "}" $$
    text "else {" $$
    nest 2 ir2Doc $$
    text "}"
ppTr (TailCall va1) = pure $ ppFunCall (text "tail") [ppId va1]
ppTr Ret  = pure $ ppFunCall (text "ret") []
ppTr (LibExport va) = pure $ ppFunCall (text "export") [ppId va]
ppTr (Error va)  = pure $ (text "error") PP.<> (ppId va)

ppLStackTr :: LStackTerminator -> PP PP.Doc
ppLStackTr = ppLocated ppTr

ppBB :: StackBBTree -> PP PP.Doc
ppBB (BB insts ltr) = do
  instDocs <- mapM ppLStackInst insts
  trDoc <- ppLStackTr ltr
  pure $ vcat $ instDocs ++ [trDoc]
