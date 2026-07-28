-- | The @troupe-ir-sexp@ document layer: the versioned wrapper around an IR
-- program, and the printer/parser entry points.
--
-- This module implements the format specified in
-- @compiler/docs/spec-troupe-ir-sexp.md@:
--
--   * 'printProg' : 'IRProgram' -> 'String' — the canonical printer (R2).
--   * 'parseProg' : 'String' -> 'Either' 'String' 'IRProgram' — the parser
--     (tolerant lexically, strict on the versioned @(troupe-ir-sexp 1 ...)@
--     wrapper).
--   * 'erasePosProg' — normalizes all source positions to 'NoPos' so the
--     round-trip law R1 can be stated over position-erased ASTs.
--
-- The encoding of the IR itself is not here: it is the 'ToSexp' / 'FromSexp'
-- instances, each defined in the module that defines its type, over the datum
-- layer in "Sexp".
module IRSexp
  ( printProg
  , parseProg
  , erasePosProg
  , Datum(..)
  ) where

import           IR
import           Basics (FieldName)
import           Sexp
import           TroupePositionInfo (Located(..), noLoc)

------------------------------------------------------------
-- The current format version.
------------------------------------------------------------

formatVersion :: Integer
formatVersion = 1

------------------------------------------------------------
-- Printing.
------------------------------------------------------------

-- | Print a whole program in the canonical wrapped s-expression form.
printProg :: IRProgram -> String
printProg p = renderDatum (encodeDocument p) ++ "\n"

encodeDocument :: IRProgram -> Datum
encodeDocument p =
  Lst [ Atom "troupe-ir-sexp"
      , toSexp formatVersion
      , toSexp p
      ]

------------------------------------------------------------
-- Parsing.
------------------------------------------------------------

-- | Parse a whole program from its canonical (or tolerant) s-expression text.
parseProg :: String -> Either String IRProgram
parseProg input = readDatum input >>= decodeDocument

decodeDocument :: Datum -> Either String IRProgram
decodeDocument (Lst [Atom "troupe-ir-sexp", verD, progD]) = do
  ver <- fromSexp verD
  if ver == formatVersion
    then fromSexp progD
    else Left ("unsupported troupe-ir-sexp version: " ++ show ver
               ++ " (this build supports version " ++ show formatVersion ++ ")")
decodeDocument (Lst (Atom "troupe-ir-sexp" : _)) =
  Left "malformed troupe-ir-sexp wrapper: expected (troupe-ir-sexp VERSION PROGRAM)"
decodeDocument d =
  Left ("not a troupe-ir-sexp document: expected head symbol \"troupe-ir-sexp\", got "
        ++ headHint d)

------------------------------------------------------------
-- Position erasure (for stating R1 over position-erased ASTs).
------------------------------------------------------------

-- | Normalize every source position in a program to 'NoPos'. The parser
-- always fills 'NoPos'; erasing the printer's input lets R1 be checked with
-- the derived structural equality.
erasePosProg :: IRProgram -> IRProgram
erasePosProg (IRProgram funs) =
  IRProgram (map (\(Loc _ f) -> noLoc (erasePosFun f)) funs)

erasePosFun :: FunDef -> FunDef
erasePosFun (FunDef hfn (Loc _ vn) consts bb) =
  FunDef hfn (noLoc vn) consts (erasePosBB bb)

erasePosBB :: IRBBTree -> IRBBTree
erasePosBB (BB insts term) =
  BB (map (\(Loc _ i) -> noLoc (erasePosInst i)) insts)
     (case term of Loc _ t -> noLoc (erasePosTerm t))

erasePosInst :: IRInst -> IRInst
erasePosInst (Assign v e) = Assign v (erasePosExpr e)
erasePosInst (MkFunClosures caps clos) =
  MkFunClosures (map (\(v, lva) -> (v, eLVA lva)) caps) clos

erasePosExpr :: IRExpr -> IRExpr
erasePosExpr (Bin op a b)         = Bin op (eLVA a) (eLVA b)
erasePosExpr (Un op a)            = Un op (eLVA a)
erasePosExpr (Tuple xs tag)       = Tuple (map eLVA xs) tag
erasePosExpr (Record fs)          = Record (map eField fs)
erasePosExpr (WithRecord lva fs)  = WithRecord (eLVA lva) (map eField fs)
erasePosExpr (ProjField lva f)    = ProjField (eLVA lva) f
erasePosExpr (ProjIdx lva w)      = ProjIdx (eLVA lva) w
erasePosExpr (List xs)            = List (map eLVA xs)
erasePosExpr (ListCons a b)       = ListCons (eLVA a) (eLVA b)
erasePosExpr e@(Const _)          = e
erasePosExpr e@(Base _)           = e
erasePosExpr e@(Lib _ _)          = e

eField :: (FieldName, LVarAccess) -> (FieldName, LVarAccess)
eField (f, lva) = (f, eLVA lva)

erasePosTerm :: IRTerminator -> IRTerminator
erasePosTerm (TailCall f a)            = TailCall (eLVA f) (eLVA a)
erasePosTerm (Ret a)                   = Ret (eLVA a)
erasePosTerm (If c t e)                = If (eLVA c) (erasePosBB t) (erasePosBB e)
erasePosTerm (AssertElseError c bb er) = AssertElseError (eLVA c) (erasePosBB bb) (eLVA er)
erasePosTerm (LibExport a)             = LibExport (eLVA a)
erasePosTerm (Error a)                 = Error (eLVA a)
erasePosTerm (StackExpand v b1 b2)     = StackExpand v (erasePosBB b1) (erasePosBB b2)

eLVA :: LVarAccess -> LVarAccess
eLVA (Loc _ va) = noLoc va
