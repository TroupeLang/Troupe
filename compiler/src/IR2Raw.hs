{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-|

IR to Raw translation.

The main part of the translation consists of translating IR expressions to Raw instructions.
For this translation, we define an abstraction 'RawComp', which describes the computation of
a result value together with the two labels. From a 'RawComp' we generate Raw instructions
representing this computation.

In 'expr2RawComp', we define the 'RawComp' for each expression, together with some additional
instructions to be executed (e.g. for assertions related to an expression). To generate
these additional instructions, we define several helper functions representing abstractions
for common patterns.

All translations happen in the translation monad TM, which collects the generated instructions
and keeps track of a counter for fresh variable names.

The translation relies on the RawOpt optimization phase for efficiency, as due
to the abstraction, there is some redundancy in the generated instructions (like unused projections from labelled values).

The entry points are 'ir2raw' and 'prog2raw' which translate an IR program to a Raw program.
There are translation functions for the different components of a program, called in this order:
'prog2raw' -> 'fun2raw' -> 'tree2raw' -> 'inst2raw'/'tr2raw' -> 'expr2raw'

NOTE: FW 2023-08 This compiler phase received a major revision, introducing the 'RawComp'
abstraction as described above. In addition, the revision includes:
- Several monitor bug fixes
- Moving of logic from the runtime to instructions generated in this phase
- Some changed and new IR instructions
"Revision 2023-08" marks places where semantics of the generated Raw code has changed
(and other notable changes). We compare against commit b3bd971, a state just before the revision.

-}
module IR2Raw (ir2raw, prog2raw) where

import qualified Basics
import qualified IR
import IR (VarAccess(..))
import Raw
import RetCPS(VarName(..))
import Control.Monad
import Control.Monad.Trans.RWS(RWS, evalRWS, ask, get, put, tell, censor, listen, local)
import TroupePositionInfo (Located(..), getLoc, unLoc, noLoc, PosInf(..))

-- ===== Monad definition =====

-- | Translation monad
-- Reader: current source position for generated instructions
-- Writer: collected Located instructions
-- State: counter for fresh variables
type TM = RWS PosInf [LRawInst] Int

-- | Get the current source position from the reader context.
currentPos :: TM PosInf
currentPos = ask

-- | Run a computation with a specific source position.
withPos :: PosInf -> TM a -> TM a
withPos pos = local (const pos)

-- | Execute the given TM computation, returning the result value and the generated instructions,
-- removing them from the resulting computation (but keeping the counter).
intercept :: TM a -> TM (a, [LRawInst])
intercept = censor (const []) . listen


-- ===== Data structures for the abstraction of Raw computations =====

-- | Representation of a labelled value, consisting of three 'RawVar's.
-- Not to be distinguished with a runtime LVal, representing a labelled value in the runtime.
data LVal = LVal { rVal :: RawVar, rValLbl :: RawVar, rTyLbl :: RawVar }

-- | Abstraction of a label computation.
-- Now uses LVarAccess (Located VarAccess) to preserve source positions
data LabelComp
    -- | The label stored in the given Raw variable.
    = Lbl RawVar
    -- | The value label of the given variable.
    | ValLbl IR.LVarAccess
    -- | The type label of the given variable.
    | TyLbl IR.LVarAccess
    -- | The join of the labels described by the given label computations.
    | Join LabelComp LabelComp [LabelComp]
    -- | The current PC label.
    | PC

-- | Describes the computation of a simple or labelled value (depending on the expression).
-- See 'RawComp'.
-- Now uses LVarAccess (Located VarAccess) to preserve source positions
data ValueComp
    -- | Result is (or is the value of) the given labelled value.
    = RVar IR.LVarAccess
    -- | Result is what the given expression computes to.
    | RExpr RawExpr
    -- | Result is what the given expression, parametrized over one variable, computes to.
    | RUn IR.LVarAccess (RawVar -> RawExpr)
    -- | Result is what the given expression, parametrized over two variables, computes to.
    | RBin IR.LVarAccess IR.LVarAccess (RawVar -> RawVar -> RawExpr)

-- | Describes the computation of a labelled value, divided into a "value computation"
-- and two label computations.
-- NOTE: The computations will be carried out in the order the fields are given in.
data RawComp
    -- | A simple computation where the given 'ValueComp' must produce a simple (unlabelled) raw value
    -- and where the labels are solely determined based on the inputs and PC.
    = SimpleRawComp
    { cVal :: ValueComp
    , cValLbl :: LabelComp
    , cTyLbl :: LabelComp
    }
    -- | A complex computation where the given 'ValueComp' must produce a labelled value.
    -- These labels are parameters to the label computations.
    | ComplexRawComp
    { ccVal :: ValueComp
    , ccValLbl :: LabelComp -> LabelComp
    , ccTyLbl :: LabelComp -> LabelComp
    }


-- ===== Helper functions =====

nextVarNum :: TM Int
nextVarNum = do
  i <- get
  put (i + 1)
  return i

freshRawVar :: TM RawVar
freshRawVar = freshRawVarWith "_raw_"

freshRawVarWith :: String -> TM RawVar
freshRawVarWith s = do
  i <- nextVarNum
  return $ RawVar $ s ++ show i

freshLValVar :: TM VarName
freshLValVar = do
  i <- nextVarNum
  return $ VN ("lval" ++ show i)

-- | Assign an expression to a Raw variable using the current position context.
assignRExpr :: RawExpr -> TM RawVar
assignRExpr e = do
  r <- freshRawVar
  pos <- currentPos
  tell [Loc pos (AssignRaw r e)]
  return r

-- | Assign an expression to the given variable using the current position context.
assignLVal :: VarName -> RawExpr -> TM ()
assignLVal vn e = do
  pos <- currentPos
  tell [Loc pos (AssignLVal vn e)]

-- | Assign an expression to a new variable representing a labelled value.
assignLVal' :: RawExpr -> TM VarAccess
assignLVal' e = do
  vn <- freshLValVar
  assignLVal vn e
  return $ IR.VarLocal vn

-- | Construct a labelled value in the runtime from the given 'LVal' and assign it to the given variable.
constructLVal :: VarName -> LVal -> TM ()
constructLVal vn LVal{..} = assignLVal vn (ConstructLVal rVal rValLbl rTyLbl)

-- | Construct a labelled value in the runtime from the given 'LVal' and assign it to a new variable.
constructLVal' :: LVal -> TM VarAccess
constructLVal' LVal{..} = assignLVal' $ ConstructLVal rVal rValLbl rTyLbl

-- | Generate instructions assigning the components of a runtime LVal
-- to individual variables.
-- Now takes LVarAccess and uses its position for generated instructions.
getLVal :: IR.LVarAccess -> TM LVal
getLVal lva@(Loc vaPos _) = do
  rVal <- freshRawVarWith "_val_"
  rValLbl <- freshRawVarWith "_vlbl_"
  rTyLbl <- freshRawVarWith "_tlbl_"
  -- Use the variable's position for the generated instructions
  let pos = vaPos

  mapM_
    (\(r, f) -> tell [Loc pos (AssignRaw r (ProjectLVal lva f))])
    [ (rVal, FieldValue),
      (rValLbl, FieldValLev),
      (rTyLbl, FieldTypLev)
    ]

  return LVal {..}

-- | Generate instructions setting the three parts of the R0 register
-- to the values of the given 'RawVar's.
setR0 :: LVal -> TM ()
setR0 LVal{..} = do
    pos <- currentPos
    tell [ Loc pos (SetState R0_Val rVal)
         , Loc pos (SetState R0_Lev rValLbl)
         , Loc pos (SetState R0_TLev rTyLbl)
         ]

-- | Generate instructions assigning the three parts of the R0 register
-- to new Raw variables.
getR0 :: TM LVal
getR0 = do
  rVal <- freshRawVarWith "_$reg0_val_"
  rValLbl <- freshRawVarWith "_$reg0_vlbl_"
  rTyLbl <- freshRawVarWith "_$reg0_tlbl_"
  pos <- currentPos
  tell [ Loc pos (AssignRaw rVal (ProjectState R0_Val))
       , Loc pos (AssignRaw rValLbl (ProjectState R0_Lev))
       , Loc pos (AssignRaw rTyLbl (ProjectState R0_TLev))
       ]
  return LVal{..}

-- | Generate instructions assigning the value of the given runtime LVal
-- to a new Raw variable.
-- Now takes LVarAccess and uses its position for generated instructions.
getVal :: IR.LVarAccess -> TM RawVar
getVal lva@(Loc vaPos _) = do
    rVal <- freshRawVarWith "_val_"
    -- Use the variable's position for the generated instruction
    tell [Loc vaPos (AssignRaw rVal (ProjectLVal lva FieldValue))]
    return rVal

-- | Special case projection for self-references, now working with LVarAccess
-- Returns a RawExpr appropriate for the field projection
projectLVal :: IR.LVarAccess -> LValField -> RawExpr
projectLVal (Loc _ VarFunSelfRef) FieldTypLev = ProjectState MonPC
projectLVal (Loc _ VarFunSelfRef) FieldValLev = ProjectState MonPC
projectLVal lva field = ProjectLVal lva field

-- | Generate instructions assigning the value label of the given runtime LVal
-- to a new Raw variable.
-- Now takes LVarAccess and uses its position for generated instructions.
getValLbl :: IR.LVarAccess -> TM RawVar
getValLbl lva@(Loc vaPos _) = do
    rValLbl <- freshRawVarWith "_vlbl_"
    -- Use the variable's position for the generated instruction
    tell [Loc vaPos (AssignRaw rValLbl (projectLVal lva FieldValLev))]
    return rValLbl

-- | Generate instructions assigning the type label of the given runtime LVal
-- to a new Raw variable.
-- Now takes LVarAccess and uses its position for generated instructions.
getTyLbl :: IR.LVarAccess -> TM RawVar
getTyLbl lva@(Loc vaPos _) = do
    rTyLbl <- freshRawVarWith "_tlbl_"
    -- Use the variable's position for the generated instruction
    tell [Loc vaPos (AssignRaw rTyLbl (projectLVal lva FieldTypLev))]
    return rTyLbl

-- | Generate instructions assigning the current PC label to a new Raw variable.
getPC :: TM RawVar
getPC = do
    pc <- freshRawVarWith "_pc_"
    pos <- currentPos
    tell [Loc pos (AssignRaw pc (ProjectState MonPC))]
    return pc

-- | Generate instructions assigning the current blocking label to a new Raw variable.
getBlock :: TM RawVar
getBlock = do
    bl <- freshRawVarWith "_bl_"
    pos <- currentPos
    tell [Loc pos (AssignRaw bl (ProjectState MonBlock))]
    return bl

-- | Generate instructions raising the PC with the label in the given variable.
-- Not to be used directly, see functions below instead.
_raisePC :: RawVar -> TM ()
_raisePC raiseBy = do
  pc <- getPC
  pc' <- freshRawVarWith "_pc_"
  pos <- currentPos
  tell [ Loc pos (AssignRaw pc' (_default_bin Basics.LatticeJoin pc raiseBy))
       , Loc pos (SetState MonPC pc')
       ]

_default_bin op r1 r2 = Bin op (UseNativeBinop False) r1 r2

-- | Generate instructions raising the blocking label with the label in the given variable.
-- Not to be used directly, see functions below instead.
_raiseBlock :: RawVar -> TM ()
_raiseBlock raiseBy = do
  bl  <- getBlock
  bl' <- freshRawVarWith "_bl_"
  pos <- currentPos
  tell [ Loc pos (AssignRaw bl' (_default_bin Basics.LatticeJoin bl raiseBy))
       , Loc pos (SetState MonBlock bl')
       ]

-- | Generate instructions raising both PC and blocking label with the label in the given variable.
-- Because of the invariant pc ⊑ block, the blocking label should always be raised when raising PC.
raisePCAndBlock :: LabelComp -> TM ()
raisePCAndBlock raiseByComp = do
  raiseBy <- compLabel raiseByComp
  _raisePC raiseBy
  _raiseBlock raiseBy

-- | Generate instructions raising the blocking label with the label in the given variable.
raiseBlock :: LabelComp -> TM ()
raiseBlock raiseByComp = do
  raiseBy <- compLabel raiseByComp
  _raiseBlock raiseBy

-- | Generate instructions for asserting the type of the given runtime LVal.
-- Consists of first raising the blocking label with the type label of that LVal and
-- then an assert instruction with the given type.
-- Now takes LVarAccess and uses its position for generated instructions.
assertTypeAndRaise :: IR.LVarAccess -> RawType -> TM ()
assertTypeAndRaise lva@(Loc vaPos _) t = do
  raiseBlock $ TyLbl lva
  r <- getVal lva
  -- Use the variable's position for the assertion
  tell [ Loc vaPos (RTAssertion (AssertType r t)) ]

-- Note: Currently, RT does not support general type equality check.
-- assertEqTypes :: [RawType] -> RawVar -> RawVar -> TM ()

-- | Generate instructions for asserting that the types of the given runtime LVals
-- are either both string or both numbers.
-- Now takes LVarAccess and uses positions for generated instructions.
assertTypesBothStringsOrBothNumbers :: IR.LVarAccess -> IR.LVarAccess -> TM ()
assertTypesBothStringsOrBothNumbers lva1 lva2 = do
  raiseBlock $ Join (TyLbl lva1) (TyLbl lva2) []
  r1 <- getVal lva1
  r2 <- getVal lva2
  pos <- currentPos
  tell [Loc pos (RTAssertion (AssertTypesBothStringsOrBothNumbers r1 r2))]

-- | Generate instructions raising the blocking label with the value label of the
-- given runtime LVal and an assertion based on the value, specified by the given function.
-- Now takes LVarAccess and uses its position for generated instructions.
assertWithValAndRaise :: IR.LVarAccess -> (RawVar -> RTAssertion) -> TM ()
assertWithValAndRaise lva@(Loc vaPos _) f = do
  raiseBlock $ ValLbl lva
  r <- getVal lva
  -- Use the variable's position for the assertion
  tell [Loc vaPos (RTAssertion (f r))]

-- | See 'InvalidateSparseBit'.
invalidateSparseBit :: TM ()
invalidateSparseBit = do
  pos <- currentPos
  tell [Loc pos InvalidateSparseBit]


-- ===== Translations from the defined abstractions to Raw instructions =====

-- | Generate instructions for a 'ValueComp' where the expected result is a simple raw value.
compSimple :: ValueComp -> TM RawVar
compSimple = \case
  RVar va -> getVal va
  RExpr e -> assignRExpr e
  RUn va f -> do
    r <- getVal va
    assignRExpr $ f r
  RBin va1 va2 f -> do
    r1 <- getVal va1
    r2 <- getVal va2
    assignRExpr $ f r1 r2

-- | Generate instructions for a 'ValueComp' where the expected result is a labelled value.
-- The main difference to 'compSimple' is the return type, together with that the 'AssignLVal'
-- instruction is used instead of the 'AssignRaw' instruction, as a labelled value is computed.
compComplex :: ValueComp -> TM VarAccess
compComplex = \case
  RVar (Loc _ va) -> return va  -- Extract VarAccess from LVarAccess
  RExpr e -> assignLVal' e
  RUn lva f -> do
    r <- getVal lva
    assignLVal' $ f r
  RBin lva1 lva2 f -> do
    r1 <- getVal lva1
    r2 <- getVal lva2
    assignLVal' $ f r1 r2

-- | Generate instructions for a 'LabelComp'.
compLabel :: LabelComp -> TM RawVar
compLabel = \case
  Lbl r -> return r
  ValLbl va -> getValLbl va
  TyLbl va -> getTyLbl va
  Join c1 c2 cs -> do
    r <- compLabel c1
    rs <- mapM compLabel (c2:cs)
    pos <- currentPos
    if null rs
    then return r
    else foldM (\(r1 :: RawVar) (r2 :: RawVar) -> do
                   r' :: RawVar <- freshRawVarWith "_lbl_"
                   tell [ Loc pos (AssignRaw r' (_default_bin Basics.LatticeJoin r1 r2)) ]
                   return r'
               ) r rs
  PC -> getPC

-- | Generate instructions joining the current PC label into the given
-- variable's value and type labels.
-- Now takes LVarAccess to preserve source positions.
-- Returns LVarAccess (NoPos since this is a compiler-generated intermediate value).
pcTaint :: IR.LVarAccess -> TM IR.LVarAccess
pcTaint lva = do
  rVal <- getVal lva
  rValLbl <- compLabel $ Join PC (ValLbl lva) []
  rTyLbl <- compLabel $ Join PC (TyLbl lva) []
  va <- constructLVal' $ LVal{..}
  return $ noLocVA va


-- ===== Translation functions for the different components of a program =====

-- | Wrap a VarAccess with NoPos to create an LVarAccess for compiler-generated variables
noLocVA :: VarAccess -> IR.LVarAccess
noLocVA va = Loc NoPos va

expr2raw :: IR.IRExpr -> TM LVal
expr2raw e = expr2rawComp e >>= \case
    SimpleRawComp{..} -> do
      rVal <- compSimple cVal
      rValLbl <- compLabel cValLbl
      rTyLbl <- compLabel cTyLbl
      return LVal{..}
    ComplexRawComp{..} -> do
      v <- compComplex ccVal
      -- Wrap with NoPos since v is a compiler-generated intermediate variable
      let lv = noLocVA v
      rVal <- getVal lv
      rResValLbl <- getValLbl lv
      rResTyLbl <- getTyLbl lv
      rValLbl <- compLabel (ccValLbl $ Lbl rResValLbl)
      rTyLbl <- compLabel (ccTyLbl $ Lbl rResTyLbl)
      return LVal{..}

-- | Definition of the Raw computations for expressions.
expr2rawComp :: IR.IRExpr -> TM RawComp
expr2rawComp = \case
  IR.Const lit -> return $ SimpleRawComp
    { cVal = RExpr $ Const lit
    , cValLbl = PC
    , cTyLbl = PC
    }

  -- TODO Special cases should probably have their own type/instruction.
  IR.Base "$$authorityarg" ->
    let lva = noLocVA $ IR.VarLocal (VN "$$authorityarg") in
    return SimpleRawComp
    { cVal = RVar lva
    , cValLbl = ValLbl lva
    , cTyLbl = TyLbl lva
    }

  -- Revision 2023-08: Changed the runtime to create unlabelled values for
  -- base functions. Previously, a labelled value with bottom and null labels
  -- as created by the runtime, and then handled here like with 'ComplexRawComp',
  -- assigning constant PC labels.
  IR.Base v -> return $ SimpleRawComp
    { cVal = RExpr $ Base v
    , cValLbl = PC
    , cTyLbl = PC
    }

  -- The following constructor operations take labelled values as arguments,
  -- but these labels do not affect the labels of the resulting compound value.
  -- Now passes LVarAccess directly to Raw.Tuple (preserves source positions)
  IR.Tuple lvs ->
    return SimpleRawComp
      { cVal = RExpr $ Tuple lvs
      , cValLbl = PC
      , cTyLbl = PC
      }
  -- Now passes LVarAccess directly to Raw.List (preserves source positions)
  IR.List lvs ->
    return SimpleRawComp
      { cVal = RExpr $ List lvs
      , cValLbl = PC
      , cTyLbl = PC
      }
  -- Now passes LFields (with LVarAccess) directly to Raw.Record (preserves source positions)
  IR.Record lfs ->
    return SimpleRawComp
      { cVal = RExpr $ Record lfs
      , cValLbl = PC
      , cTyLbl = PC
      }

  -- The following two constructors extend an existing collection data structure
  -- with new values. The given labelled values are just added to the collection
  -- and not touched, therefore their labels are not joined into the datastructure's
  -- label.
  -- Now passes LVarAccess directly to Raw.ListCons head (preserves source positions)
  IR.ListCons lv ll -> do
    assertTypeAndRaise ll RawList
    return SimpleRawComp
      { cVal = RUn ll $ ListCons lv
      , cValLbl = Join PC (ValLbl ll) []
      , cTyLbl = PC
      }
  -- Now passes LFields (with LVarAccess) directly to Raw.WithRecord (preserves source positions)
  IR.WithRecord lv lfs -> do
    assertTypeAndRaise lv RawRecord
    return SimpleRawComp
      { cVal = RUn lv $ \r -> WithRecord r lfs
      , cValLbl = Join PC (ValLbl lv) []
      , cTyLbl = PC
      }

  -- For the following projection operations (as well as for 'Basics.Head'), a labelled value
  -- is extracted from a collection. The value label of the collection is joined into the value label
  -- of the extracted value. It is necessary to do this at projection, as the label of the
  -- data structure might have been raised.
  -- Revision 2023-08: 'ProjField' is the new name for the previous 'Proj' for records,
  -- to distinguish from the new 'ProjIdx' for tuples. The returned labelled value is
  -- the same. New is the 'AssertRecordHasField' assertion which was missing (together
  -- with raising the blocking label accordingly).
  -- Now uses LVarAccess for position tracking
  IR.ProjField lv field -> do
    assertTypeAndRaise lv RawRecord
    assertWithValAndRaise lv $ \r -> AssertRecordHasField r field
    return ComplexRawComp
      { ccVal = RUn lv $ \r -> ProjField r field
      , ccValLbl = \resValLbl -> Join PC (ValLbl lv) [resValLbl]
      , ccTyLbl = \resTyLbl -> Join PC resTyLbl []
      }
  -- Revision 2023-08: 'ProjIdx' is the new indexing operation for tuples replacing
  -- the previous 'Index'. The difference is that the index is a constant to the operation
  -- instead of a variable. Previously, the type label of the tuple was incorrectly joined
  -- into the result type label.
  -- Now uses LVarAccess for position tracking
  IR.ProjIdx lv idx -> do
    assertTypeAndRaise lv RawTuple
    assertWithValAndRaise lv $ \r -> AssertTupleLengthGreaterThan r idx
    return ComplexRawComp
      { ccVal = RUn lv $ \r -> ProjIdx r idx
      , ccValLbl = \resValLbl -> Join PC (ValLbl lv) [resValLbl]
      , ccTyLbl = \resTyLbl -> Join PC resTyLbl []
      }

  -- Revision 2023-08: Changed the RT operation to return an unlabelled value,
  -- as the labels are PC anyway.
  IR.Lib libname funname ->
    return SimpleRawComp
      { cVal = RExpr $ Lib libname funname
      , cValLbl = PC
      , cTyLbl = PC
      }

  -- Revision 2023-08: Raising of the blocking label with an argument's type label and
  -- an assertion on this argument are now coupled together for each argument (if possible).
  -- This results in higher granularity for the blocking label: when the first assertion fails,
  -- the blocking label is only raised with the type label of the first argument, not with that of
  -- the second.
  -- Now uses LVarAccess (lv1, lv2) for position tracking
  IR.Bin op lv1 lv2 ->
    -- Basic binary op computation, where the value label is the join of those of the input and the type label is PC
    -- (for operations where the result type is fixed).
    let basicBinOpComp =
         return SimpleRawComp
          { cVal = RBin lv1 lv2 $ Bin op (UseNativeBinop True)
          , cValLbl = Join PC (ValLbl lv1) [ValLbl lv2]
          , cTyLbl = PC
          }
        numBinOpComp = do
          assertTypeAndRaise lv1 RawNumber
          assertTypeAndRaise lv2 RawNumber
          basicBinOpComp
        stringBinOpComp = do
          assertTypeAndRaise lv1 RawString
          assertTypeAndRaise lv2 RawString
          basicBinOpComp
        -- Note: The result type is boolean in any case.
        numOrStringBinOpComp = do
          assertTypesBothStringsOrBothNumbers lv1 lv2
          basicBinOpComp

    in case op of
      Basics.Plus -> numBinOpComp
      Basics.Minus -> numBinOpComp
      Basics.Mult -> numBinOpComp
      -- Note: Division operations now check for division by zero.
      -- Revision 2023-08: Removed incorrect raising of PC by value label
      -- of second operand (the operation always succeeds).
      Basics.Div -> do
        assertTypeAndRaise lv1 RawNumber
        assertTypeAndRaise lv2 RawNumber
        assertWithValAndRaise lv2 $ \r -> AssertNotZero r
        basicBinOpComp
      Basics.Mod -> do
        assertTypeAndRaise lv1 RawNumber
        assertTypeAndRaise lv2 RawNumber
        assertWithValAndRaise lv2 $ \r -> AssertNotZero r
        basicBinOpComp
      Basics.IntDiv -> do
        assertTypeAndRaise lv1 RawNumber
        assertTypeAndRaise lv2 RawNumber
        assertWithValAndRaise lv2 $ \r -> AssertNotZero r
        basicBinOpComp
      Basics.Gt -> numOrStringBinOpComp
      Basics.Lt -> numOrStringBinOpComp
      Basics.Ge -> numOrStringBinOpComp
      Basics.Le -> numOrStringBinOpComp
      Basics.Concat -> stringBinOpComp

      -- Revision 2023-08: Changed special handling as a "ComplexBin" (where the
      -- inputs to the runtime operation were labelled values) to a normal
      -- 'ComplexRawComp', where label computations with the labels of the inputs
      -- are handled here instead. This removes the need for the runtime to compute
      -- joins for shallow comparisons and allows for more optimization at Raw level.
      -- The runtime only considers labels of nested values, which are joined into
      -- the the returned labelled value (which is then joined here).
      -- The downside is that we are now always constructing a new LVal here, in
      -- addition to the one constructed in the RT, and that we always have to join the
      -- returned value label.
      -- Note: Even though the result depends on the types of the parameters, it is sufficient to join their
      -- value labels into the result's value label, due to the invariant tyLbl ⊑ valLbl.
      Basics.Eq -> return ComplexRawComp
        { ccVal = RBin lv1 lv2 $ Bin op (UseNativeBinop False)
        , ccValLbl = \resValLbl -> Join PC (ValLbl lv1) [ValLbl lv2, resValLbl]
        , ccTyLbl = const PC -- The result type is always boolean
        }
      Basics.Neq -> return ComplexRawComp
        { ccVal = RBin lv1 lv2 $ Bin op (UseNativeBinop False)
        , ccValLbl = \resValLbl -> Join PC (ValLbl lv1) [ValLbl lv2, resValLbl]
        , ccTyLbl = const PC -- The result type is always boolean
        }
      Basics.HasField -> do
        assertTypeAndRaise lv1 RawRecord
        assertTypeAndRaise lv2 RawString
        basicBinOpComp

      -- IFC Level Operations

      -- Revision 2023-08: Introduced new instruction InvalidateSparseBit
      -- (before this was called by a runtime raisedTo operation, which is now not necessary anymore).
      -- Otherwise equivalent except for order of instructions.
      Basics.RaisedTo -> do
        assertTypeAndRaise lv2 RawLevel
        rRaiseTo <- getVal lv2
        invalidateSparseBit
        return SimpleRawComp
          { cVal = RVar lv1
          , cValLbl = Join PC (ValLbl lv1) [ValLbl lv2, Lbl rRaiseTo]
          , cTyLbl = Join PC (TyLbl lv1) []
          }

      Basics.LatticeMeet -> do
        assertTypeAndRaise lv1 RawLevel
        assertTypeAndRaise lv2 RawLevel
        return SimpleRawComp
          { cVal = RBin lv1 lv2 $ Bin op $ (UseNativeBinop False)
          , cValLbl = Join PC (ValLbl lv1) [ValLbl lv2]
          , cTyLbl = Join PC (TyLbl lv1) [TyLbl lv2]
          }

      Basics.LatticeJoin -> do
        assertTypeAndRaise lv1 RawLevel
        assertTypeAndRaise lv2 RawLevel
        return SimpleRawComp
          { cVal = RBin lv1 lv2 $ Bin op $ (UseNativeBinop False)
          , cValLbl = Join PC (ValLbl lv1) [ValLbl lv2]
          , cTyLbl = Join PC (TyLbl lv1) [TyLbl lv2]
          }

      -- Bit operations
      Basics.BinAnd -> numBinOpComp
      Basics.BinOr -> numBinOpComp
      Basics.BinXor -> numBinOpComp
      Basics.BinShiftLeft -> numBinOpComp
      Basics.BinShiftRight -> numBinOpComp
      Basics.BinZeroShiftRight -> numBinOpComp

      -- TODO Implement remaining operations
      _ -> error $ "Binary operation not yet implemented: " ++ show op

  -- Now uses LVarAccess (lv) for position tracking
  IR.Un op lv ->
    -- Basic unary op computation, where the value label is that of the input and the type label is PC
    -- (for operations where the result type is fixed).
    let basicUnOpComp =
         return SimpleRawComp
          { cVal = RUn lv $ Un op
          , cValLbl = Join PC (ValLbl lv) []
          , cTyLbl = PC
          }
    in case op of
      -- Revision 2023-08: Not raising block for IsTuple, IsList, IsRecord anymore, as they cannot fail. Otherwise equivalent.
      Basics.IsTuple -> basicUnOpComp
      Basics.IsList -> basicUnOpComp
      Basics.IsRecord -> basicUnOpComp
      -- Revision 2023-08: Separate operations for list and tuple length.
      -- Now also asserting the type (before only block was raised).
      Basics.ListLength -> do
        assertTypeAndRaise lv RawList
        basicUnOpComp
      Basics.TupleLength -> do
        assertTypeAndRaise lv RawTuple
        basicUnOpComp
      Basics.RecordSize -> do
        assertTypeAndRaise lv RawRecord
        basicUnOpComp
      -- Revision 2023-08: Equivalent.
      Basics.Tail -> do
        assertTypeAndRaise lv RawList
        basicUnOpComp
      -- Revision 2023-08: Now also joining PC into value label (missed due to a typo in the previous version)
      Basics.Head -> do
        assertTypeAndRaise lv RawList
        return ComplexRawComp
          { ccVal = RUn lv $ Un op
          , ccValLbl = \resValLbl -> Join PC (ValLbl lv) [resValLbl]
          , ccTyLbl = \resTyLbl -> Join PC resTyLbl []
          }
      -- Revision 2023-08: Now setting type label to PC instead of joining original type label (as the type is asserted).
      Basics.UnMinus -> do
        assertTypeAndRaise lv RawNumber
        basicUnOpComp

      Basics.Not -> do
        assertTypeAndRaise lv RawBoolean
        basicUnOpComp

      -- TODO Implement remaining operations
      _ -> error $ "Unary operation not yet implemented: " ++ show op


-- Revision 2023-08: Changed and moved handling of the complex operations Eq and Neq to expr2Raw.
-- | Generate raw instructions for the given Located IR instruction.
-- Adapter: extracts position from Located wrapper and uses it for generated instructions.
inst2raw :: IR.LIRInst -> TM ()
inst2raw (Loc pos inst) = case inst of
  -- Note: This is the only place where expressions occur in an IR program.
  -- We use withPos to set the position context for all generated instructions.
  IR.Assign vn expr -> withPos pos $ do
    LVal{..} <- expr2raw expr
    -- Joining PC to be safe, even though for now PC is always joined when computing the expression (and it will be optimized away).
    rValLbl' <- compLabel $ Join PC (Lbl rValLbl) []
    rTyLbl' <- compLabel $ Join PC (Lbl rTyLbl) []
    constructLVal vn LVal{rValLbl = rValLbl', rTyLbl = rTyLbl', .. }

  IR.MkFunClosures vs env -> do
    -- The generation of closures and the related monitoring is first implemented in stack generation,
    -- to be able to use cyclic pointers for constructing the environments.
    tell [Loc pos (MkFunClosures vs env)]


-- | Translate a Located IR terminator to a Located Raw terminator, generating instructions.
-- Extracts position from Located wrapper and uses it for generated terminators.
-- IRTerminator now uses LVarAccess directly for variable references.
tr2raw :: IR.LIRTerminator -> TM LRawTerminator
tr2raw (Loc pos term) = case term of
  -- Revision 2023-08: Equivalent except for the additional redundant raise.
  -- lv1 and lv2 are now LVarAccess (from IRTerminator)
  IR.TailCall lv1 lv2 -> do
    raisePCAndBlock $ ValLbl lv1
    -- Note: The raise here is redundant because we have already raised by the value label above.
    -- However, optimizations aware of the relation between type- and value label will remove it.
    assertTypeAndRaise lv1 RawFunction
    setR0 =<< getLVal lv2
    r <- getVal lv1 -- labels of lv1 are dismissed at this point
    return $ Loc pos (TailCall r)

  -- Revision 2023-08: This generates now more instructions than before,
  -- as we also construct LVals instead of just working on single RawVars,
  -- but after optimization with RawOpt the result is the same.
  -- lv is now LVarAccess (from IRTerminator)
  IR.Ret lv -> do
    -- At this point, we taint value and type label of the to-be-returned value with PC,
    -- as both value and type can depend on it. Note: when implementing more fine-grained type labels,
    -- the type label should not always be tainted here.
    setR0 =<< getLVal =<< pcTaint lv
    return $ Loc pos Ret

  -- lv is now LVarAccess (from IRTerminator)
  -- Raw.LibExport now takes LVarAccess directly
  IR.LibExport lv ->
    return $ Loc pos (LibExport lv)

  -- Revision 2023-08: Equivalent except for the additional redundant raise.
  -- lv is now LVarAccess (from IRTerminator)
  IR.If lv bb1 bb2 -> do
    raisePCAndBlock $ ValLbl lv
    bb1' <- tree2raw bb1
    bb2' <- tree2raw bb2
    -- Note: The raise here is redundant because we have already raised by the value label above.
    -- However, optimizations aware of the relation between type- and value label will remove it.
    assertTypeAndRaise lv RawBoolean
    tell [ Loc pos SetBranchFlag ]
    r <- getVal lv
    return $ Loc pos (If r bb1' bb2')

  -- Revision 2023-08: Equivalent, only way of modifying bb2 changed.
  IR.StackExpand v irBB1 irBB2 -> do
    bb1 <- tree2raw irBB1
    BB insts2 tr2 <- tree2raw irBB2
    -- Prepend before insts2 instructions to store in variable v the result
    -- of executing BB1 (expected in R0 after a RET which eventually ends execution of BB1).
    (_, insts2') <- intercept $ do
                      r0 <- getR0
                      constructLVal v r0
                      tell insts2 -- Note on performance: concatenating lists might be slow,
                                  -- generally using Sequence (faster concatenation) for instructions
                                  -- might improve performance
    let bb2 = BB insts2' tr2
    return $ Loc pos (StackExpand bb1 bb2)

  -- Note: This is translated into branching and Error for throwing RT exception
  -- Revision 2023-08: More fine-grained raising of blocking label, see below.
  -- Note: pos is the error source location from the Located wrapper
  -- lv1 and lverr are now LVarAccess (from IRTerminator)
  IR.AssertElseError lv1 irBB lverr -> do
    -- Note: We are first raising the blocking label with the type label,
    -- then assert the type and then raise with the value label.
    -- Raising with the value label directly would be sufficient,
    -- but with the two-step raising we obtain a more fine-grained
    -- blocking label for the assertion. In case more is know about
    -- value and type label, optimizations might eliminated the second raise.
    assertTypeAndRaise lv1 RawBoolean
    raiseBlock $ ValLbl lv1
    bb <- tree2raw irBB
    -- Generate the BB for the error case, using the error source position from Located wrapper
    (tr_err, insts_err) <- intercept $ tr2rawError lverr pos
    let bb_err = BB insts_err tr_err
    r <- getVal lv1
    return $ Loc pos (If r bb bb_err)

  -- Revision 2023-08: Now asserting that verr is a string. Also raising both PC
  -- and blocking label to the error message's value label (which will become
  -- obsolete with an improvement moving the arguments of the Raw.Error
  -- instruction to R0).
  -- Note: pos is the error source location from the Located wrapper
  -- lverr is now LVarAccess (from IRTerminator)
  IR.Error lverr -> tr2rawError lverr pos

-- | Helper for Error translation with explicit error position
-- Note: errPos is the error source location (PosInf from Located wrapper)
-- Takes LVarAccess for the error variable.
tr2rawError :: IR.LVarAccess -> PosInf -> TM LRawTerminator
tr2rawError lverr errPos = do
  -- Note: first raising block with type label and then with value label; see AssertElseError for explanation.
  assertTypeAndRaise lverr RawString
  -- Note: There is no value label anymore at Raw level; instead join the label into PC (which e.g. determines whether are allowed to print the message)
  raisePCAndBlock $ ValLbl lverr
  r <- getVal lverr
  -- Note: Error still contains errPos as the error source location for runtime error messages
  return $ Loc errPos (Error r)


-- Revision 2023-08: unchanged
-- | Translate an IR tree to a Raw tree (does not add any instructions to the monad).
tree2raw :: IR.IRBBTree -> TM RawBBTree
tree2raw (IR.BB irInsts irTr) = do
    -- Generate Raw instructions for the instructions of the block and the terminator.
    (tr, insts) <- intercept
            $ mapM_ inst2raw irInsts >> tr2raw irTr -- inst2raw only generates instructions without result value, tr2raw adds instructions and returns resulting RawTerminator
    return $ BB insts tr

-- Revision 2023-08: new code, but equivalent
-- Now produces LFunDef (Located FunDef) with position on wrapper.
fun2raw :: IR.LFunDef -> LFunDef
fun2raw lirfdef@(Loc funDefPos irfdef@(IR.FunDef hfn (Loc argPos vname) consts (IR.BB irInsts irTr))) =
   Loc funDefPos (FunDef hfn rawConsts (BB insts tr) irfdef)
      where ((tr, rawConsts), insts) = evalRWS comp NoPos 0
            comp = do
              -- Store the argument from R0 in the variable under which the argument is expected.
              -- Use the argument's source position for the R0 read instructions.
              r0 <- withPos argPos getR0
              constructLVal vname r0
              -- Generate instructions creating LVals for the constants
              pc <- getPC
              rawConsts <- forM consts $ \(v@(VN vn), constVal) -> do
                let r = RawVar $ vn ++ "$$$const"
                constructLVal v LVal { rVal = r, rValLbl = pc, rTyLbl = pc }
                return (r, constVal)
              -- Generate the instructions for the BB
              mapM_ inst2raw irInsts
              -- Generate instructions for and translate the terminator
              tr' <- tr2raw irTr
              return (tr', rawConsts)

-- Note: FunSerialization contains FunDef without Located wrapper,
-- so we wrap with noLoc when deserializing.
ir2raw :: IR.SerializationUnit -> RawUnit
ir2raw (IR.FunSerialization f) = FunRawUnit (fun2raw (Loc NoPos f))
ir2raw (IR.AtomsSerialization c) = AtomRawUnit c
ir2raw (IR.ProgramSerialization prog) = ProgramRawUnit (prog2raw prog)

prog2raw :: IR.IRProgram -> RawProgram
prog2raw (IR.IRProgram atoms funs) =
    RawProgram atoms (map fun2raw funs)


