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
import Control.Monad.Trans.RWS(RWS, evalRWS, ask, get, put, tell, censor, listen)

-- ===== Monad definition =====

-- | Translation monad
-- Reader: currently not used
-- Writer: collected instructions
-- State: counter for fresh variables
type TM = RWS () [RawInst] Int

-- | Execute the given TM computation, returning the result value and the generated instructions,
-- removing them from the resulting computation (but keeping the counter).
intercept :: TM a -> TM (a, [RawInst])
intercept = censor (const []) . listen


-- ===== Data structures for the abstraction of Raw computations =====

-- | Representation of a labelled value, consisting of three 'RawVar's.
-- Not to be distinguished with a runtime LVal, representing a labelled value in the runtime.
data LVal = LVal { rVal :: RawVar, rValLbl :: RawVar, rTyLbl :: RawVar }

-- | Abstraction of a label computation.
data LabelComp
    -- | The label stored in the given Raw variable.
    = Lbl RawVar
    -- | The value label of the given variable.
    | ValLbl VarAccess
    -- | The type label of the given variable.
    | TyLbl VarAccess
    -- | The join of the labels described by the given label computations.
    | Join LabelComp LabelComp [LabelComp]
    -- | The current PC label.
    | PC

-- | Describes the computation of a simple or labelled value (depending on the expression).
-- See 'RawComp'.
data ValueComp
    -- | Result is (or is the value of) the given labelled value.
    = RVar VarAccess
    -- | Result is what the given expression computes to.
    | RExpr RawExpr
    -- | Result is what the given expression, parametrized over one variable, computes to.
    | RUn VarAccess (RawVar -> RawExpr)
    -- | Result is what the given expression, parametrized over two variables, computes to.
    | RBin VarAccess VarAccess (RawVar -> RawVar -> RawExpr)

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

-- | Assign an expression to a Raw variable.
assignRExpr :: RawExpr -> TM RawVar
assignRExpr e = do
  r <- freshRawVar
  tell [AssignRaw r e]
  return r

-- | Assign an expression to the given variable.
assignLVal :: VarName -> RawExpr -> TM ()
assignLVal vn e = do
  tell [AssignLVal vn e]

-- | Assign an expression to a new variable representing a labelled value.
assignLVal' :: RawExpr -> TM VarAccess
assignLVal' e = do
  vn <- freshLValVar
  assignLVal vn e
  return $ IR.VarLocal vn

-- | Construct a labelled value in the runtime from the given 'LVal' and assign it to the given variable.
constructLVal :: VarName -> LVal -> TM ()
constructLVal vn LVal{..} = assignLVal vn $ ConstructLVal rVal rValLbl rTyLbl

-- | Construct a labelled value in the runtime from the given 'LVal' and assign it to a new variable.
constructLVal' :: LVal -> TM VarAccess
constructLVal' LVal{..} = assignLVal' $ ConstructLVal rVal rValLbl rTyLbl

-- | Generate instructions assigning the components of a runtime LVal
-- to individual variables.
getLVal :: VarAccess -> TM LVal
getLVal va = do
  rVal <- freshRawVarWith "_val_"
  rValLbl <- freshRawVarWith "_vlbl_"
  rTyLbl <- freshRawVarWith "_tlbl_"

  mapM_
    (\(r, f) -> tell [AssignRaw r (ProjectLVal va f)])
    [ (rVal, FieldValue),
      (rValLbl, FieldValLev),
      (rTyLbl, FieldTypLev)
    ]

  return LVal {..}

-- | Generate instructions setting the three parts of the R0 register
-- to the values of the given 'RawVar's.
setR0 :: LVal -> TM ()
setR0 LVal{..} =
    tell [ SetState R0_Val rVal
         , SetState R0_Lev rValLbl
         , SetState R0_TLev rTyLbl
         ]

-- | Generate instructions assigning the three parts of the R0 register
-- to new Raw variables.
getR0 :: TM LVal
getR0 = do
  rVal <- freshRawVarWith "_$reg0_val_"
  rValLbl <- freshRawVarWith "_$reg0_vlbl_"
  rTyLbl <- freshRawVarWith "_$reg0_tlbl_"
  tell [ AssignRaw rVal (ProjectState R0_Val)
       , AssignRaw rValLbl (ProjectState R0_Lev)
       , AssignRaw rTyLbl (ProjectState R0_TLev)
       ]
  return LVal{..}

-- | Generate instructions assigning the value of the given runtime LVal
-- to a new Raw variable.
getVal :: VarAccess -> TM RawVar
getVal va = do
    rVal <- freshRawVarWith "_val_"
    tell [AssignRaw rVal (ProjectLVal va FieldValue)]
    return rVal

-- | Generate instructions assigning the value label of the given runtime LVal
-- to a new Raw variable.
getValLbl :: VarAccess -> TM RawVar
getValLbl va = do
    rValLbl <- freshRawVarWith "_vlbl_"
    tell [AssignRaw rValLbl (ProjectLVal va FieldValLev)]
    return rValLbl

-- | Generate instructions assigning the type label of the given runtime LVal
-- to a new Raw variable.
getTyLbl :: VarAccess -> TM RawVar
getTyLbl va = do
    rTyLbl <- freshRawVarWith "_tlbl_"
    tell [AssignRaw rTyLbl (ProjectLVal va FieldTypLev)]
    return rTyLbl

-- | Generate instructions assigning the current PC label to a new Raw variable.
getPC :: TM RawVar
getPC = do
    pc <- freshRawVarWith "_pc_"
    tell [AssignRaw pc (ProjectState MonPC) ]
    return pc

-- | Generate instructions assigning the current blocking label to a new Raw variable.
getBlock :: TM RawVar
getBlock = do
    bl <- freshRawVarWith "_bl_"
    tell [AssignRaw bl (ProjectState MonBlock) ]
    return bl

-- | Generate instructions raising the PC with the label in the given variable.
-- Not to be used directly, see functions below instead.
_raisePC :: RawVar -> TM ()
_raisePC raiseBy = do
  pc <- getPC
  pc' <- freshRawVarWith "_pc_"
  tell [ AssignRaw pc' (Bin Basics.LatticeJoin pc raiseBy)
       , SetState MonPC pc'
       ]

-- | Generate instructions raising the blocking label with the label in the given variable.
-- Not to be used directly, see functions below instead.
_raiseBlock :: RawVar -> TM ()
_raiseBlock raiseBy = do
  bl  <- getBlock
  bl' <- freshRawVarWith "_bl_"
  tell [ AssignRaw bl' (Bin Basics.LatticeJoin bl raiseBy)
       , SetState MonBlock bl'
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
assertTypeAndRaise :: VarAccess -> RawType -> TM ()
assertTypeAndRaise va t = do
  raiseBlock $ TyLbl va
  r <- getVal va
  tell [ RTAssertion $ AssertType r t ]

-- Note: Currently, RT does not support general type equality check.
-- assertEqTypes :: [RawType] -> RawVar -> RawVar -> TM ()

-- | Generate instructions for asserting that the types of the given runtime LVals
-- are either both string or both numbers.
assertTypesBothStringsOrBothNumbers :: VarAccess -> VarAccess -> TM ()
assertTypesBothStringsOrBothNumbers va1 va2 = do
  raiseBlock $ Join (TyLbl va1) (TyLbl va2) []
  r1 <- getVal va1
  r2 <- getVal va2
  tell [RTAssertion $ AssertTypesBothStringsOrBothNumbers r1 r2]

-- | Generate instructions raising the blocking label with the value label of the
-- given runtime LVal and an assertion based on the value, specified by the given function.
assertWithValAndRaise :: VarAccess -> (RawVar -> RTAssertion) -> TM ()
assertWithValAndRaise va f = do
  raiseBlock $ ValLbl va
  r <- getVal va
  tell [RTAssertion $ f r]

-- | See 'InvalidateSparseBit'.
invalidateSparseBit :: TM ()
invalidateSparseBit = tell [InvalidateSparseBit]


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
  RVar va -> return va
  RExpr e -> assignLVal' e
  RUn va f -> do
    r <- getVal va
    assignLVal' $ f r
  RBin va1 va2 f -> do
    r1 <- getVal va1
    r2 <- getVal va2
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
    if null rs
    then return r
    else foldM (\(r1 :: RawVar) (r2 :: RawVar) -> do
                   r' :: RawVar <- freshRawVarWith "_lbl_"
                   tell [ AssignRaw r' $ Bin Basics.LatticeJoin r1 r2 ]
                   return r'
               ) r rs
  PC -> getPC

-- | Generate instructions joining the current PC label into the given
-- variable's value and type labels.
pcTaint :: VarAccess -> TM VarAccess
pcTaint va = do
  rVal <- getVal va
  rValLbl <- compLabel $ Join PC (ValLbl va) []
  rTyLbl <- compLabel $ Join PC (TyLbl va) []
  constructLVal' $ LVal{..}


-- ===== Translation functions for the different components of a program =====

expr2raw :: IR.IRExpr -> TM LVal
expr2raw e = expr2rawComp e >>= \case
    SimpleRawComp{..} -> do
      rVal <- compSimple cVal
      rValLbl <- compLabel cValLbl
      rTyLbl <- compLabel cTyLbl
      return LVal{..}
    ComplexRawComp{..} -> do
      v <- compComplex ccVal
      rVal <- getVal v
      rResValLbl <- getValLbl v
      rResTyLbl <- getTyLbl v
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
  IR.Base "$$authorityarg" -> return $
    let v = IR.VarLocal (VN "$$authorityarg") in
    SimpleRawComp
    { cVal = RVar v
    , cValLbl = ValLbl v
    , cTyLbl = TyLbl v
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
  IR.Tuple vs ->
    return SimpleRawComp
      { cVal = RExpr $ Tuple vs
      , cValLbl = PC
      , cTyLbl = PC
      }
  IR.List vs ->
    return SimpleRawComp
      { cVal = RExpr $ List vs
      , cValLbl = PC
      , cTyLbl = PC
      }
  IR.Record fs ->
    return SimpleRawComp
      { cVal = RExpr $ Record fs
      , cValLbl = PC
      , cTyLbl = PC
      }

  -- The following two constructors extend an existing collection data structure
  -- with new values. The given labelled values are just added to the collection
  -- and not touched, therefore their labels are not joined into the datastructure's
  -- label.
  IR.ListCons v l -> do
    assertTypeAndRaise l RawList
    return SimpleRawComp
      { cVal = RUn l $ ListCons v
      , cValLbl = Join PC (ValLbl l) []
      , cTyLbl = PC
      }
  IR.WithRecord v fs -> do
    assertTypeAndRaise v RawRecord
    return SimpleRawComp
      { cVal = RUn v $ \r -> WithRecord r fs
      , cValLbl = Join PC (ValLbl v) []
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
  IR.ProjField v field -> do
    assertTypeAndRaise v RawRecord
    assertWithValAndRaise v $ \r -> AssertRecordHasField r field
    return ComplexRawComp
      { ccVal = RUn v $ \r -> ProjField r field
      , ccValLbl = \resValLbl -> Join PC (ValLbl v) [resValLbl]
      , ccTyLbl = \resTyLbl -> Join PC resTyLbl []
      }
  -- Revision 2023-08: 'ProjIdx' is the new indexing operation for tuples replacing
  -- the previous 'Index'. The difference is that the index is a constant to the operation
  -- instead of a variable. Previously, the type label of the tuple was incorrectly joined
  -- into the result type label.
  IR.ProjIdx v idx -> do
    assertTypeAndRaise v RawTuple
    assertWithValAndRaise v $ \r -> AssertTupleLengthGreaterThan r idx
    return ComplexRawComp
      { ccVal = RUn v $ \r -> ProjIdx r idx
      , ccValLbl = \resValLbl -> Join PC (ValLbl v) [resValLbl]
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
  IR.Bin op v1 v2 ->
    -- Basic binary op computation, where the value label is the join of those of the input and the type label is PC
    -- (for operations where the result type is fixed).
    let basicBinOpComp =
         return SimpleRawComp
          { cVal = RBin v1 v2 $ Bin op
          , cValLbl = Join PC (ValLbl v1) [ValLbl v2]
          , cTyLbl = PC
          }
        numBinOpComp = do
          assertTypeAndRaise v1 RawNumber
          assertTypeAndRaise v2 RawNumber
          basicBinOpComp
        stringBinOpComp = do
          assertTypeAndRaise v1 RawString
          assertTypeAndRaise v2 RawString
          basicBinOpComp
        -- Note: The result type is boolean in any case.
        numOrStringBinOpComp = do
          assertTypesBothStringsOrBothNumbers v1 v2
          basicBinOpComp

    in case op of
      Basics.Plus -> numBinOpComp
      Basics.Minus -> numBinOpComp
      Basics.Mult -> numBinOpComp
      -- Note: Division operations never result in an exception.
      -- Revision 2023-08: Removed incorrect raising of PC by value label
      -- of second operand (the operation always succeeds).
      Basics.Div -> numBinOpComp
      Basics.Mod -> numBinOpComp
      Basics.IntDiv -> numBinOpComp
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
        { ccVal = RBin v1 v2 $ Bin op
        , ccValLbl = \resValLbl -> Join PC (ValLbl v1) [ValLbl v2, resValLbl]
        , ccTyLbl = const PC -- The result type is always boolean
        }
      Basics.Neq -> return ComplexRawComp
        { ccVal = RBin v1 v2 $ Bin op
        , ccValLbl = \resValLbl -> Join PC (ValLbl v1) [ValLbl v2, resValLbl]
        , ccTyLbl = const PC -- The result type is always boolean
        }
      -- Revision 2023-08: Introduced new instruction InvalidateSparseBit
      -- (before this was called by a runtime raisedTo operation, which is now not necessary anymore).
      -- Otherwise equivalent except for order of instructions.
      Basics.RaisedTo -> do
        assertTypeAndRaise v2 RawLevel
        rRaiseTo <- getVal v2
        invalidateSparseBit
        return SimpleRawComp
          { cVal = RVar v1
          , cValLbl = Join PC (ValLbl v1) [ValLbl v2, Lbl rRaiseTo]
          , cTyLbl = Join PC (TyLbl v1) []
          }
      Basics.HasField -> do
        assertTypeAndRaise v1 RawRecord
        assertTypeAndRaise v2 RawString
        basicBinOpComp

      -- TODO Implement remaining operations
      _ -> error $ "Binary operation not yet implemented: " ++ show op

  IR.Un op v ->
    -- Basic unary op computation, where the value label is that of the input and the type label is PC
    -- (for operations where the result type is fixed).
    let basicUnOpComp =
         return SimpleRawComp
          { cVal = RUn v $ Un op
          , cValLbl = Join PC (ValLbl v) []
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
        assertTypeAndRaise v RawList
        basicUnOpComp
      Basics.TupleLength -> do
        assertTypeAndRaise v RawTuple
        basicUnOpComp
      -- Revision 2023-08: Equivalent.
      Basics.Tail -> do
        assertTypeAndRaise v RawList
        basicUnOpComp
      -- Revision 2023-08: Now also joining PC into value label (missed due to a typo in the previous version)
      Basics.Head -> do
        assertTypeAndRaise v RawList
        return ComplexRawComp
          { ccVal = RUn v $ Un op
          , ccValLbl = \resValLbl -> Join PC (ValLbl v) [resValLbl]
          , ccTyLbl = \resTyLbl -> Join PC resTyLbl []
          }
      -- Revision 2023-08: Now setting type label to PC instead of joining original type label (as the type is asserted).
      Basics.UnMinus -> do
        assertTypeAndRaise v RawNumber
        basicUnOpComp

      -- TODO Implement remaining operations
      _ -> error $ "Unary operation not yet implemented: " ++ show op


-- Revision 2023-08: Changed and moved handling of the complex operations Eq and Neq to expr2Raw.
-- | Generate raw instructions for the given IR instruction.
inst2raw :: IR.IRInst -> TM ()
inst2raw = \case
  -- Note: This is the only place where expressions occur in an IR program.
  IR.Assign vn expr -> do
    LVal{..} <- expr2raw expr
    -- Joining PC to be safe, even though for now PC is always joined when computing the expression (and it will be optimized away).
    rValLbl' <- compLabel $ Join PC (Lbl rValLbl) []
    rTyLbl' <- compLabel $ Join PC (Lbl rTyLbl) []
    constructLVal vn LVal{rValLbl = rValLbl', rTyLbl = rTyLbl', .. }

  IR.MkFunClosures vs env -> do
    -- The generation of closures and the related monitoring is first implemented in stack generation,
    -- to be able to use cyclic pointers for constructing the environments.
    tell [MkFunClosures vs env]


-- | Translate an IR terminator to a Raw terminator, generating instructions.
tr2raw :: IR.IRTerminator -> TM RawTerminator
tr2raw = \case
  -- Revision 2023-08: Equivalent except for the additional redundant raise.
  IR.TailCall v1 v2 -> do
    raisePCAndBlock $ ValLbl v1
    -- Note: The raise here is redundant because we have already raised by the value label above.
    -- However, optimizations aware of the relation between type- and value label will remove it.
    assertTypeAndRaise v1 RawFunction
    setR0 =<< getLVal v2
    TailCall <$> getVal v1 -- labels of v1 are dismissed at this point

  -- Revision 2023-08: This generates now more instructions than before,
  -- as we also construct LVals instead of just working on single RawVars,
  -- but after optimization with RawOpt the result is the same.
  IR.Ret v -> do
    -- At this point, we taint value and type label of the to-be-returned value with PC,
    -- as both value and type can depend on it. Note: when implementing more fine-grained type labels,
    -- the type label should not always be tainted here.
    setR0 =<< getLVal =<< pcTaint v
    return Ret

  IR.LibExport x ->
    return $ LibExport x

  -- Revision 2023-08: Equivalent except for the additional redundant raise.
  IR.If v bb1 bb2 -> do
    raisePCAndBlock $ ValLbl v
    bb1' <- tree2raw bb1
    bb2' <- tree2raw bb2
    -- Note: The raise here is redundant because we have already raised by the value label above.
    -- However, optimizations aware of the relation between type- and value label will remove it.
    assertTypeAndRaise v RawBoolean
    tell [ SetBranchFlag ]
    r <- getVal v
    return $ If r bb1' bb2'

  -- Revision 2023-08: Equivalent, only way of modifying bb2 changed.
  IR.Call v irBB1 irBB2 -> do
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
    return $ Call bb1 bb2

  -- Note: This is translated into branching and Error for throwing RT exception
  -- Revision 2023-08: More fine-grained raising of blocking label, see below.
  IR.AssertElseError v1 irBB verr pos -> do
    -- Note: We are first raising the blocking label with the type label,
    -- then assert the type and then raise with the value label.
    -- Raising with the value label directly would be sufficient,
    -- but with the two-step raising we obtain a more fine-grained
    -- blocking label for the assertion. In case more is know about
    -- value and type label, optimizations might eliminated the second raise.
    assertTypeAndRaise v1 RawBoolean
    raiseBlock $ ValLbl v1
    bb <- tree2raw irBB
    -- Generate the BB for the error case
    (tr_err, insts_err) <- intercept $ tr2raw $ IR.Error verr pos
    let bb_err = BB insts_err tr_err
    r <- getVal v1
    return $ If r bb bb_err

  -- Revision 2023-08: Now asserting that verr is a string. Also raising both PC
  -- and blocking label to the error message's value label (which will become
  -- obsolete with an improvement moving the arguments of the Raw.Error
  -- instruction to R0).
  IR.Error verr pos -> do
    -- Note: first raising block with type label and then with value label; see AssertElseError for explanation.
    assertTypeAndRaise verr RawString
    -- Note: There is no value label anymore at Raw level; instead join the label into PC (which e.g. determines whether are allowed to print the message)
    raisePCAndBlock $ ValLbl verr
    r <- getVal verr
    return $ Error r pos


-- Revision 2023-08: unchanged
-- | Translate an IR tree to a Raw tree (does not add any instructions to the monad).
tree2raw :: IR.IRBBTree -> TM RawBBTree
tree2raw (IR.BB irInsts irTr) = do
    -- Generate Raw instructions for the instructions of the block and the terminator.
    (tr, insts) <- intercept
            $ mapM_ inst2raw irInsts >> tr2raw irTr -- inst2raw only generates instructions without result value, tr2raw adds instructions and returns resulting RawTerminator
    return $ BB insts tr

-- Revision 2023-08: new code, but equivalent
fun2raw :: IR.FunDef -> FunDef
fun2raw irfdef@(IR.FunDef hfn vname consts (IR.BB irInsts irTr)) =
   FunDef hfn rawConsts (BB insts tr) irfdef
      where ((tr, rawConsts), insts) = evalRWS comp () 0
            comp = do
              -- Store the argument from R0 in the variable under which the argument is expected.
              r0 <- getR0
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

-- Revision 2023-08: unchanged
ir2raw :: IR.SerializationUnit -> RawUnit
ir2raw (IR.FunSerialization f) = FunRawUnit (fun2raw f)
ir2raw (IR.AtomsSerialization c) = AtomRawUnit c
ir2raw (IR.ProgramSerialization prog) = ProgramRawUnit (prog2raw prog)

-- Revision 2023-08: unchanged
prog2raw :: IR.IRProgram -> RawProgram
prog2raw (IR.IRProgram atoms funs) =
    RawProgram atoms (map fun2raw funs)


