{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}


module Raw2Stack (rawProg2Stack, rawFun2Stack, raw2Stack)
where

import InternalError (internalError)
import IR (SerializationUnit(..), HFN(..)
          , ppId, ppFunCall, ppArgs, Fields (..), Ident
          , serializeFunDef
          , serializeAtoms )
import qualified IR
import qualified Raw
import qualified Stack 
import qualified Data.Maybe as Maybe
import Data.Map.Lazy (Map,(!))
import qualified Data.Map.Lazy as Map 

import Data.Set(Set)
import qualified Data.Set as Set 

import qualified Basics
import qualified Core as C
import RetCPS(VarName(..))
import qualified RetCPS as CPS
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Base64 (encode,decode)
import TroupePositionInfo (Located(..), getLoc, unLoc, noLoc, PosInf(..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import           RetCPS (VarName (..))

import           IR ( Identifier(..)
                    , VarAccess(..), HFN (..), Fields (..), Ident
                    , ppId,ppFunCall,ppArgs
                    )

import RawDefUse

data TEnv = TEnv { defsUses :: DefUse, offsets :: OffsetMap, localCallDepth :: Int, __consts :: Raw.ConstMap }
type BlockNumber = Int 

type Tr = RWS TEnv () BlockNumber

getBlockNumber :: Tr BlockNumber 
getBlockNumber = get 


setBlockNumber :: BlockNumber -> Tr ()
setBlockNumber = put


frameOverhead = 5

offsetWithCallDepth = do 
  __callDepth <- localCallDepth <$> ask 
  __offsets <- offsets <$> ask 
  let frameSize = Map.size __offsets
  let rel i = i - if __callDepth == 0 
                      then 0
                      else __callDepth * frameOverhead + (frameSize + 1)
  return rel 
      


-- | Translate Located Raw instructions to Located Stack instructions
trInsts :: [Raw.LRawInst] -> Tr [Stack.LStackInst]
trInsts ii = work [] [] ii  where
  -- Translate one regular instruction (non-label-specific)
  trOneRegInst :: Raw.LRawInst -> Tr [Stack.LStackInst]
  trOneRegInst li = do
    __offsets <- offsets <$> ask
    rel <- offsetWithCallDepth
    let pos = getLoc li
    let i = unLoc li
    let store a =
            case Map.lookup a __offsets of
                Nothing -> []
                Just j ->  [Loc pos (Stack.StoreStack a (rel j))]
    case i of
      Raw.AssignRaw x e -> return $
        (Loc pos (Stack.AssignRaw Stack.AssignConst x e)):(store (Raw.AssignableRaw x))
      Raw.AssignLVal x e -> return $
        (Loc pos (Stack.AssignLVal x e)):(store (Raw.AssignableLVal x))
      Raw.SetState cmp x -> return [Loc pos (Stack.SetState cmp x)]
      Raw.SetBranchFlag -> return [Loc pos Stack.SetBranchFlag]
      Raw.InvalidateSparseBit -> return [Loc pos Stack.InvalidateSparseBit]
      Raw.MkFunClosures envmap vars -> do
        let stores = concat $ map (\v -> store (Raw.AssignableLVal v)) (fst (unzip vars))
        return $ (Loc pos (Stack.MkFunClosures envmap vars)):stores
      Raw.RTAssertion a -> return [Loc pos (Stack.RTAssertion a)]
      Raw.SourcePosAnnotation r -> return [Loc pos (Stack.SourcePosAnnotation r)]

  translateGroup [] = return []
  translateGroup linsts = do
    rr <- ask
    rel <- offsetWithCallDepth
    let __uses = (uses.defsUses) rr
        __defs = (defs.defsUses) rr
        __offsets = offsets rr

        filteredUsesOf f x =
          let x' = Raw.AssignableRaw x
              loc_def = case Map.lookup x' __defs of
                            Nothing-> internalError $ "cannot find " ++ (show x')
                            Just w -> w
              x_uses_set = Map.findWithDefault Set.empty x' __uses
          in Set.filter (f loc_def) x_uses_set


        escapingUses = filteredUsesOf $
                 \(c_def, _) ( c_use, _) -> c_use > c_def

        outsideGroupUses = filteredUsesOf $
                 \(c_def, z_def) ( c_use, z_use) -> c_use > c_def || z_use /= z_def

        isGroupEscaping x = 0 < Set.size ( outsideGroupUses x )
        isBlockEscaping x = 0 < Set.size ( escapingUses x )

        -- Extract var and position from Located Raw instructions
        assignVarsWithPos = concat $ map assignVar linsts
                 where
                   assignVar li = case unLoc li of
                     Raw.AssignRaw x _ -> [(x, getLoc li)]
                     _ -> []

        assignVars = map fst assignVarsWithPos

        prologue = [ Loc pos (Stack.AssignRaw Stack.AssignLet x (Raw.ProjectState Raw.MonPC))
                       | (x, pos) <- assignVarsWithPos,
                         isGroupEscaping x ]


        epilogue = [ Loc pos (Stack.StoreStack x' (rel j))
                       | (x, pos) <- assignVarsWithPos
                       , isBlockEscaping x
                       , let x' = Raw.AssignableRaw x
                       , let j = case Map.lookup x' __offsets of
                                          Nothing -> internalError $ "epilogue: cannot find " ++ (show x')
                                          Just w -> w
                   ]

        -- Translate Located instruction, preserving position
        tri li =
          let pos = getLoc li
          in case unLoc li of
                  Raw.AssignRaw x y ->
                      let t = if isGroupEscaping x then Stack.AssignMut
                                                   else Stack.AssignConst
                      in Loc pos (Stack.AssignRaw t x y)
                  Raw.SetState cmp x -> Loc pos (Stack.SetState cmp x)
                  _ -> internalError "impossible case/bug: only label instructions must be passed to this translation function"

        -- Get position for the group (use first instruction's position)
        -- Note: linsts is non-empty here due to the guard at translateGroup []
        groupPos = case linsts of
          (li:_) -> getLoc li
          _ -> NoPos  -- unreachable, but needed for exhaustiveness

        insts' = Loc groupPos (Stack.LabelGroup (map tri linsts))

    return $ prologue ++ (insts' : epilogue )

  work accum group ii = do
    case ii of
      [] -> do
              gg <- translateGroup group
              return $ accum ++ gg
      (linst:linsts) -> do
        if instructionType (unLoc linst) == LabelSpecificInstruction
          then
            case group of
              [] -> work accum ([linst]) linsts
              jj -> work accum ((jj ++ [linst])) linsts
          else do
            ii' <- trOneRegInst linst
            case group of
              [] -> work (accum ++ ii') [] linsts
              jj -> do
                  gg <- translateGroup jj
                  work (accum ++ gg ++ ii') [] linsts 

           

-- | Translate Located RawTerminator to Located StackTerminator
trTr :: Raw.LRawTerminator -> Tr Stack.LStackTerminator
trTr ltr = do
  let pos = getLoc ltr
  let tr = unLoc ltr
  case tr of
    Raw.TailCall r -> return $ Loc pos (Stack.TailCall r)
    Raw.Ret -> return $ Loc pos Stack.Ret
    Raw.If r bb1 bb2 -> do
         bb1' <- trBB bb1
         bb2' <- trBB bb2
         return $ Loc pos (Stack.If r bb1' bb2')
    Raw.LibExport v -> return $ Loc pos (Stack.LibExport v)
    Raw.Error r1 -> return $ Loc pos (Stack.Error r1)
    Raw.StackExpand bb1 bb2 -> do
       __callDepth <- localCallDepth <$> ask
       bb1' <- local (\tenv -> tenv { localCallDepth = __callDepth + 1 } ) $ trBB bb1
       n <- getBlockNumber
       let n' = n + 1
       setBlockNumber n'
       varsToLoad <-
         (Map.findWithDefault Set.empty n').escapingUses.defsUses <$> ask
       offsets <- offsets <$> ask
       rel <- offsetWithCallDepth
       consts <- __consts <$> ask
       let filterConsts (Raw.AssignableRaw x) = Map.notMember x consts
           filterConsts _ = True
       let loads = [ Loc pos (Stack.FetchStack x (rel (Map.findWithDefault (internalError ("no stack offset for " ++ show x)) x offsets)))
                        | x <-  filter filterConsts (Set.elems varsToLoad) ]
       bb2'@(Stack.BB inst_2 tr_2) <- trBB bb2

       return $ Loc pos (Stack.StackExpand bb1' (Stack.BB (loads ++ inst_2) tr_2))


trBB :: Raw.RawBBTree -> Tr Stack.StackBBTree 
trBB (Raw.BB insts tr) = do
  insts' <- trInsts insts
  tr'    <- trTr tr 
  return $ Stack.BB insts' tr'


-- | Translate Located FunDef from Raw to Stack
trFun :: Raw.LFunDef -> Stack.LFunDef
trFun lfdef =
  let pos = getLoc lfdef
      fdef@(Raw.FunDef hfn consts bb ir) = unLoc lfdef
      defUseInfo = defUse fdef
      constMap = Map.fromList consts
      offsets = offsetMap constMap defUseInfo

      env = TEnv { defsUses = defUseInfo
                 , offsets = offsets
                 , localCallDepth = 0
                 , __consts = constMap
                 }
      (bb', _, _) = runRWS (trBB bb) env 0
      Stack.BB insts bb_ = bb'
      insts_ = case Map.lookup Raw.Env offsets of
                     Nothing -> insts
                     Just ee  -> (Loc NoPos (Stack.StoreStack Raw.Env ee)) : insts
      frameSize = Map.size offsets
  in Loc pos (Stack.FunDef hfn frameSize consts (Stack.BB insts_ bb_) ir)


rawProg2Stack :: Raw.RawProgram -> Stack.StackProgram
rawProg2Stack (Raw.RawProgram atms fdefs) =
  Stack.StackProgram atms (map trFun fdefs)


rawFun2Stack :: Raw.LFunDef -> Stack.LFunDef
rawFun2Stack = trFun

raw2Stack :: Raw.RawUnit -> Stack.StackUnit
raw2Stack r = case r of
  Raw.FunRawUnit f -> Stack.FunStackUnit (trFun f)
  Raw.AtomRawUnit c -> Stack.AtomStackUnit c
  Raw.ProgramRawUnit p -> Stack.ProgramStackUnit (rawProg2Stack p)