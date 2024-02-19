{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}


module Raw2Stack (rawProg2Stack, rawFun2Stack, raw2Stack)
where

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
import CompileMode
import TroupePositionInfo
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
      


trInsts :: [Raw.RawInst] -> Tr [Stack.StackInst]
trInsts ii = work [] [] ii  where
  trOneRegInst :: Raw.RawInst -> Tr [Stack.StackInst]
  trOneRegInst i = do 
    __offsets <- offsets <$> ask  
    rel <- offsetWithCallDepth 
    let store a =
            case Map.lookup a __offsets of
                Nothing -> []
                Just i ->  [Stack.StoreStack a (rel i)]
    case i of 
      Raw.AssignRaw x e -> return $
        (Stack.AssignRaw Stack.AssignConst x e):(store (Raw.AssignableRaw x))
      Raw.AssignLVal x e -> return $ 
        (Stack.AssignLVal x e):(store (Raw.AssignableLVal x))            
      Raw.SetState cmp x -> return [Stack.SetState cmp x]
      Raw.SetBranchFlag -> return [Stack.SetBranchFlag]
      Raw.InvalidateSparseBit -> return [Stack.InvalidateSparseBit]
      Raw.MkFunClosures envmap vars -> do 
        let stores = concat $ map (store . Raw.AssignableLVal) (fst (unzip vars))      
        return $ (Stack.MkFunClosures envmap vars):stores
      Raw.RTAssertion a -> return [Stack.RTAssertion a]

  translateGroup [] = return []
  translateGroup insts = do 
    rr <- ask 
    rel <- offsetWithCallDepth
    let __uses = (uses.defsUses) rr
        __defs = (defs.defsUses) rr 
        __offsets = offsets rr 

        filteredUsesOf f x = 
          let x' = Raw.AssignableRaw x 
              loc_def = case Map.lookup x' __defs of
                            Nothing-> error $ "cannot find " ++ (show x')
                            Just w -> w
              x_uses_set = Map.findWithDefault Set.empty x' __uses
          in Set.filter (f loc_def) x_uses_set


        escapingUses = filteredUsesOf $ 
                 \(c_def, _) ( c_use, _) -> c_use > c_def

        outsideGroupUses = filteredUsesOf $ 
                 \(c_def, z_def) ( c_use, z_use) -> c_use > c_def || z_use /= z_def

        isGroupEscaping x = 0 < Set.size ( outsideGroupUses x )
        isBlockEscaping x = 0 < Set.size ( escapingUses x )

        assignVars = concat $ map assignVar insts
                 where 
                   assignVar i = case i of 
                     Raw.AssignRaw x _ -> [x]
                     _ -> []
        

        prologue = [ Stack.AssignRaw Stack.AssignLet x (Raw.ProjectState Raw.MonPC) 
                       | x <- assignVars,
                         isGroupEscaping x ]

        
        epilogue = [ Stack.StoreStack x' (rel j)
                       | x <- assignVars
                       , isBlockEscaping x
                       , let x' = Raw.AssignableRaw x
                       , let j = case Map.lookup x' __offsets of 
                                          Nothing -> error $ "epilogue: cannot find " ++ (show x')
                                          Just w -> w 
                   ]
                    
        tri i = case i of 
                  Raw.AssignRaw x y -> 
                      let t = if isGroupEscaping x then Stack.AssignMut 
                                                   else Stack.AssignConst 
                      in Stack.AssignRaw t x y
                  Raw.SetState cmp x -> Stack.SetState cmp x 
                  _ -> error "impossible case/bug: only label instructions must be passed to this translation function"

        insts' = Stack.LabelGroup $ map tri insts 
        
    return $ prologue ++ (insts' : epilogue )

  work accum group ii = do
    case ii of 
      [] -> do 
              gg <- translateGroup group 
              return $ accum ++ gg 
      (inst:insts) -> do
        if instructionType inst == LabelSpecificInstruction 
          then
            case group of 
              [] -> work accum ([inst]) insts 
              jj -> work accum ((jj ++ [inst])) insts 
          else do 
            ii' <- trOneRegInst inst 
            case group of 
              [] -> work (accum ++ ii') [] insts 
              jj -> do 
                  gg <- translateGroup jj 
                  work (accum ++ gg ++ ii') [] insts 

           

trTr :: Raw.RawTerminator -> Tr Stack.StackTerminator
trTr (Raw.TailCall r) = do 
     return $ Stack.TailCall r
trTr Raw.Ret = return Stack.Ret 
trTr (Raw.If r bb1 bb2) = do 
     bb1' <- trBB bb1 
     bb2' <- trBB bb2 
     return $ Stack.If r bb1' bb2' 
trTr (Raw.LibExport v) = do 
     return $ Stack.LibExport v 
trTr (Raw.Error r1 p) = do 
     return $ Stack.Error r1 p 
trTr (Raw.Call bb1 bb2) = do 
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
   let loads = [ Stack.FetchStack x (rel (Map.findWithDefault (error (show x)) x offsets))
                    | x <-  filter filterConsts (Set.elems varsToLoad) ]   
   bb2'@(Stack.BB inst_2 tr_2) <- trBB bb2

   return $ Stack.Call bb1' (Stack.BB (loads ++ inst_2) tr_2)


trBB :: Raw.RawBBTree -> Tr Stack.StackBBTree 
trBB (Raw.BB insts tr) = do
  insts' <- trInsts insts
  tr'    <- trTr tr 
  return $ Stack.BB insts' tr'


trFun :: Raw.FunDef -> Stack.FunDef
trFun fdef@(Raw.FunDef hfn consts bb ir) = 
  let defUseInfo = defUse fdef 
      constMap = Map.fromList consts
      offsets = offsetMap constMap defUseInfo
      
      env = TEnv { defsUses = defUseInfo
                 , offsets = offsets
                 , localCallDepth = 0
                 , __consts = constMap 
                 }
      (bb', _, _) =runRWS (trBB bb) env 0
      Stack.BB insts bb_ = bb' 
      insts_ = case Map.lookup Raw.Env offsets of 
                     Nothing -> insts 
                     Just ee  -> (Stack.StoreStack Raw.Env ee) :insts
      frameSize = Map.size offsets
  in Stack.FunDef hfn frameSize consts (Stack.BB insts_ bb_) ir


rawProg2Stack :: Raw.RawProgram -> Stack.StackProgram
rawProg2Stack (Raw.RawProgram atms fdefs)  =
  Stack.StackProgram atms (map trFun fdefs)


rawFun2Stack = trFun 

raw2Stack :: Raw.RawUnit -> Stack.StackUnit 
raw2Stack r = case r of 
  Raw.FunRawUnit f -> Stack.FunStackUnit (trFun f)
  Raw.AtomRawUnit c -> Stack.AtomStackUnit c 
  Raw.ProgramRawUnit p -> Stack.ProgramStackUnit (rawProg2Stack p)