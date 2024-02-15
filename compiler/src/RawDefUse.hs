{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RawDefUse (offsetMap
                 , defUse
                 , DefUse (..)
                 , OffsetMap(..)
                 , InstructionType (..)
                 , instructionType
                 , iDefUse
                 ) where

import Raw  
import IR (SerializationUnit(..), HFN(..)
          , ppId, ppFunCall, ppArgs, Fields (..), Ident
          , serializeFunDef
          , serializeAtoms )
import qualified IR           
import qualified Stack 
import qualified Data.Maybe as Maybe
import Data.Map.Lazy (Map, (!))
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

type CallLocation = Int
type ZoneLocation = Int 
type Location = (CallLocation, ZoneLocation) -- calls, zones


data DefUse = DefUse
  { 
    defs :: Map Assignable Location ,
    uses :: Map Assignable (Set Location),
    escapingUses :: Map CallLocation (Set Assignable) -- map from basic blocks to the assignables        
  }


data TraverseState a = TraverseState 
  { 
    defUseMaps :: a,
    locInfo :: Location ,
    nCalls :: Int
  }


data DefUseOps a = DefUseOps 
  { 
    __insertUse :: Assignable -> TraverseState a -> TraverseState a,
    __insertDef :: Assignable -> TraverseState a -> TraverseState a
  }


-- | The traversal monad for the Def-Use analysis.
type UseDefTraversal a = RWS (DefUseOps a) () (TraverseState a)
type Tr = UseDefTraversal DefUse 


type OffsetMap = Map Assignable Int
offsetMap :: ConstMap -> DefUse -> OffsetMap
offsetMap consts (DefUse { defs, uses, escapingUses }) = 
  let isEscaping x (c_def, _) = 
        let uses_set = Map.findWithDefault (Set.empty) x uses 
            uses' = Set.filter ( \(c,_) -> c > c_def) uses_set
        in (0 < Set.size uses') && (case x of AssignableRaw  r -> Map.notMember r consts
                                              _ -> True  )
      map' = Map.filterWithKey isEscaping defs 
      escaping = Map.keys map' 
  in Map.fromList $ zip escaping [0..(-1 + Map.size map')] 



class Definable a b where 
  define :: a -> UseDefTraversal b ()

class Usable a b where
  use :: a -> UseDefTraversal b ()



__insertUsePure x state = 
    let defsUses = defUseMaps state 
        useMap = uses defsUses
        defMap = defs defsUses
        escUse = escapingUses defsUses 
        block@(c_use,_) = locInfo state
        (c_def, _) = 
          case Map.lookup x defMap of 
                 Nothing -> error $ "insert use: cannot find " ++ (show x)
                 Just w -> w 
        currentUses = Map.findWithDefault (Set.empty) x useMap
        currentEsc = Map.findWithDefault (Set.empty) (fst block) escUse
        newUse = Set.insert block currentUses
        newEsc = if c_def < c_use then Set.insert x currentEsc
                                  else currentEsc
    in  state { 
            defUseMaps = 
              defsUses {
                uses = Map.insert x newUse useMap, 
                escapingUses = Map.insert (fst block) newEsc escUse
        }} 


__insertDefPure x state = 
  let defsUses = defUseMaps state 
      defMap = defs defsUses
      block = locInfo state
  in
    if Map.member x defMap 
      then error $ "Duplicate bindings for " ++ (show x)
      else  state { 
                  defUseMaps = 
                    defsUses {
                      defs = Map.insert x block defMap}} 



instance Definable RawVar b where 
  define x = do 
    f <- __insertDef <$> ask 
    modify $ f (AssignableRaw x)


instance Definable VarName b where 
  define x = do
    f <- __insertDef <$> ask
    modify $ f (AssignableLVal x)


instance Usable Assignable b where
  use x = do 
     f <- __insertUse <$> ask
     modify (f x)


instance Definable a b => Definable [a] b where
  define = mapM_ define 


instance Usable RawVar b where
  use x = do 
     insertUse <- __insertUse <$> ask 
     modify (insertUse (AssignableRaw x))


instance Usable VarName b where 
  use x = do 
    insertUse <- __insertUse <$> ask 
    modify $ insertUse (AssignableLVal x)


instance Usable a b => Usable [a] b
  where use x = mapM_ use x


instance Usable VarAccess b where
  use (VarLocal x) = use x
  use _ = use Env


instance Trav a => Trav [a]
  where trav = mapM_ trav


instance Usable RawExpr b where 
  use e =     
      case e of 
        Raw.Bin _ x y -> use [x,y] 
        Raw.Un _ x -> use x
        Raw.ProjectLVal x _ -> use x
        Raw.ProjectState _ -> return ()
        Raw.Tuple xs -> use xs 
        Raw.Record fields -> use (snd (unzip fields))
        Raw.WithRecord x fields -> do 
          use x 
          use (snd (unzip fields))
        Raw.ProjField x _ -> use x
        Raw.ProjIdx x _ -> use x
        Raw.List xs -> use xs
        Raw.ListCons x y -> use x >> use y
        Raw.Const _ -> return ()
        Raw.Lib _ _ -> return ()
        Raw.Base _ -> return ()    
        Raw.ConstructLVal x y z -> do use x 
                                      use [y,z]


clearZone = do 
  (c,z) <- getLocation
  if z `mod` 2 /= 0 then 
    setLocation (c, z + 1)
  else 
    return ()

instance Trav RawTerminator where 
  trav tr = 
     case tr of 
       TailCall r -> use r 
       Ret -> return () 
       If r bb1 bb2 -> do 
         (c, z) <- getLocation 
         use r 
         setLocation $ (c, z + 2) 
         trav bb1 
         setLocation $ (c, z + 2) 
         trav bb2
       LibExport v -> use v 
       Error r _ -> use r 
       Call bb1 bb2 -> do
         trav bb1
         modify (\s -> 
                     let (c, _) = locInfo s 
                         n =  1 + nCalls s                         
                     in s { locInfo = (n, 0), nCalls = n })
         trav bb2 


getLocation :: UseDefTraversal b Location
getLocation = locInfo <$> get

setLocation b = modify (\st -> st {locInfo = b})


-- Instructions in the basic blocks are partitioned into so-called 
-- zones, a zone is a natural number. Even zones correspond to regular 
-- instructions, e.g., an arithmetic plus while odd zones correspond 
-- to label operation such as setting a pc. 

-- The idea behind the zones is that instructions within the same
-- label zone may be shortcutted if the current pc bounds the 
-- maximum amount of information that may be accessed in the function 


updateZone :: RawInst -> UseDefTraversal b ()
updateZone i = do 
  (blockCounter, zoneCounter) <- getLocation
  let zoneType = zoneCounter `mod` 2 == 0 
      typeAsBool LabelSpecificInstruction = False 
      typeAsBool _ = True 
  if (typeAsBool.instructionType) i  /= zoneType then 
      setLocation ( blockCounter, zoneCounter + 1 )
  else return ()


-- | Def-Use analysis: mark used variables
instance Usable RawInst b where 
  use i = do 
     updateZone i
     case i of 
       AssignRaw x e -> use e 
       AssignLVal x e -> use e 
       SetState cmp x -> use x 
       RTAssertion (AssertType r _) -> use r
       --  RTAssertion (AssertEqTypes _ x y) -> use [x,y]
       RTAssertion (AssertTypesBothStringsOrBothNumbers x y) -> use [x,y]
       RTAssertion (AssertRecordHasField r _) -> use r
       RTAssertion (AssertTupleLengthGreaterThan r _) -> use r
       MkFunClosures xs _ -> use (snd (unzip xs))
       -- Instructions without variables
       InvalidateSparseBit -> return ()
       SetBranchFlag -> return ()


-- | Mark variables that are defined.
instance Definable RawInst b where 
  define i = do 
    updateZone i 
    case i of 
       AssignRaw x _ -> define x
       AssignLVal x _ -> define x
       SetState cmp x -> return ()
       RTAssertion _ -> return ()
       SetBranchFlag -> return ()
       InvalidateSparseBit -> return ()
       MkFunClosures _ ys -> mapM_ define (fst (unzip ys))


instance Trav RawBBTree where 
  trav (BB ii tr) = do 
    clearZone
    (blockCounter, zz) <- getLocation    
    mapM_ define ii
    setLocation (blockCounter, zz) -- reset
    mapM_ use ii 
    trav tr


class Trav a where 
   trav :: a -> Tr ()


defUse :: FunDef -> DefUse 
defUse (FunDef _ consts bb _) = 
  let constVars = ( fst . unzip )consts 
      insertConsts = mapM define constVars 
      (defUse, _) = execRWS 
        (modify (__insertDefPure Env) >> insertConsts >> trav bb)
        DefUseOps { 
                __insertUse = __insertUsePure,
                __insertDef = __insertDefPure
              }
        (TraverseState 
            {defUseMaps = DefUse 
                           { defs = Map.empty,
                             uses = Map.empty, 
                             escapingUses = Map.empty
                             },
            locInfo = (0,0),
            nCalls = 0
            })
  in defUseMaps defUse


iDefUse :: RawInst -> (Set Assignable, Set Assignable)
iDefUse i = 
  let go = do 
            -- insDef <- __insertDef <$> ask 
            -- modify $  insDef Env             
            define i
            use i 
      (defUse, _) = execRWS go 
        (DefUseOps {
            __insertDef = \x state -> 
                            let (inserts,uses) = defUseMaps state 
                            in state {defUseMaps = (Set.insert x inserts, uses)},
            __insertUse = \x state -> 
                            let (inserts, uses) = defUseMaps state 
                            in state {defUseMaps = (inserts, Set.insert x uses)}
        })
        (TraverseState {
             defUseMaps = (Set.empty, Set.empty),
             locInfo = (0, 0),
             nCalls = 0
        })
  in defUseMaps defUse    
     
         

  
