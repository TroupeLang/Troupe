{-- 
Translation from Stack to JS code.
The names of most runtime functions are specified at the respective place here.
However, those for 'RTAssertion' are defined via 'ppRTAssertion'.

TODO
- Port the code for serialization (AA; 2020-12-04)

--}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Stack2JS where
-- import qualified IR2JS 

import IR (SerializationUnit(..), HFN(..)
          , ppFunCall, ppArgs, Fields (..), Ident
          , serializeFunDef
          , serializeAtoms )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified IR
import qualified Raw

import Raw (RawExpr (..), RawType(..), RawVar (..), MonComponent(..), RTAssertion(..),
            ppRawExpr, ppRTAssertionCode)

import Stack

import qualified Basics
import           Basics(BinOp(..), UnaryOp(..))
import qualified Core as C
import           Core (ppLit)
import           RetCPS(VarName(..))
import qualified RetCPS as CPS
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Data.List
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Base64 (encode,decode)
import           CompileMode
import           TroupePositionInfo
import qualified Data.Aeson as Aeson
import           GHC.Generics (Generic)


import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import Data.Aeson (ToJSON(toJSON), Value)
import DCLabels (dcLabelExpToDCLabel)


data LibAccess = LibAccess Basics.LibName Basics.VarName
   deriving (Eq, Show,Generic)


data JSOutput = JSOutput { libs :: [LibAccess] 
                         , fname:: Maybe String 
                         , code :: String 
                         , atoms :: [Basics.AtomName]
                         } deriving (Show, Generic)

instance Aeson.ToJSON Basics.LibName 
instance Aeson.ToJSON LibAccess
instance Aeson.ToJSON JSOutput

ppLibAccess :: LibAccess -> PP.Doc
ppLibAccess (LibAccess (Basics.LibName libname) varname) = PP.braces $
  PP.text "lib:" <+> (PP.doubleQuotes. PP.text) libname <+> PP.text "," <+>
  PP.text "decl:" <+> (PP.doubleQuotes. PP.text) varname


ppLibs :: [LibAccess] -> PP.Doc
ppLibs libs = PP.brackets $
                vcat $ PP.punctuate (text ",")
                  $ map ppLibAccess (nub libs)

jsLoadLibs = vcat $ map text [
  "this.libSet = new Set ()",
  "this.libs = []",
  "this.addLib = function (lib, decl) { if (!this.libSet.has (lib +'.'+decl)) { this.libSet.add (lib +'.'+decl); this.libs.push ({lib:lib, decl:decl})} }",
  "this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) }" ]
                            
      
addOneLib (LibAccess (Basics.LibName libname) varname) =
  let args = (PP.doubleQuotes.PP.text) libname <+> text "," <+> (PP.doubleQuotes. PP.text) varname
  in text "this.addLib " <+> PP.parens args

addLibs xs = vcat $ nub (map addOneLib xs)


data TheState = TheState { freshCounter :: Integer
                         , frameSize    :: Int
                         , sparseSlot    :: Int
                         , consts       :: Raw.Consts
                         , stHFN        :: IR.HFN }

type RetKontText = PP.Doc

type W = RWS Bool  ([LibAccess], [Basics.AtomName], [RetKontText]) TheState


initState = TheState { freshCounter = 0
                     , frameSize = error "frameSize should not be accessed yet"
                     , sparseSlot = error "sparseSlot should not be accessed yet"
                     , consts = error "consts should not be accessed yet"
                     , stHFN = error "stHFN should not be accessed yet"
                     }

a $$+ b  = a $$ (nest 2 b)



class Identifier a where
  ppId :: a ->  PP.Doc


instance Identifier VarName where
  ppId = IR.ppVarName

-- instance Identifier IR.VarAccess where
--   ppId = IR.ppVarAccess

instance Identifier HFN where
  ppId (HFN n) = text n

instance Identifier Basics.LibName where 
  ppId (Basics.LibName s) = text s

instance Identifier Basics.AtomName where 
  ppId = text

instance Identifier RawVar where 
  ppId (RawVar x) = text x

instance Identifier Raw.Assignable where 
  ppId (Raw.AssignableRaw x) = ppId x 
  ppId (Raw.AssignableLVal x) = ppId x 
  ppId (Raw.Env) = text "$env"

-- | Translation monad collecting the generated JS parts when passing through the 'StackProgram' tree.
class ToJS a where
   toJS :: a -> W PP.Doc



irProg2JSString :: CompileMode -> Bool -> StackProgram -> String
irProg2JSString compileMode debugOut ir =
  let (fns, _, (_,_,konts)) = runRWS (toJS ir) debugOut initState
      inner = vcat (fns:konts)
      outer = vcat $
        stdlib
        ++
        [ "function" <+> ppNamespaceName <+> text "(rt) {" ]
        ++
        [ nest 2 inner
        , text "}" ]
        ++
        suffix
  in      
    PP.render $
      case compileMode of
         Normal -> outer
         Export -> inner


stack2JSString :: StackUnit -> String
stack2JSString x =
  let (inner, _, (libs, atoms, konts)) = runRWS (toJS x) False initState
  in PP.render (addLibs libs $$ (vcat (inner:konts)))



stack2JSON :: StackUnit -> ByteString
stack2JSON (ProgramStackUnit _) = error "needs to be ported"
stack2JSON x = 
  let (inner, _, (libs, atoms, konts)) = runRWS (toJS x) False initState
  in Aeson.encode $ JSOutput { libs = libs
                             , fname = case x of FunStackUnit (FunDef (HFN n)_ _ _ _) -> Just n
                                                 _ -> Nothing
                             , atoms = atoms                              
                             , code = PP.render (addLibs libs $$ (vcat (inner:konts))) 
                             } 


instance ToJS StackUnit where
  toJS (FunStackUnit fdecl) = toJS fdecl
  toJS (AtomStackUnit ca) = toJS ca
  toJS (ProgramStackUnit p) = error "not implemented"

instance ToJS IR.VarAccess where 
  toJS (IR.VarLocal vn) = return $ IR.ppVarName vn 
  toJS (IR.VarEnv vn) = return $ text "$env." PP.<> (IR.ppVarName vn)
  toJS (IR.VarFunSelfRef) = do 
    HFN (fname) <- gets stHFN 
    return $ text fname 


-- instance (Identifier a) => ToJS a where 
--   toJS x = return $ ppId x

ppNamespaceName = text "Top"  -- should be generating a new namespace per received blob


irProg2JsWrapped prog = do
    inner <- toJS prog
    return $
       text "function" <+> ppNamespaceName <+> text "(rt) {"
       $$ nest 2 inner
       $$ text "}"



instance ToJS StackProgram where
  toJS (StackProgram atoms funs) = do
     jjA <- toJS atoms
     (jjF, (libsF, atoms', _)) <- listen $ mapM toJS funs
     
     return $
          vcat $ [ jsLoadLibs
                 , addLibs libsF
                 , jjA
                 ] ++ jjF

          


instance ToJS C.Atoms where
  toJS catoms@(C.Atoms atoms) = return $
    vcat [ vcat $ (map  (\a -> hsep ["const"
                                    , text a
                                    , "= new rt.Atom"
                                                  , (PP.parens ( (PP.doubleQuotes.text) a))]) atoms)
         , text "this.serializedatoms =" <+> (pickle.serializeAtoms) catoms]


jsonValueToString :: Value -> String
jsonValueToString val = BL.unpack (Aeson.encode val)

lit2JS C.LUnit = text "rt.__unitbase"
lit2JS (C.LLabel s) = text "rt.mkV1Label" <> (PP.parens . PP.doubleQuotes) (text s)
lit2JS (C.LDCLabel dc) = 
  text "rt.mkDCLabel" <> (PP.parens.text.jsonValueToString.toJSON.dcLabelExpToDCLabel) dc 

lit2JS lit = ppLit lit

constsToJS consts = 
     vcat $ map toJsConst consts 
               where toJsConst (x,lit) = hsep ["const", ppId x , text "=", lit2JS lit ]

instance ToJS FunDef where 
    toJS fdef@(FunDef hfn stacksize consts bb irfdef) = do
       {--
          |  |  | ... | <sparse slot> | 
          ^           ^
          |           |
          SP          stacksize 
       
       --}       
       let _frameSize = stacksize + 1 

       modify (\s -> s { frameSize = _frameSize, sparseSlot = stacksize, stHFN = hfn, consts = consts } ) -- + 1 for the sparse flag; 2021-03-17; AA
       let lits = constsToJS consts
       jj <- toJS bb
       debug <- ask
       let (irdeps, libdeps, atomdeps ) = IR.ppDeps irfdef
       sparseSlotIdxPP <- ppSparseSlotIdx

       return $
          vcat [text "this." PP.<>  ppId hfn <+> text "=" <+> ppArgs ["$env"] <+> text "=> {"
               , if debug then nest 2 $ text "rt.debug" <+> (PP.parens . PP.doubleQuotes.  ppId) hfn
                          else PP.empty 
               , nest 2 $ vcat $ [ 
                  "let _T = rt.runtime.$t",
                  "let _STACK = _T.callStack",
                  "let _SP = _T._sp",
                  "let _SP_OLD",
                  -- Update sparse bit at function entry:
                  -- Check whether environment's data level, and the label and data level of R0 are bound by PC.
                  -- Requires sparseSlot to be updated first.
                  "_T.sparseSlot = " <+> sparseSlotIdxPP,
                  "_T.updateSparseBitOnEntry($env.__dataLevel)",
                  lits,
                  jj]
               , text "}"
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".deps =" <+> irdeps
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".libdeps =" <+> libdeps
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".serialized =" <+> (pickle.serializeFunDef) irfdef
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".framesize =" <+> (PP.int stacksize) ]



instance ToJS StackBBTree where 
--  toJS = bb2js

    toJS (BB ins tr) = do
      jj  <- mapM toJS ins
      j'  <- toJS tr
      return $ vcat $ jj ++ [j']


instance ToJS StackInst where
  toJS = ir2js

instance ToJS StackTerminator where 
  toJS = tr2js

binOpToJS :: BinOp -> Raw.UseNativeBinop ->  String
binOpToJS op (Raw.UseNativeBinop isNative) = case op of 
    -- JS binary operators (some not implemented in IR2Raw)
    Plus -> "+"
    Minus -> "-"
    Mult -> "*"
    Div -> "/"
    Mod -> "%"
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"
    And -> "&&"
    Or -> "||"
    BinAnd -> "&"
    BinOr -> "|"
    BinXor -> "^"
    BinShiftLeft -> "<<"
    BinShiftRight -> ">>"
    BinZeroShiftRight -> ">>>"
    -- Functions defined in UserRuntimeZero.ts
    IntDiv -> "rt.intdiv"
    Eq -> if isNative then "===" else "rt.eq"
    Neq -> if isNative then "!==" else "rt.neq"
    Concat -> "+"
    HasField -> "rt.hasField"
    LatticeJoin -> "rt.raw_join"
    -- No RT operations (should be moved to a different datatype)
    RaisedTo -> error "Not a runtime operation"
    -- Not yet implemented in IR2Raw
    FlowsTo -> error "Not yet implemented: FlowsTo" -- (implemented in tagsets.ts: "rt.flowsTo")
    LatticeMeet -> error "Not yet implemented: LatticeMeet"

unaryOpToJS :: UnaryOp -> String
unaryOpToJS = \case
    -- Functions defined in UserRuntimeZero.ts
    IsTuple -> "rt.raw_istuple"
    IsList -> "rt.raw_islist"
    IsRecord -> "rt.isRecord"
    -- Note: Currently lists and tuples are both using the same RT length function.
    ListLength -> "rt.raw_listLength"
    TupleLength -> "rt.raw_tupleLength"
    RecordSize -> "rt.raw_recordSize"
    Head -> "rt.head"
    Tail -> "rt.tail"
    UnMinus -> "-"
    -- Not yet implemented in IR2Raw
    Fst -> error "Not yet implemented: Fst"
    Snd -> error "Not yet implemented: Snd"
    LevelOf -> error "Not yet implemented: LevelOf" -- (implemented in levelops.ts: "rt.levelOf")

{-- INSTRUCTIONS --}


-- omit _ = PP.empty 

ir2js :: StackInst -> W PP.Doc
ir2js (AssignRaw tt vn e) = do
  jj <- toJS e
  let pfx = case tt of 
               AssignConst -> text "const"
               AssignLet   -> text "let"
               AssignMut   -> PP.empty 
  return $ semi $ pfx <+> ppId vn <+> text "=" <+> jj 

-- Note: Technically this is handled in the same way as 'AssignRaw' (with 'AssignConst'),
-- because in JS it is just an assignment to a variable.
-- The only difference to AssignRaw is the type of variable name (here 'VarName', there 'RawVar') (even though both are wrappers for String)
ir2js (AssignLVal vn cexpr) = do
  d <- toJS cexpr
  return $ semi $ ppLet vn <+> d


ir2js (FetchStack x i) = return $ 
   ppLet x <+> text "_STACK[ _SP + " PP.<> text (show i) PP.<> text "]"

ir2js (StoreStack x i) = return $ 
   text "_STACK[ _SP + " PP.<> text (show i) PP.<> text "] = " <+> ppId x 


ir2js (MkFunClosures envBindings funBindings) = do
    -- Create new environment
    env <- freshEnvVar
    dd_env_ids <- ppEnvIds env envBindings
    let ppEnv = vcat [ semi $ hsep [ ppLet env
                                   , text "new rt.Env()"]
                     , dd_env_ids]
    let ppFF = map (\(v, f) -> jsClosure v env f) funBindings
    return $ vcat (ppEnv : ppFF)

       where ppEnvIds :: VarName ->  [(VarName, IR.VarAccess)] -> W PP.Doc
             ppEnvIds env ls = do 
               let penv = ppId env 
               d1 <- mapM (\(a,b) -> do 
                                  d_b <- toJS b
                                  return $ semi $ penv PP.<> text "." PP.<> (ppId a) <+> text "=" <+> d_b
                          ) 
                          ls
               d3 <- mapM (\(_, b) -> do 
                              d_b <- toJS b 
                              return $ d_b <> text ".dataLevel") ls
               let d2 = penv PP.<> text ".__dataLevel = " 
                        <+> jsFunCall (text $ binOpToJS Basics.LatticeJoin (Raw.UseNativeBinop False)) d3
                                
               return $ vcat ( d1 ++ [d2])
             hsepc ls = semi $ PP.hsep (PP.punctuate (text ",") ls)


ir2js (SetState c x) = return $ semi $ monStateToJs c <+> "=" <+> ppId x

ir2js (RTAssertion a) = return $ ppRTAssertionCode jsFunCall a

ir2js (LabelGroup ii) = do
  ii' <- mapM ppLevelOp ii
  sparseSlot <- ppSparseSlot
  return $ vcat $
           [ -- "if (! _T.getSparseBit()) {" -- Alternative, but involves extra call to RT
             "if (!" <+> sparseSlot <+> ") {"
           , nest 2 (vcat ii')
           , text "}"
           ]
    where ppLevelOp (AssignRaw tt vn e) = do
            jj <- toJS e
            let pfx = if tt == AssignConst then text "const" else PP.empty 
            return $ semi $ pfx <+> ppId vn <+> text "=" <+> jj 
          ppLevelOp x = toJS x  

ir2js (SetBranchFlag) = return $
  text "_T.setBranchFlag()"
ir2js InvalidateSparseBit = return $
  text "rt.raw_invalidateSparseBit()"



-- ir2js x = error $ "ir instruction translation not implemented: " ++ (show x)


{-- TERMINATORS --}


tr2js (StackExpand bb bb2) = do
    _frameSize <- gets frameSize
    _sparseSlot <- gets sparseSlot
    _consts <- gets consts
    modify (\s -> s {frameSize = 0, sparseSlot = _sparseSlot - _frameSize - 5})
        -- AA; 2021-04-24; Because 
    js <- toJS bb
    modify (\s -> s { frameSize = _frameSize, sparseSlot = _sparseSlot })
        -- TODO: AA; 2021-04-24; we should really be using a reader monad here for frame size
        -- #codedebt
    js2 <- toJS bb2
    kname <- freshKontName
    sparseSlotIdxPP <- ppSparseSlotIdx
    let jsKont =
           vcat ["this." PP.<> ppId kname <+> text "= () => {",
                  nest 2 $
                        vcat [
                          "let _T = rt.runtime.$t",
                          "let _STACK = _T.callStack",
                          "let _SP = _T._sp",
                          -- TODO Do we need this? It seems to be only used zero or one time in the generated places.
                          -- So we could instead just use the let where it is actually set.
                          "let _SP_OLD",
                          -- Check data bound at return point (could have received labelled information or raised).
                          -- Requires sparseSlot to be updated first.
                          "_T.sparseSlot =" <+> sparseSlotIdxPP,
                          "_T.updateSparseBitOnReturn()",
                          constsToJS _consts , -- 2021-05-18; TODO: optimize by including only the _used_ constants
                          js2
                        ],
                    "}"
                    -- debug support; 2021-04-24; AA                    
                    , "this." PP.<> ppId kname PP.<> text ".debugname = \"" PP.<> ppId kname PP.<> "\""                
                    ]


    tell ([], [], [jsKont] )
    return $ vcat [
      "_SP_OLD = _SP; ", -- 2021-04-23; hack ! ;AA
      "_SP = _SP + " <+> text (show (_frameSize + 5)) <+> ";",
      "_STACK[_SP - 5] = _SP_OLD;",
      "_STACK[_SP - 4] = _T.pc;", 
      "_STACK[_SP - 3] = this." PP.<> ppId kname, 
      "_STACK[_SP - 2] = _T.mailbox.mclear;", 
      "_STACK[_SP - 1] = false;",
      "_T._sp = _SP;", 
      js
      ] 
  --  return $ jsFunCall (text "_T.pushFrame") [ text "this." PP.<> ppId kname, (text.show) _frameSize ] $$ js




tr2js (If va bb1 bb2) = do
  js1 <- toJS bb1
  js2 <- toJS bb2
  return $
    vcat [ 
      -- jsFunCall (text "rt.branch") [ppId va],
      text "if" <+> PP.parens ( ppId va) <+> text "{",
      nest 2 js1,
      text "} else {",
      nest 2 js2,
      text "}"
    ]

    

tr2js (Ret) = return $
  jsFunCall (text "return _T.returnImmediate") []

tr2js (Error va pos) = return $
  (jsFunCall (text "rt.rawErrorPos")) [ppId va, ppPosInfo pos]

tr2js (TailCall va1 ) = return $
    "return" <+> ppId va1 

tr2js (LibExport va) = do
  d <- toJS va 
  return $ jsFunCall (text "return") [d]


monStateToJs c = 
  text "_T." PP.<>
      case c of 
        MonPC -> text "pc"
        MonBlock -> text "bl"
        R0_Val -> text "r0_val"
        R0_Lev -> text "r0_lev"
        R0_TLev -> text "r0_tlev"


ppSparseSlotIdx :: W PP.Doc
ppSparseSlotIdx = do
  s <- gets sparseSlot
  return $ text "_SP + " PP.<+> PP.int s

ppSparseSlot :: W PP.Doc 
ppSparseSlot = do    
   idx <- ppSparseSlotIdx
   return $ text "_STACK[ "  PP.<> idx PP.<> text "]"

-----------------------------------------------------------


fieldToJS :: ToJS a => (String, a) -> W PP.Doc
fieldToJS (f, v) = do 
    d <- toJS v 
    return $ PP.brackets $ PP.doubleQuotes (text f) <> text "," <> d

fieldsToJS :: ToJS a => [(String, a)] -> W [PP.Doc]
fieldsToJS fs = do 
    dd <- mapM fieldToJS fs 
    return $ PP.punctuate (text ",") dd

instance ToJS RawExpr where
  toJS x = do 
    HFN (fname) <- gets stHFN
    let ppFunSelfRef = text "$env." PP.<> ppId fname 
    let ppVarName IR.VarFunSelfRef = ppFunSelfRef
        ppVarName x = IR.ppVarAccess x
        
    case x of 
      ProjectState c -> return $ monStateToJs c
      ProjectLVal IR.VarFunSelfRef lf -> return (
        case lf of 
           Raw.FieldValue ->  ppFunSelfRef PP.<> 
                                text "." PP.<> PP.text (show Raw.FieldValue)
           Raw.FieldValLev -> monStateToJs MonPC
           Raw.FieldTypLev -> monStateToJs MonPC)
      e@(ProjectLVal _ _) -> return $ ppRawExpr e
      Bin binop use_native va1 va2 -> return $
        let text' = text (binOpToJS binop use_native) in
          if isInfixBinop binop use_native
          then hsep [ ppId va1, text', ppId va2 ]
          else jsFunCall text' [ppId va1, ppId va2]
      Un op v -> return $ text (unaryOpToJS op) <> PP.parens (ppId v)
      Tuple vars -> return $
        text "rt.mkTuple" <> PP.parens (PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppVarName vars))
      Record fields -> do
        jsFields <- fieldsToJS fields 
        return $
          PP.parens $ text "rt.mkRecord" <> PP.parens (PP.brackets $ PP.hsep $ jsFields )
      WithRecord r fields -> do 
        jsFields <- fieldsToJS fields 
        return $
          text "rt.withRecord" <> PP.parens (
            PP.hsep [ppId r, text ",", PP.brackets $ PP.hsep $ jsFields ])
      ProjField x f -> return $
        text "rt.getField" <> PP.parens (ppId x <> text "," <>  PP.doubleQuotes (text f ) )
      ProjIdx x idx -> return $
        text "rt.raw_indexTuple" <> PP.parens (ppId x <> text "," <>  text (show idx) )
      List vars -> return $
        PP.parens $   text "rt.mkList" <> PP.parens (PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppVarName vars))
      ListCons v1 v2 -> return $
        text "rt.cons" <>  PP.parens (ppVarName v1 <> text "," <> ppId v2)
      Const C.LUnit -> return $ text "rt.__unitbase"
      Const (C.LLabel s) -> return $
        text "rt.mkV1Label" <> (PP.parens . PP.doubleQuotes) (text s)
      Const lit -> do
        case lit of
          C.LAtom atom -> tell ([], [atom], [])
          _ -> return ()
        return $ ppLit lit
      Lib lib'@(Basics.LibName libname) varname -> do
        tell ([LibAccess lib' varname], [], [])
        return $
          text "rt.loadLib" <> PP.parens ((PP.doubleQuotes.text) libname <> text ", " <> (PP.doubleQuotes.text) varname <> text ", this")
      ConstructLVal r1 r2 r3 -> return $
        ppFunCall  (text "rt.constructLVal")  (map ppId [r1,r2,r3])
      Base b -> return $ text "rt." <+> text b -- Note: The "$$authorityarg" case is handled in IR2Raw




-----------------------------------------------------------
ppPosInfo :: GetPosInfo a => a -> PP.Doc 
ppPosInfo  = PP.doubleQuotes . text . show . posInfo

pickle = PP.doubleQuotes.text.T.unpack.decodeUtf8.encode
stdlib = [] -- "let runtime = require('../runtimeMonitored.js')"]
suffix  = [ "module.exports = Top "]


jsClosure var env f =
     vcat [ ppLet var <+> ((text "rt.mkVal") <> (PP.parens ((text "rt.RawClosure") <> (PP.parens (PP.hsep $ PP.punctuate "," [ppId env, text "this", text "this." PP.<> ppId f])))))
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) <+> PP.text "=" <+> ppId var
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) PP.<> text ".selfpointer = true"
          ]

ppLet x =  text "const" <+> ppId x <+> text "="

semi t = t PP.<> text ";"
jsFunCall a b = semi $ ppFunCall a b


freshEnvVar :: W VarName
freshEnvVar = do
    k <- gets freshCounter
    modify (\s -> s { freshCounter = k + 1  } )
    return $ VN  $ "$$$env" ++ (show k)


freshKontName :: W VarName
freshKontName = do
    j <- gets freshCounter
    HFN s <- gets stHFN
    modify (\s -> s { freshCounter = j + 1})
    return $ VN $  "$$$" ++ s ++ "$$$kont" ++ (show j)


isInfixBinop :: Basics.BinOp -> Raw.UseNativeBinop -> Bool
isInfixBinop op (Raw.UseNativeBinop use_native) = case op of 
  -- Infix
  Plus -> True
  Minus -> True
  Mult -> True
  Div -> True
  Mod -> True
  Le -> True
  Lt -> True
  Ge -> True
  Gt -> True
  And -> True
  Or -> True
  Concat -> True
  BinAnd -> True
  BinOr -> True
  BinXor -> True
  BinShiftLeft -> True
  BinShiftRight -> True
  BinZeroShiftRight -> True
  -- Flag dependent 
  Eq -> use_native
  Neq -> use_native 
  -- Not infix
  RaisedTo -> False
  FlowsTo -> False
  IntDiv -> False
  HasField -> False
  LatticeJoin -> False
  LatticeMeet -> False
