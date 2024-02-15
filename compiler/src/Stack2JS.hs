{-- 
Translation from Stack to JS code.
The names of most runtime functions are specified at the respective place here.
However, those for 'RTAssertion' are defined via 'ppRTAssertion'.

TODO
- Port the code for serialization (AA; 2020-12-04)

--}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Stack2JS where
-- import qualified IR2JS 

import IR (SerializationUnit(..), HFN(..)
          , ppId, ppFunCall, ppArgs, Fields (..), Ident
          , serializeFunDef
          , serializeAtoms )
import qualified IR
import qualified Raw

import Raw (RawExpr (..), RawType(..), RawVar (..), MonComponent(..), RTAssertion(..),
            ppRawExpr, ppRTAssertionCode)

import Stack

import qualified Basics
import Basics(BinOp(..), UnaryOp(..))
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


import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)

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
  PP.text "lib:" <+> (PP.quotes. PP.text) libname <+> PP.text "," <+>
  PP.text "decl:" <+> (PP.quotes. PP.text) varname


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
  let args = (PP.quotes.PP.text) libname <+> text "," <+> (PP.quotes. PP.text) varname
  in text "this.addLib " <+> PP.parens args

addLibs xs = vcat (map addOneLib xs)


data TheState = TheState { freshCounter :: Integer
                         , frameSize    :: Int
                         , boundSlot    :: Int
                         , consts       :: Raw.Consts 
                         , stHFN        :: IR.HFN } 

type RetKontText = PP.Doc

type W = RWS Bool  ([LibAccess], [Basics.AtomName], [RetKontText]) TheState


initState = TheState { freshCounter = 0
                     , frameSize = error "frameSize should not be accessed yet"
                     , boundSlot = error "boundSlot should not be accessed yet"
                     , consts = error "consts should not be accessed yet"
                     , stHFN = error "stHFN should not be accessed yet"
                     }

a $$+ b  = a $$ (nest 2 b)

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
                                                  , (PP.parens ( (PP.quotes.text) a))]) atoms)
         , text "this.serializedatoms =" <+> (pickle.serializeAtoms) catoms]

lit2JS C.LUnit = text "rt.__unitbase"
lit2JS (C.LLabel s) = text "rt.mkLabel" <> (PP.parens . PP.doubleQuotes) (text s)
lit2JS lit = CPS.ppLit lit

constsToJS consts = 
     vcat $ map toJsConst consts 
               where toJsConst (x,lit) = hsep ["const", ppId x , text "=", lit2JS lit ]

instance ToJS FunDef where 
    toJS fdef@(FunDef hfn stacksize consts bb irfdef) = do
       {--
          |  |  | ... | <bound_slot> | 
          ^           ^
          |           |
          SP          stacksize 
       
       --}       
       let _frameSize = stacksize + 1 

       modify (\s -> s { frameSize = _frameSize, boundSlot = stacksize, stHFN = hfn, consts = consts } ) -- + 1 for the _data_bound_by_pc flag; 2021-03-17; AA
       let lits = constsToJS consts 
       jj <- toJS bb
       debug <- ask
       let (irdeps, libdeps, atomdeps ) = IR.ppDeps irfdef           
           b_slot_index = text "_SP + " PP.<> (PP.int stacksize)
           data_bound_by_pc_slot = text "_STACK[ " PP.<> b_slot_index PP.<> "]"
            
       return $
          vcat [text "this." PP.<>  ppId hfn <+> text "=" <+> ppArgs ["$env"] <+> text "=> {"
               , if debug then nest 2 $ text "rt.debug" <+> (PP.parens . PP.quotes.  ppId) hfn
                          else PP.empty 
               , nest 2 $ vcat $ [ 
                  "let _T = rt.runtime.$t",
                  "let _STACK = _T.callStack",
                  "let _SP = _T._sp",
                  "let _SP_OLD",
                  data_bound_by_pc_slot <+> " = _T.checkDataBoundsEntry($env.__dataLevel)",
                  "_T.boundSlot = " <+> b_slot_index,
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

binOpToJS :: BinOp -> String
binOpToJS = \case
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
    Eq -> "rt.eq"
    Neq -> "rt.neq"
    Concat -> "+"
    HasField -> "rt.hasField"
    LatticeJoin -> "rt.join"
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
    let ppEnv = vcat [ semi $ hsep [ ppLet env
                                   , text "new rt.Env()"]
                     , ppEnvIds env envBindings]
    let ppFF = map (\(v, f) -> jsClosure v env f) funBindings
    return $ vcat (ppEnv : ppFF)

       where ppEnvIds env ls =
                vcat (
                      (map (\(a,b) -> semi $ (ppId env) PP.<> text "." PP.<> (ppId a) <+> text "=" <+> ppId b ) ls)
                      ++ 
                      [ppId env PP.<> text ".__dataLevel = " <+> (jsFunCall "rt.join" (map (\(_, b) -> ppId b <> text ".dataLevel") ls )) ]
                )
             hsepc ls = semi $ PP.hsep (PP.punctuate (text ",") ls)


ir2js (SetState c x) = 
  let rhs = case c of MonBlock -> ppFunCall "rt.wrap_block_rhs" [ppId x]
                      _     -> ppId x 

  in return $ semi $ monStateToJs c <+> "=" <+> rhs 

ir2js (RTAssertion a) = return $ ppRTAssertionCode jsFunCall a

ir2js (LabelGroup ii) = do 
  ii' <- mapM ppLevelOp ii 
  b_slot <- data_bounded_by_pc_slot
  return $ vcat $
           [ "if (!" <+> b_slot <+> ") {"
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


tr2js (Call bb bb2) = do 
    _frameSize <- frameSize <$> get 
    _boundSlot <- boundSlot <$> get 
    _consts    <- consts <$> get 
    modify (\s -> s {frameSize = 0, boundSlot = _boundSlot - _frameSize - 5})
        -- AA; 2021-04-24; Because 
    js <- toJS bb
    modify (\s -> s { frameSize = _frameSize, boundSlot = _boundSlot }) 
        -- TODO: AA; 2021-04-24; we should really be using a reader monad here for frame size
        -- #codedebt
    js2 <- toJS bb2
    kname <- freshKontName 
    b_slot <- data_bounded_by_pc_slot
    b_slot_index <- b_slot_absolute_index
    let jsKont = 
           vcat ["this." PP.<> ppId kname <+> text "= () => {",                 
                  nest 2 $ 
                        vcat [                          
                          "let _T = rt.runtime.$t",
                          "let _STACK = _T.callStack",
                          "let _SP = _T._sp",
                          "let _SP_OLD",
                          b_slot <+> "= _T.checkDataBounds(" <+> b_slot <+> ")" , 
                          "_T.boundSlot =" <+> b_slot_index ,
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

tr2js (LibExport va) = return $
  jsFunCall (text "return") [ppId va]


monStateToJs c = 
  text "_T." PP.<>
      case c of 
        MonPC -> text "pc"
        MonBlock -> text "bl"
        R0_Val -> text "r0_val"
        R0_Lev -> text "r0_lev"
        R0_TLev -> text "r0_tlev"


data_bounded_by_pc_slot  :: W PP.Doc 
data_bounded_by_pc_slot = do    
   _b <- boundSlot <$> get 
   return $ text "_STACK[ _SP + "  PP.<> (text (show (_b)))  PP.<> text "]"

b_slot_absolute_index :: W PP.Doc 
b_slot_absolute_index = do 
  _b <- boundSlot<$> get  
  return $ text "_SP +" PP.<+> (PP.int _b)
-----------------------------------------------------------


ppField :: IR.Identifier a => (String, a) -> PP.Doc
ppField (f, v) = PP.brackets $ PP.quotes (text f) <> text "," <> ppId v

ppFields :: IR.Identifier a => [(String, a)] -> [PP.Doc]
ppFields fs = PP.punctuate (text ",") (map ppField fs)

instance ToJS RawExpr where
  toJS = \case
    ProjectState c -> return $ monStateToJs c
    e@(ProjectLVal _ _) -> return $ ppRawExpr e
    Bin binop va1 va2 -> return $
      let text' = (text . binOpToJS) binop in
        if isInfixBinop binop
        then hsep [ ppId va1, text', ppId va2 ]
        else jsFunCall text' [ppId va1, ppId va2]
    Un op v -> return $ text (unaryOpToJS op) <> PP.parens (ppId v)
    Tuple vars -> return $
     text "rt.mkTuple" <> PP.parens (PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppId vars))
    Record fields -> return $
      PP.parens $ text "rt.mkRecord" <> PP.parens (PP.brackets $ PP.hsep $ ppFields fields)
    WithRecord r fields -> return $
      text "rt.withRecord" <> PP.parens (
        PP.hsep [ppId r, text ",", PP.brackets $ PP.hsep $ ppFields fields ])
    ProjField x f -> return $
      text "rt.getField" <> PP.parens (ppId x <> text "," <>  PP.quotes (text f ) )
    ProjIdx x idx -> return $
      text "rt.raw_indexTuple" <> PP.parens (ppId x <> text "," <>  text (show idx) )
    List vars -> return $
      PP.parens $   text "rt.mkList" <> PP.parens (PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppId vars))
    ListCons v1 v2 -> return $
      text "rt.cons" <>  PP.parens (ppId v1 <> text "," <> ppId v2)
    Const C.LUnit -> return $ text "rt.__unitbase"
    Const (C.LLabel s) -> return $
      text "rt.mkLabel" <> (PP.parens . PP.doubleQuotes) (text s)
    Const lit -> do
      case lit of
        C.LAtom atom -> tell ([], [atom], [])
        _ -> return ()
      return $ CPS.ppLit lit
    Lib lib'@(Basics.LibName libname) varname -> do
      tell ([LibAccess lib' varname], [], [])
      return $
        text "rt.loadLib" <> PP.parens ((PP.quotes.text) libname <> text ", " <> (PP.quotes.text) varname <> text ", this")
    ConstructLVal r1 r2 r3 -> return $
      ppFunCall  (text "rt.constructLVal")  (map ppId [r1,r2,r3])
    Base b -> return $ text "rt." <+> text b -- Note: The "$$authorityarg" case is handled in IR2Raw




-----------------------------------------------------------
ppPosInfo :: GetPosInfo a => a -> PP.Doc 
ppPosInfo  = PP.quotes . text . show . posInfo

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
    k <- freshCounter <$>  get 
    modify (\s -> s { freshCounter = k + 1  } )  
    return $ VN  $ "$$$env" ++ (show k)

 
freshKontName :: W VarName
freshKontName = do 
    j <- freshCounter <$> get 
    HFN s <- stHFN <$> get 
    modify (\s -> s { freshCounter = j + 1})
    return $ VN $  "$$$" ++ s ++ "$$$kont" ++ (show j)


isInfixBinop :: Basics.BinOp -> Bool
isInfixBinop = \case
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
  -- Not infix
  Eq -> False
  Neq -> False
  RaisedTo -> False
  FlowsTo -> False
  IntDiv -> False
  HasField -> False
  LatticeJoin -> False
  LatticeMeet -> False
