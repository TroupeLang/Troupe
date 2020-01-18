{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CCIRANF2JS (irProg2JSString,irToJSString,irToJSON) where 

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
import CCIRANF
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


type TheState = Integer

type W = RWS Bool  [LibAccess] TheState


initState = 0 

a $$+ b  = a $$ (nest 2 b)

class ToJS a where
   toJS :: a -> W PP.Doc



irProg2JSString :: CompileMode -> Bool -> IRProgram -> String
irProg2JSString compileMode debugOut ir =
  let (inner, _, _) = runRWS (toJS ir) debugOut initState
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


irToJSString :: ToJS a => a -> String
irToJSString x =
  let (inner, _, libs) = runRWS (toJS x) False initState
  in PP.render (addLibs libs $$ inner)


irToJSON :: SerializationUnit -> ByteString
irToJSON x = 
  let (inner, _, libs) = runRWS (toJS x) False initState
  in Aeson.encode $ JSOutput { libs = libs
                             , fname = case x of FunSerialization (FunDef (HFN n)_ _) -> Just n
                                                 _ -> Nothing
                             , code = PP.render (addLibs libs $$ inner) 
                             } 



instance ToJS SerializationUnit where
  toJS (FunSerialization fdecl) = toJS fdecl
  toJS (AtomsSerialization ca) = toJS ca

ppNamespaceName = text "Top"  -- should be generating a new namespace per received blob


irProg2JsWrapped prog = do
    inner <- toJS prog
    return $
       text "function" <+> ppNamespaceName <+> text "(rt) {"
       $$ nest 2 inner
       $$ text "}"



instance ToJS IRProgram where
  toJS (IRProgram atoms funs) = do
     jjA <- toJS atoms
     (jjF, libsF) <- listen $ mapM toJS funs
     
     return $
          vcat $ [ -- text "this.uuid = rt.rt_uuid", 
                   jsLoadLibs
                 , addLibs libsF
                 , jjA
                 ] ++ jjF

          


instance ToJS C.Atoms where
  toJS catoms@(C.Atoms atoms) = return $
    vcat [ vcat $ (map  (\a -> hsep ["const"
                                    , text a
                                    , "= new rt.Atom"
                                    , (PP.parens ( (PP.quotes.text) a <+> text ", rt.rt_uuid"))]) atoms)
         , text "this.serializedatoms =" <+> (pickle.serializeAtoms) catoms]



instance ToJS FunDef where 
    toJS fdef@(FunDef hfn arg bb) = do
       jj <- toJS bb
       debug <- ask
       let (irdeps, libdeps ) = ppDeps fdef
       return $
          vcat [text "this." PP.<>  ppId hfn <+> text "= function" <+> ppArgs ["$env", ppId arg] <+> text "{"
               , if debug then nest 2 $ text "rt.debug" <+> (PP.parens . PP.quotes.  ppId) hfn
                          else PP.empty 
               , nest 2 jj
               , text "}"
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".deps =" <+> irdeps
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".libdeps =" <+> libdeps
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".serialized =" <+> (pickle.serializeFunDef) fdef ]



instance ToJS IRBBTree where 
--  toJS = bb2js

    toJS (BB ins tr) = do
      jj  <- mapM toJS ins
      j'  <- toJS tr
      return $ vcat $ jj ++ [j']

{--
bb2js :: IRBBTree -> W PP.Doc 
bb2js (BB [] tr) = toJS tr 
bb2js (BB (i:ii) tr) = do
  ppII <- bb2js (BB ii tr)    
  case i of 
    (Assign vn st) -> do
        jj <- irExpr2js st
        return $ (semi$ ppLet vn <+> jj) $$ ppII

    (MkFunClosures envBindings funBindings) ->  do 
      env <- freshEnvVar
      let ppEnv = vcat [ semi $ hsep [ ppLet env
                                    , text "new rt.Env()"]
                      , ppEnvIds env envBindings]
      let ppFF = map (\(v, f) -> jsClosure v env f) funBindings
      return $ vcat (ppEnv : ppFF) $$ ppII
        where ppEnvIds env ls =
                  vcat $ map (\(a,b) -> semi $ (ppId env) PP.<> text "." PP.<> (ppId a) <+> text "=" <+> ppId b ) ls
              hsepc ls = semi $ PP.hsep (PP.punctuate (text ",") ls)
    (Call vn bb) -> do 
        js <- toJS bb
        let jsAnonClos = (PP.parens (ppId vn)) <+> text "=>" $$ PP.braces (nest 2 ppII)
        return $ jsFunCall (text "rt.push") [ jsAnonClos ] $$ js
    
--}




instance ToJS IRInst where
  toJS = ir2js


instance ToJS IRTerminator where 
  toJS = tr2js

ir2js :: IRInst -> W PP.Doc
ir2js (Assign vn st) = do
  jj <- irExpr2js st
  return $ semi$ ppLet vn <+> jj 

ir2js (MkFunClosures envBindings funBindings) = do
    env <- freshEnvVar
    let ppEnv = vcat [ semi $ hsep [ ppLet env
                                   , text "new rt.Env()"]
                     , ppEnvIds env envBindings]
    let ppFF = map (\(v, f) -> jsClosure v env f) funBindings
    return $ vcat (ppEnv : ppFF)

       where ppEnvIds env ls =
                vcat $ map (\(a,b) -> semi $ (ppId env) PP.<> text "." PP.<> (ppId a) <+> text "=" <+> ppId b ) ls
             hsepc ls = semi $ PP.hsep (PP.punctuate (text ",") ls)


tr2js (Call vn bb bb2) = do 
    js <- toJS bb
    js2 <- toJS bb2
    let jsAnonClos = (PP.parens (ppId vn)) <+> text "=>" $$ PP.braces (nest 2 js2)
    return $ jsFunCall (text "rt.push") [ jsAnonClos ] $$ js


tr2js (If va bb1 bb2) = do
  js1 <- toJS bb1
  js2 <- toJS bb2
  return $
    vcat [ 
      jsFunCall (text "rt.branch") [ppId va],
      text "if" <+> PP.parens ((text "rt.getVal" ) <> PP.parens (ppId va)) <+> text "{",
      nest 2 js1,
      text "} else {",
      nest 2 js2,
      text "}"
    ]


tr2js (AssertElseError va bb1 verr pos) = do
  js1 <- toJS bb1  
  return $    
    vcat [ 
      jsFunCall (text "rt.assertOrError") [ppId va],
      text "if" <+> PP.parens ( text "rt.getVal" <> PP.parens (ppId va)) <+> text "{" ,
      nest 2 js1,
      text "} else {",
      nest 2 $ jsFunCall (text "rt.errorPos") [ppId verr, ppPosInfo pos] ,
      text "}"
    ]
    

tr2js (Ret va) = return $
  jsFunCall (text "rt.ret") [ppId va]

tr2js (Error va pos) = return $
  (jsFunCall (text "rt.errorPos")) [ppId va, ppPosInfo pos]

tr2js (TailCall va1 va2) = return $
  jsFunCall (text "rt.tailcall") [ppId va1, ppId va2]

tr2js (LibExport va) = return $
  jsFunCall (text "return") [ppId va]



-----------------------------------------------------------

irExpr2js :: IRExpr -> W PP.Doc
irExpr2js (Bin binop va1 va2) = return $
  jsFunCall ((text . textOfBinOp) binop) [  ppVarAccess va1, ppVarAccess va2]
irExpr2js (Un op v) = return $
  text (textOfUnOp op) <> PP.parens (ppVarAccess v)
irExpr2js (Tuple vars) = return $
  text "rt.mkVal"  <> (PP.parens $  (text "rt.mkTuple") <> (PP.parens $PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppVarAccess vars)   ))
irExpr2js (List vars) = return $
  text "rt.mkVal" <> (PP.parens $   (text "rt.mkList") <> (PP.parens $ PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map ppVarAccess vars)) )
irExpr2js (ListCons v1 v2) = return $
  text "rt.cons" <>  ( PP.parens $ ppVarAccess v1 <> text "," <> ppVarAccess v2)
irExpr2js (Const (C.LUnit)) = return $ (text "rt.__unit")
irExpr2js (Const (C.LLabel s)) = return $
  text "rt.mkLabel" <> (PP.parens . PP.doubleQuotes) (text s)
irExpr2js (Const lit) = return $
  jsFunCall (text "rt.mkValPos") [CPS.ppLit lit, ppPosInfo lit]

  -- text "rt.mkVal" <> (PP.parens $ CPS.ppLit lit)
irExpr2js (Base v) = 
  return $ 
    if v == "$$authorityarg"  -- we are taking special cover of the argument; a bit of a hack; 2018-10-18; AA
      then text v 
      else text "rt.mkCopy" <> (PP.parens $ text "rt." <> text v)
irExpr2js (Lib lib'@(Basics.LibName libname) varname) = do
  tell [LibAccess lib' varname]
  return $
    text "rt.loadLib" <> (PP.parens  $ (PP.quotes.text) libname <> text ", " <> (PP.quotes.text) varname <> text ", this" )


-----------------------------------------------------------
ppPosInfo :: GetPosInfo a => a -> PP.Doc 
ppPosInfo  = PP.quotes . text . show . posInfo

pickle = PP.doubleQuotes.text.T.unpack.decodeUtf8.encode
stdlib = [] -- "let runtime = require('../runtimeMonitored.js')"]
suffix  = [ "module.exports = Top "]


jsClosure var env f =
     vcat [ ppLet var <+> ((text "rt.mkVal") <> (PP.parens ((text "new rt.Closure") <> (PP.parens (PP.hsep $ PP.punctuate "," [ppId env, text "this", text "this." PP.<> ppId f])))))
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) <+> PP.text "=" <+> ppId var
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) PP.<> text ".selfpointer = true"
          ]

ppLet x =  text "const" <+> ppId x <+> text "="

semi t = t PP.<> text ";"
jsFunCall a b = semi $ ppFunCall a b


-- freshEnvVar :: W Integer 
freshEnvVar = do 
    k <- get 
    put (k + 1 )
    return $ VN  $ "$$$env" ++ (show k)