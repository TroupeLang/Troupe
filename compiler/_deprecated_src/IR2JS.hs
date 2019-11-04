{-# LANGUAGE OverloadedStrings #-}
module IR2JS (irProg2JSString,irToJSString) where

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
import Data.ByteString.Base64 (encode,decode) -- cabal install base64-bytestring
import IR
import CompileMode

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)

data LibAccess = LibAccess Basics.LibName Basics.VarName
   deriving (Eq, Show)


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

type W = RWS Bool  [LibAccess] ()

a $$+ b  = a $$ (nest 2 b)

class ToJS a where
   toJS :: a -> W PP.Doc


irProg2JSString :: CompileMode -> Bool -> IRProgram -> String
irProg2JSString compileMode debugOut ir =
  let (inner, _, _) = runRWS (toJS ir) debugOut ()
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
  let (inner, _, libs) = runRWS (toJS x) False ()
  in PP.render (addLibs libs $$ inner)


instance ToJS SerializationUnit where
  toJS (KontSerialization kdecl) = toJS kdecl
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
  toJS (IRProgram atoms konts funs) = do
     jjA <- toJS atoms
     (jjK, libsK) <- listen $ mapM toJS konts
     (jjF, libsF) <- listen $ mapM toJS funs
     
     return $
          vcat $ [ text "this.uuid = rt.rt_uuid"

                 , jsLoadLibs
                 , addLibs $ libsK ++ libsF
                 , jjA
                 ] ++ jjK ++ jjF

          


instance ToJS C.Atoms where
  toJS catoms@(C.Atoms atoms) = return $
    vcat [ vcat $ (map  (\a -> hsep ["const"
                                    , text a
                                    , "= new rt.Atom"
                                    , (PP.parens ( (PP.quotes.text) a <+> text ", this.uuid"))]) atoms)
         , text "this.serializedatoms =" <+> (pickle.serializeAtoms) catoms]



instance ToJS IRBBTree where
    toJS (BB ins tr) = do
      jj  <- mapM toJS ins
      j'  <- toJS tr
      return $ vcat $ jj ++ [j']


instance ToJS KontFunDef where
    toJS kdef@(KontFunDef hkn env arg bb) = do
        jj <- toJS bb
        debug <- ask
        return $
          vcat [ text "this." PP.<> ppId hkn <+> text "= function" <+> ppArgs [ppId env, ppId arg] <+> text "{"
               , if debug then nest 2 $ text "rt.debug" <+> (PP.parens . PP.quotes.  ppId) hkn  
                          else PP.empty 
               -- , nest 2 $ semi $ text "rt.resetret(" PP.<> ppId env PP.<> text ".ret)"
               , nest 2 jj
               , text "}"
               , semi $ text "this." PP.<> ppId hkn PP.<> text ".deps =" <+> ppDeps kdef
               , semi $ text "this." PP.<> ppId hkn PP.<> text ".serialized =" <+> (pickle.serializeKontDef) kdef ]



instance ToJS FunDef where
    toJS fdef@(FunDef hfn env arg bb) = do
       jj <- toJS bb
       debug <- ask
       return $
          vcat [text "this." PP.<>  ppId hfn <+> text "= function" <+> ppArgs [ppId env, ppId arg] <+> text "{"
               , if debug then nest 2 $ text "rt.debug" <+> (PP.parens . PP.quotes.  ppId) hfn
                          else PP.empty 
               , nest 2 jj
               , text "}"
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".deps =" <+> ppDeps fdef
               , semi $ text "this." PP.<> ppId hfn PP.<> text ".serialized =" <+> (pickle.serializeFunDef) fdef ]


pickle = PP.doubleQuotes.text.T.unpack.decodeUtf8.encode

instance ToJS IRInst where
  toJS = ir2js

  
ir2js :: IRInst -> W PP.Doc
ir2js (AssignVar vn st) = do
  jj <- irExpr2js st
  return $ semi$ ppLet vn <+> jj 

ir2js (MkFunClosure vn env hfn) = return $
  jsClosure vn env hfn

ir2js (SetRet ka env) = return $
  jsFunCall (text "rt.setret") [text "this", text "this." PP.<> ppId ka, ppId env]

ir2js (MkEnv env vars) = return $
    vcat [ semi $ hsep [ ppLet env
                       , text "new rt.Env()"]
         , ppEnvIds vars]
  where ppEnvIds ls =
          vcat $ map (\(a,b) -> semi $ (ppId env) PP.<> text "." PP.<> (ppId a) <+> text "=" <+> ppId b ) ls
        hsepc ls = semi $ PP.hsep (PP.punctuate (text ",") ls)



instance ToJS IRTerminator where
  toJS = tr2js



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
    


tr2js (AssertOrError va bb1 verr) = do
  js1 <- toJS bb1  
  return $    
    vcat [ 
      jsFunCall (text "rt.assertOrError") [ppId va],
      text "if" <+> PP.parens ( text "rt.getVal" <> PP.parens (ppId va)) <+> text "{" ,
      nest 2 js1,
      text "} else {",
      nest 2 $ jsFunCall (text "rt.error") [ppId verr] ,
      text "}"
    ]
    

tr2js (Ret va) = return $
  jsFunCall (text "rt.ret") [ppId va]

tr2js (Error va) = return $
  (jsFunCall (text "rt.error")) [ppId va]

tr2js (TailCall va1 va2) = return $
  jsFunCall (text "rt.tailcall") [ppId va1, ppId va2]

tr2js (LibExport va) = return $
  jsFunCall (text "return") [ppId va]



jsClosure var env f =
     vcat [ ppLet var <+> ((text "rt.mkVal") <> (PP.parens ((text "new rt.Closure") <> (PP.parens (PP.hsep $ PP.punctuate "," [ppId env, text "this", text "this." PP.<> ppId f])))))
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) <+> PP.text "=" <+> ppId var
          , semi $ ppId env PP.<> PP.text "." PP.<> (ppId var ) PP.<> text ".selfpointer = true"
          ]



ppLet x =  text "const" <+> ppId x <+> text "="


--------------------------------------------------

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
  text "rt.mkVal" <> (PP.parens $ CPS.ppLit lit)
irExpr2js (Base v) = 
  return $ 
    if v == "$$authorityarg"  -- we are taking special cover of the argument; a bit of a hack; 2018-10-18; AA
      then text v 
      else text "rt.mkCopy" <> (PP.parens $ text "rt." <> text v)
irExpr2js (Lib lib'@(Basics.LibName libname) varname) = do
  tell [LibAccess lib' varname]
  return $
    text "rt.loadLib" <> (PP.parens  $ (PP.quotes.text) libname <> text ", " <> (PP.quotes.text) varname <> text ", this" )


stdlib = [] -- "let runtime = require('../runtimeMonitored.js')"]
--suffix = ["let $top = new Top(runtime.runtime);"
--         ,"runtime.start ($top)"]

suffix  = [ "module.exports = Top "]



-- some utility functions 

semi t = t PP.<> text ";"
jsFunCall a b = semi $ ppFunCall a b
