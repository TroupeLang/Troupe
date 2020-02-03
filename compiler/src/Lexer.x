{

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts #-}


module Lexer (
  Token(..),
  scanTokens,
  L(..),
  AlexPosn(..)
) where

import Direct
import Control.Monad.Except
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)


}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$graphic    = $printable # $white
@string     = \" ($printable # \")* \"
-- @label      = \{ $white* ($graphic # \})* $white* \}
@label      = \{ ($printable # \})*  \}


tokens :-
-- Whitespace insensitive
<0> $eol                           ;
<0> $white+                        ;

  -- Comments
<0> "(*)".*                          ;
  -- We support simple one line comments that are consistent with SML
<0> "(*".*"*)"                     ;

  -- Syntax
<0>   let                            { \p s -> L p TokenLet }
<0>   in                             { \p s -> L p TokenIn }
<0>   end                            { \p s -> L p TokenEnd }
<0>   val                            { \p s -> L p TokenVal }
<0>   fun                            { \p s -> L p TokenFun }
<0>   and                            { \p s -> L p TokenAnd }
<0>   if                             { \p s -> L p TokenIf  }
<0>   then                           { \p s -> L p TokenThen }
<0>   else                           { \p s -> L p TokenElse }
<0>   fn                             { \p s -> L p TokenFn }
<0>   hn                             { \p s -> L p TokenHn }
<0>   true                           { \p s -> L p TokenTrue }
<0>   false                          { \p s -> L p TokenFalse }
<0>   case                           { \p s -> L p TokenCase }
<0>   of                             { \p s -> L p TokenOf }
<0>   import                         { \p s -> L p TokenImport }
<0>   andalso                        { \p s -> L p TokenAndAlso }
<0>   orelse                         { \p s -> L p TokenOrElse }
<0>   raisedTo                       { \p s -> L p TokenRaisedTo }
<0>   pini                           { \p s -> L p TokenPini}
<0>   when                           { \p s -> L p TokenWhen  }
<0>   datatype                       { \p s -> L p TokenDatatype }
<0>   Atoms                          { \p s -> L p TokenAtoms }
<0>   $digit+                        { \p s -> L p (TokenNum (read s)) }
<0>   [\@]                           { \p s -> L p TokenAt }
<0>   [\=][\>]                       { \p s -> L p TokenArrow }
<0>   [\=]                           { \p s -> L p TokenEq }
<0>   [\+]                           { \p s -> L p TokenAdd }
<0>   [\-]                           { \p s -> L p TokenSub }
<0>   [\*]                           { \p s -> L p TokenMul }
<0>   [\^]                           { \p s -> L p TokenCaret } 
<0>   [\/]                           { \p s -> L p TokenDiv }
<0>   [\<][\>]                       { \p s -> L p TokenNe }
<0>   [\<][\=]                       { \p s -> L p TokenLe }
<0>   [\<]                           { \p s -> L p TokenLt }
<0>   [\>][\=]                       { \p s -> L p TokenGe }
<0>   [\>]                           { \p s -> L p TokenGt }
<0>   \(                             { \p s -> L p TokenLParen }
<0>   \)                             { \p s -> L p TokenRParen }
<0>   [\,]                           { \p s -> L p TokenComma }
<0>   [\|]                           { \p s -> L p TokenBar }
<0>   [\_]                           { \p s -> L p TokenWildcard }
<0>   [\:][\:]                       { \p s -> L p TokenColonColon }
<0>   [\[]                           { \p s -> L p TokenLBracket }
<0>   [\]]                           { \p s -> L p TokenRBracket }
<0>   $alpha [$alpha $digit \_ \']*  { \p s -> L p (TokenSym s) }
<0>   @string                        { \p s -> L p (TokenString (unquote s)) }
<0>   @label                         { \p s -> L p (TokenLabel (((map toLower) . trim . unquote) s)) }

{

unquote = reverse . tail . reverse . tail
trim = dropWhileEnd isSpace . dropWhile isSpace

data L a = L {  getPos :: AlexPosn, unPos :: a } deriving (Eq, Show)


data Token
  = TokenLet
  | TokenIn
  | TokenEnd
  | TokenFun
  | TokenAnd
  | TokenVal
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenCase
  | TokenOf
  | TokenImport
  | TokenReceive
  | TokenPini
  | TokenWhen
  | TokenDatatype
  | TokenAtoms
  | TokenFn
  | TokenHn
  | TokenNum Integer
  | TokenSym String
  | TokenString String
  | TokenTrue
  | TokenFalse
  | TokenAndAlso
  | TokenOrElse
  | TokenArrow
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenEq
  | TokenNe
  | TokenLt
  | TokenLe
  | TokenGe
  | TokenGt
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenWildcard
  | TokenBar
  | TokenColonColon
  | TokenLBracket
  | TokenRBracket
  | TokenEOF
  | TokenRaisedTo
  | TokenFlowsTo
  | TokenLevelOf
  | TokenLabel String
  | TokenAt
  | TokenCaret 
  deriving (Eq,Show)

scanTokens :: String -> Except String [L Token]
scanTokens str = go (alexStartPos, '\n',[],str) where
    go inp@(pos,_,_bs,str) =
      case alexScan inp 0 of
       AlexEOF -> return []

       AlexError ((AlexPn _ line column), _,_, _) -> 
            throwError $ "Invalid lexeme at position " ++ (show line) ++ ":" ++ (show column) 

       AlexSkip  inp' len     -> go inp'
       AlexToken inp' len act -> do
        res <- go inp'
        let rest = act pos (take len str)
        return (rest : res)
}
