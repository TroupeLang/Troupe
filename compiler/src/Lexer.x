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
  $eol                           ;
  $white+                        ;

  -- Comments
  "(*)".*                          ;
  -- We support simple one line comments that are consistent with SML
  "(*".*"*)"                     ;

  -- Syntax
  let                            { \p s -> L p TokenLet }
  in                             { \p s -> L p TokenIn }
  end                            { \p s -> L p TokenEnd }
  val                            { \p s -> L p TokenVal }
  fun                            { \p s -> L p TokenFun }
  and                            { \p s -> L p TokenAnd }
  if                             { \p s -> L p TokenIf  }
  then                           { \p s -> L p TokenThen }
  else                           { \p s -> L p TokenElse }
  fn                             { \p s -> L p TokenFn }
  hn                             { \p s -> L p TokenHn }
  true                           { \p s -> L p TokenTrue }
  false                          { \p s -> L p TokenFalse }
  case                           { \p s -> L p TokenCase }
  of                             { \p s -> L p TokenOf }
  import                         { \p s -> L p TokenImport }
  andalso                        { \p s -> L p TokenAndAlso }
  orelse                         { \p s -> L p TokenOrElse }
  raisedTo                       { \p s -> L p TokenRaisedTo }
  pini                           { \p s -> L p TokenPini}

  when                           { \p s -> L p TokenWhen  }
  datatype                       { \p s -> L p TokenDatatype }
  Atoms                          { \p s -> L p TokenAtoms }
  $digit+                        { \p s -> L p (TokenNum (read s)) }

  [\@]                           { \p s -> L p TokenAt }
  [\=][\>]                       { \p s -> L p TokenArrow }
  [\=]                           { \p s -> L p TokenEq }
  [\+]                           { \p s -> L p TokenAdd }
  [\-]                           { \p s -> L p TokenSub }
  [\*]                           { \p s -> L p TokenMul }
  [\^]                           { \p s -> L p TokenCaret } 
  [\/]                           { \p s -> L p TokenDiv }
  [\<][\>]                       { \p s -> L p TokenNe }
  [\<][\=]                       { \p s -> L p TokenLe }
  [\<]                           { \p s -> L p TokenLt }
  [\>][\=]                       { \p s -> L p TokenGe }
  [\>]                           { \p s -> L p TokenGt }
  \(                             { \p s -> L p TokenLParen }
  \)                             { \p s -> L p TokenRParen }
  [\,]                           { \p s -> L p TokenComma }
  [\|]                           { \p s -> L p TokenBar }
  [\_]                           { \p s -> L p TokenWildcard }
  [\:][\:]                       { \p s -> L p TokenColonColon }
  [\[]                           { \p s -> L p TokenLBracket }
  [\]]                           { \p s -> L p TokenRBracket }

  $alpha [$alpha $digit \_ \']*  { \p s -> L p (TokenSym s) }
  @string                        { \p s -> L p (TokenString (unquote s)) }
  @label                         { \p s -> L p (TokenLabel (((map toLower) . trim . unquote) s)) }

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
