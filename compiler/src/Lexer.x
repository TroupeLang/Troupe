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

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$graphic    = $printable # $white
@string     = \" ($printable # \")* \"
@label      = \{ ($printable # \})*  \}


tokens:-
-- Whitespace insensitive
<0> $eol                           ;
<0> $white+                        ;

-- Comments
<0> "(*)".*                        ;
-- We support simple one line comments that are consistent with SML
<0> "(*".*"*)"                     ;

-- Syntax
<0>   let                            { mkL TokenLet }
<0>   in                             { mkL TokenIn }
<0>   end                            { mkL TokenEnd }
<0>   val                            { mkL TokenVal }
<0>   fun                            { mkL TokenFun }
<0>   and                            { mkL TokenAnd }
<0>   if                             { mkL TokenIf  }
<0>   then                           { mkL TokenThen }
<0>   else                           { mkL TokenElse }
<0>   fn                             { mkL TokenFn }
<0>   hn                             { mkL TokenHn }
<0>   true                           { mkL TokenTrue }
<0>   false                          { mkL TokenFalse }
<0>   case                           { mkL TokenCase }
<0>   of                             { mkL TokenOf }
<0>   import                         { mkL TokenImport }
<0>   andalso                        { mkL TokenAndAlso }
<0>   orelse                         { mkL TokenOrElse }
<0>   raisedTo                       { mkL TokenRaisedTo }
<0>   pini                           { mkL TokenPini}
<0>   when                           { mkL TokenWhen  }
<0>   datatype                       { mkL TokenDatatype }
<0>   Atoms                          { mkL TokenAtoms }
<0>   $digit+                        { mkLs (\s -> TokenNum (read s)) }
<0>   [\@]                           { mkL TokenAt }
<0>   [\=][\>]                       { mkL TokenArrow }
<0>   [\=]                           { mkL TokenEq }
<0>   [\+]                           { mkL TokenAdd }
<0>   [\-]                           { mkL TokenSub }
<0>   [\*]                           { mkL TokenMul }
<0>   [\^]                           { mkL TokenCaret } 
<0>   [\/]                           { mkL TokenDiv }
<0>   [\<][\>]                       { mkL TokenNe }
<0>   [\<][\=]                       { mkL TokenLe }
<0>   [\<]                           { mkL TokenLt }
<0>   [\>][\=]                       { mkL TokenGe }
<0>   [\>]                           { mkL TokenGt }
<0>   \(                             { mkL TokenLParen }
<0>   \)                             { mkL TokenRParen }
<0>   [\,]                           { mkL TokenComma }
<0>   [\|]                           { mkL TokenBar }
<0>   [\_]                           { mkL TokenWildcard }
<0>   [\:][\:]                       { mkL TokenColonColon }
<0>   [\[]                           { mkL TokenLBracket }
<0>   [\]]                           { mkL TokenRBracket }
<0>   $alpha [$alpha $digit \_ \']*  { mkLs (\s -> TokenSym s) }
<0>   @string                        { mkLs (\s -> TokenString (unquote s)) }
<0>   @label                         { mkLs (\s -> (TokenLabel (((map toLower) . trim . unquote) s)))}

{

-- The user state monad
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                   --  , lexerStringState   :: Bool
                   --  , lexerStringValue   :: String
                   }

alexInitUserState :: AlexUserState 
alexInitUserState = AlexUserState 
                   {
                       lexerCommentDepth = 0 
                   }
               
unquote = reverse . tail . reverse . tail
trim = dropWhileEnd isSpace . dropWhile isSpace

data L a = L {  getPos :: AlexPosn, unPos :: a } deriving (Eq, Show)
-- unPos is a pretty unfortunate name here; 2020-02-03; AA

mkL :: Token -> AlexInput -> Int -> Alex Lexeme
mkL t (p,_,_,_) len = return (L p t)


mkLs :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme 
mkLs f (p, _, _, str) len = 
    let s = take len str in
    return (L p (f s))

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

type Lexeme = L Token 

-- definition needed by Alex
alexEOF :: Alex Lexeme
alexEOF = return (L undefined TokenEOF)


-- scanTokensOLD :: String -> Except String [L Token]
-- scanTokensOLD str = go (alexStartPos, '\n',[],str) where
--     go inp@(pos,_,_bs,str) =
--       case alexScan inp 0 of
--        AlexEOF -> return []
-- 
--        AlexError ((AlexPn _ line column), _,_, _) -> 
--             throwError $ "Invalid lexeme at position " ++ (show line) ++ ":" ++ (show column) 
-- 
--        AlexSkip  inp' len     -> go inp'
--        AlexToken inp' len act -> do
--         res <- go inp'
--         let rest = act pos (take len str)
--         return (rest : res)




monadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  us <- alexGetUserState
  case alexScanUser us inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> 
                alexError $ "Invalid lexeme at position " ++ (show line) ++ ":" ++ (show column) 
    AlexSkip  inp' len -> do
        alexSetInput inp'
        monadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

scanTokens :: String -> Except String [L Token]
scanTokens str =
    let loop = 
            do t <- monadScan 
               case unPos t of 
                 TokenEOF -> return [] 
                 _ -> do toks <- loop 
                         return (t: toks)
    in case runAlex str loop of 
         Right r -> return r 
         Left s -> throwError s 
-- TODO: 2020-02-03; AA; check if we want to change the error reporting format here

}

