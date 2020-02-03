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
<0> "(*"                             { enterNewComment `andBegin` state_comment } 
<state_comment> "(*"                 { embedComment }
<state_comment> "*)"                 { unembedComment }
<state_comment> .                    ; 
<state_comment> \n                   ;

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
alexEOF = do 
    comment_depth <- getLexerCommentDepth 
    if comment_depth > 0 
        then alexError "Comment not closed at end of file"
        else return (L undefined TokenEOF)


-- handling of nested comments through the state
-- taken from the following example
-- https://github.com/simonmar/alex/blob/master/examples/tiger.x

state_initial :: Int
state_initial = 0

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len


getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())


-- we use a custom version of monadScan so that we have full
-- control over the error reporting; this one is based on 
-- the built-in alexMonadScan
-- see: https://github.com/simonmar/alex/blob/master/templates/wrappers.hs


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
-- TODO: 2020-02-03; AA; revisit this in case we want better error reporting 


}

