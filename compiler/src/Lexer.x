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
import Data.Char ( chr )
import Numeric ( readDec )
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alpha_ = [$alpha \_]
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

-- Strings 

<0>   @string                        { mkLs (\s -> TokenString (unquote s)) }


-- More complicated string handling is currently OFF 
-- The lexer infrastructure is in the file, but we do not have the backend support yet 
-- and not even sure that it is needed (we should be able to piggyback on JS) 

-- <0>             \"           { enterNewString `andBegin` state_string }
-- <state_string>  \\n          { addCharToString '\n' }
-- <state_string>  \\t          { addCharToString '\t' }
-- <state_string>  \\\^[@-_]    { addControlToString }
-- <state_string>  \\$digit$digit$digit { addAsciiToString }
-- <state_string>  \\\"         { addCharToString '\"' }
-- <state_string>  \\\\         { addCharToString '\\' }
-- <state_string>  \\[\ \n\t\f\r\b\v]+\\   ;
-- <state_string>  \\           { \_ _ -> lexerError "Illegal escape sequence" }
-- <state_string>  \"           { leaveString `andBegin` state_initial }
-- <state_string>  .            { addCurrentToString }
-- <state_string>  \n           { skip }


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
<0>   [\;]                           { mkL TokenSemi }
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
<0>   $alpha_ [$alpha $digit \_ \']*  { mkLs (\s -> TokenSym s) }
<0>   @label                         { mkLs (\s -> (TokenLabel (((map toLower) . trim . unquote) s)))}

{

-- The user state monad
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringState   :: Bool
                     , lexerStringValue   :: String
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
  | TokenSemi
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

type Action = AlexInput -> Int -> Alex Lexeme

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       monadScan

addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       monadScan

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"

leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return ( L p (TokenString (reverse s))) 
       -- Lexeme p (STRING (reverse s)) (Just (take len input)))


showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col


lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')




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

