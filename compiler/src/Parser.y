{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseProg,
  parseTokens,
) where

import Lexer
import Direct
import DCLabels
import Basics
import TroupePositionInfo (Located(..), PosInf(..), noLoc, getLoc)
import ParseError (ParseEnv(..), ParseState(..), ParseErrorInfo(..),
                   formatParseError, formatAllErrors, initialParseState,
                   maxParseErrors, minErrorDistance)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (group, sort, intercalate)


}

-- Entry point
%name prog

-- Lexer structure
%tokentype { L Token }

-- Parser monad (ReaderT + StateT for error accumulation)
%monad { ReaderT ParseEnv (StateT ParseState (Except String)) } { (>>=) } { return }
%error { parserAbort } { parserReport }
%errorhandlertype explist

-- Token Names
%token
    let   { L _ TokenLet }
    in    { L _ TokenIn }
    end   { L _ TokenEnd }
    val   { L _ TokenVal }
    fun   { L _ TokenFun }
    and   { L _ TokenAnd }
    if    { L _ TokenIf }
    then  { L _ TokenThen }
    else  { L _ TokenElse }
    case  { L _ TokenCase }
    of    { L _ TokenOf }
    import { L _ TokenImport }
    datatype { L _ TokenDatatype }
    Atoms { L _ TokenAtoms }
    fn    { L _ TokenFn }
    hn    { L _ TokenHn }
    pini  { L _ TokenPini }
    when  { L _ TokenWhen }
    with  { L _ TokenWith }
    qualified { L _ TokenQualified }
    as    { L _ TokenAs }
    true  { L _ TokenTrue }
    false { L _ TokenFalse }
    andalso { L _ TokenAndAlso }
    orelse  { L _ TokenOrElse }
    'not' { L _ TokenNot }
    NUM   { L _ (TokenNum _) }
    FLOAT { L _ (TokenFloat _) }
    STRING{ L _ (TokenString _)}
    VAR   { L _  (TokenSym _) }
    LABEL { L _  (TokenLabel _) }
    '@'   { L _  TokenAt }
    '=>'  { L _ TokenArrow }
    '='   { L _ TokenEq }
    '+'   { L _ TokenAdd }
    '-'   { L _ TokenSub }
    '*'   { L _ TokenMul }
    '/'   { L _ TokenDiv }
    ';'   { L _ TokenSemi }
    '^'   { L _ TokenCaret }
    '<='  { L _ TokenLe }
    '>='  { L _ TokenGe }
    '<'   { L _ TokenLt }
    '>'   { L _ TokenGt }
    '<>'  { L _ TokenNe }
    div { L _ TokenIntDiv }
    mod { L _ TokenMod }
    andb  { L _ TokenBinAnd }
    orb   { L _ TokenBinOr }
    xorb  { L _ TokenBinXor }
    '<<'    { L _ TokenBinShiftLeft }
    '>>'    { L _ TokenBinShiftRight }
    '~>>'   { L _ TokenBinZeroShiftRight }
    '`<'    { L _ TokenDCLabelLeft  } 
    '>`'    { L _ TokenDCLabelRight } 
    '&'     { L _ TokenAmpersand }
    '#true'  { L _ TokenDCTrue }
    '#false' { L _ TokenDCFalse }
    '#root-confidentiality' { L _ TokenDCRootConf }
    '#null-confidentiality' { L _ TokenDCNullConf }
    '#root-integrity' { L _ TokenDCRootInteg }
    '#null-integrity' { L _ TokenDCNullInteg }    

    'raisedTo'    { L _ TokenRaisedTo }
    'latticeJoin' { L _ TokenLatticeJoin }
    'latticeMeet' { L _ TokenLatticeMeet }

    'isTuple' { L _ TokenIsTuple }
    'isList' { L _ TokenIsList }
    'isRecord' { L _ TokenIsRecord }

    '('   { L _ TokenLParen }
    ')'   { L _ TokenRParen }
    ','   { L _ TokenComma }
    '_'   { L _ TokenWildcard }
    '|'   { L _ TokenBar }
    '::'  { L _ TokenColonColon }
    '['   { L _ TokenLBracket }
    ']'   { L _ TokenRBracket }
    '.'   { L _ TokenDot }
    '..'  { L _ TokenDotDot }
    '{'   { L _ TokenLBrace }
    '}'   { L _ TokenRBrace }



-- Operators


%nonassoc with
%right '=>'
%right '|'
%right else
%right ';'
%left andalso orelse

%nonassoc '=' '<=' '>=' '<>' '<' '>' '@'

%left andb orb xorb
%left '<<' '>>' '~>>'

%left '+' '-'
%left '*' '/' div mod

%left '|'
%left '&'

%right '::'

%right '.'

%left 'raisedTo'

%left 'isTuple'
%left 'isList'
%left 'isRecord'

%left 'not'

%left 'latticeJoin'
%left 'latticeMeet'

%left '^'
%%


-- Syntax


Prog : ImportDecl AtomsDecl Expr                       { Prog (Imports $1) (Atoms $2) $3 }

ImportDecl: import OptQualified OptSelection VAR OptAlias ImportDecl
              { (ImportDecl (LibName (varTok $4)) $5 Nothing $3 $2) : $6 }
          | { [] }

OptQualified : qualified  { Qualified }
             | { Unqualified }

OptSelection : '{' VarList '}'  { Just $2 }
             | { Nothing }

OptAlias : as VAR   { Just (LibName (varTok $2)) }
         | { Nothing }

VarList : VAR              { [varTok $1] }
        | VAR ',' VarList  { (varTok $1) : $3 }


AtomsDecl : datatype Atoms '=' VAR AtomsList    {% do { p <- pos $4; checkDuplicateAtoms ((varTok $4, p):$5) } }
          |  {[]}

AtomsList : { [] }
          | '|' VAR AtomsList  {% do { p <- pos $2; return ((varTok $2, p): $3) } }


Expr: Form                        { $1 }
    | catch                        { noLoc (Lit LUnit) }  -- Error recovery
    | let pini Expr Decs in Expr end  {% atPos $1 (Let (piniDecl $3 $4) $6) }
    | let Decs in Expr end        {% atPos $1 (Let $2 $4) }
    | if Expr then Expr else Expr {% atPos $1 (If $2 $4 $6) }
    | fn Pattern '=>' Expr        {% atPos $1 (Abs (Lambda [$2] $4)) }
    | hn Pattern '=>' Expr        {% atPos $1 (Hnd (Handler $2 Nothing Nothing $4)) }
    | hn Pattern '|' Pattern '=>' Expr      {% atPos $1 (Hnd (Handler $2 (Just $4) Nothing $6)) }
    | hn Pattern when Expr '=>' Expr        {% atPos $1 (Hnd (Handler $2 Nothing (Just $4) $6)) }
    | hn Pattern '|' Pattern when Expr '=>' Expr      {% atPos $1 (Hnd (Handler $2 (Just $4) (Just $6) $8)) }
    | case Expr of Match          {% atPos $1 (Case $2 $4) }
    | Expr ';' Expr               {% mkSeq $1 $3 $2 }
    | Expr 'latticeMeet' Expr     {% atPos $2 (Bin LatticeMeet $1 $3) }
    | Expr 'latticeJoin' Expr     {% atPos $2 (Bin LatticeJoin $1 $3) }
    | Expr '-' Expr               {% atPos $2 (Bin Minus $1 $3) }
    | Expr '+' Expr               {% atPos $2 (Bin Plus $1 $3) }
    | Expr '>=' Expr              {% atPos $2 (Bin Ge $1 $3) }
    | Expr '*' Expr               {% atPos $2 (Bin Mult $1 $3) }
    | Expr '/' Expr               {% atPos $2 (Bin Div $1 $3) }
    | Expr div Expr               {% atPos $2 (Bin IntDiv $1 $3) }
    | Expr mod Expr               {% atPos $2 (Bin Mod $1 $3) }
    | Expr '^' Expr               {% atPos $2 (Bin Concat $1 $3) }
    | Expr '=' Expr               {% atPos $2 (Bin Eq $1 $3) }
    | Expr '<=' Expr              {% atPos $2 (Bin Le $1 $3) }
    | Expr '<' Expr               {% atPos $2 (Bin Lt $1 $3) }
    | Expr '>' Expr               {% atPos $2 (Bin Gt $1 $3) }
    | Expr '<>' Expr              {% atPos $2 (Bin Neq $1 $3) }
    | Expr andalso Expr           {% atPos $2 (Bin And $1 $3) }
    | Expr orelse  Expr           {% atPos $2 (Bin Or $1 $3) }
    | Expr andb Expr              {% atPos $2 (Bin BinAnd $1 $3) }
    | Expr orb Expr               {% atPos $2 (Bin BinOr $1 $3) }
    | Expr xorb Expr              {% atPos $2 (Bin BinXor $1 $3) }
    | Expr '<<' Expr              {% atPos $2 (Bin BinShiftLeft $1 $3) }
    | Expr '>>' Expr              {% atPos $2 (Bin BinShiftRight $1 $3) }
    | Expr '~>>' Expr             {% atPos $2 (Bin BinZeroShiftRight $1 $3) }
    | Expr '::' Expr              {% atPos $2 (ListCons $1 $3) }
    | Expr 'raisedTo' Expr        {% atPos $2 (Bin RaisedTo $1 $3) }
    | 'isTuple' Expr              {% atPos $1 (Un IsTuple $2) }
    | 'isList' Expr               {% atPos $1 (Un IsList $2) }
    | 'isRecord' Expr             {% atPos $1 (Un IsRecord $2) }
    | 'not' Expr                  {% atPos $1 (Un Not $2) }


Match : Pattern '=>' Expr                      { [($1,$3)] }
      | Pattern '=>' Expr '|' Match            { ($1,$3):$5 }
      -- Error recovery: skip bad case arm content
      | catch                                  { [(noLoc ErrorPattern, noLoc (Lit LUnit))] }


Form :: { LTerm }
Form :  '-' Form                    {% atPos $1 (Un UnMinus $2) }
     | Fact                        { fromFact $1 }


Fact : Fact Atom                   { $2 : $1 }
     | Atom                        { [$1] }


LabelExp: 
       VAR                         { TagExp (varTok $1) }
     | '(' LabelExp ')'            { $2 }
     | LabelExp '&'  LabelExp      { OpExp Conj $1 $3 } 
     | LabelExp '|'  LabelExp      { OpExp Disj $1 $3 }

ConfLabelExp :                     { ConstComponent LabelTrue }
     | '#root-confidentiality'     { ConstComponent LabelFalse }
     | '#null-confidentiality'     { ConstComponent LabelTrue }
     | '#false'                    { ConstComponent LabelFalse }
     | '#true'                     { ConstComponent LabelTrue }
     | LabelExp                    { ExprComponent $1 }

IntLabelExp :                      { ConstComponent LabelTrue }
     | '#root-integrity'           { ConstComponent LabelFalse }
     | '#null-integrity'           { ConstComponent LabelTrue }
     | '#false'                    { ConstComponent LabelFalse }
     | '#true'                     { ConstComponent LabelTrue }
     | LabelExp                    { ExprComponent $1 }     

DCLabelExp:
     ConfLabelExp ';' IntLabelExp         { DCLabelExp ($1, $3) } 

-- Lit now returns Located Lit to preserve source positions for all literals
Lit:   NUM                        {% atPos $1 (LNumeric (NumInt (numTok $1))) }
     | FLOAT                       {% atPos $1 (LNumeric (NumFloat (floatTok $1))) }
     | STRING                      {% atPos $1 (LString (strTok $1)) }
     | true                        {% atPos $1 (LBool True) }
     | false                       {% atPos $1 (LBool False) }
     | LABEL                       {% atPos $1 (LLabel (lblTok $1)) }
     |'`<' DCLabelExp '>`'         {% atPos $1 (LDCLabel $2) }


-- Atom uses Located Lit to preserve source positions
Atom : '(' Expr ')'                { $2 }
     | Lit                         { let Loc p l = $1 in Loc p (Lit l) }
     | VAR                         {% atPos $1 (Var (varTok $1)) }
     | '(' ')'                     {% atPos $1 (Lit LUnit) }
     | '(' CSExpr Expr ')'         {% atPos $1 (Tuple (reverse ($3:$2))) }
     | '{' '}'                     {% atPos $1 (Record []) }
     | RecordExpr                  { $1 }
     | ListExpr                    { $1 }
     | Atom '.' VAR                {% atPos $2 (ProjField $1 (varTok $3)) }
     | Atom '.' NUM                {% atPos $2 (ProjIdx $1 (fromInteger (numTok $3))) }


RecordExpr
     : '{' RecordFields  '}'          {% atPos $1 (Record $2) }
     | '{' Atom with RecordFields'}'  {% atPos $1 (WithRecord $2 $4) }
     

RecordFields
     : Field                           { [$1] }
     | Field ',' RecordFields          { $1 : $3 }


Field 
     : VAR                         { (varTok $1, Nothing) }
     | VAR '=' Expr                { (varTok $1, Just $3) }
     


ListExpr :: {LTerm}
ListExpr : '[' ']'                 {% atPos $1 (List []) }
     | '[' Expr ']'                {% atPos $1 (List [$2]) }
     | '[' CSExpr Expr ']'         {% atPos $1 (List (reverse ($3:$2))) }

CSExpr : Expr ','                  { [$1] }
     | CSExpr Expr ','             { ($2:$1) }


Pattern : VAR                               {% atPos $1 (VarPattern (varTok $1)) }
    | '(' Pattern ')'                       { $2 }
    | Pattern '@' LABEL                     {% atPos $2 (AtPattern $1 (lblTok $3)) }
    | '(' ')'                               {% atPos $1 (ValPattern LUnit) }
    | '_'                                   {% atPos $1 Wildcard }
    | NUM                                   {% atPos $1 (ValPattern (LNumeric (NumInt (numTok $1)))) }
    | FLOAT                                 {% atPos $1 (ValPattern (LNumeric (NumFloat (floatTok $1)))) }
    | STRING                                {% atPos $1 (ValPattern (LString (strTok $1))) }
    | true                                  {% atPos $1 (ValPattern (LBool True)) }
    | false                                 {% atPos $1 (ValPattern (LBool False)) }
    | LABEL                                 {% atPos $1 (ValPattern (LLabel (lblTok $1))) }
    | '`<' DCLabelExp '>`'                  {% atPos $1 (ValPattern (LDCLabel $2)) }
    | '(' CSPattern Pattern ')'             {% atPos $1 (TuplePattern (reverse ($3:$2))) }
    | FieldPattern                          { $1 }
    | ListPattern   { $1}


FieldPattern :
      '{' '}'                                        {% atPos $1 (RecordPattern [] ExactMatch) }
    | '{' '..' '}'                                   {% atPos $1 (RecordPattern [] WildcardMatch) }
    | '{' FieldPat '}'                               {% atPos $1 (RecordPattern [$2] ExactMatch) }
    | '{' FieldPat ',' '..' '}'                      {% atPos $1 (RecordPattern [$2] WildcardMatch) }
    | '{' FieldPatterns FieldPat '}'                 {% atPos $1 (RecordPattern (reverse ($3:$2)) ExactMatch) }
    | '{' FieldPatterns FieldPat ',' '..' '}'        {% atPos $1 (RecordPattern (reverse ($3:$2)) WildcardMatch) }


FieldPatterns
    : FieldPat ','                  { [$1]    }
    | FieldPatterns FieldPat ','    { ($2:$1 )}


FieldPat
    : VAR              {(varTok $1, Nothing) }
    | VAR '=' Pattern  {(varTok $1, Just $3) }

ListPattern:  '[' ']'                              {% atPos $1 (ListPattern []) }
    | '[' Pattern ']'                              {% atPos $1 (ListPattern [$2]) }
    | '[' CSPattern Pattern ']'                    {% atPos $1 (ListPattern (reverse ($3:$2))) }
    |     Pattern '::' Pattern                     {% atPos $2 (ConsPattern $1 $3) }


CSPattern : Pattern ','         { [$1] }
    | CSPattern  Pattern ','    { ($2:$1) }


Dec : val Pattern '=' Expr         { ValDecl $2 $4 }
    | FunDecs                       { FunDecs $1 }
    -- Error recovery: skip bad declaration
    | catch                         { ErrorDecl }

Decs : Dec                          { [$1] }
     | Dec Decs                     { $1 : $2 }

FunDecs : FunDecl                  { [$1] }
      | FunDecl AndFunDecs         { $1 : $2 }

AndFunDecs : AndFunDecl            { [$1] }
           | AndFunDecl AndFunDecs { $1 : $2 }




FunOptions : FirstFunOption         { [$1] }
   | FirstFunOption OtherFunOptions { $1: $2}

OtherFunOptions : OtherFunOption   {[ $1 ]}
  | OtherFunOption OtherFunOptions { $1 : $2 }

FirstFunOption : FunArgs '=' Expr   { Lambda $1 $3}

OtherFunOption : '|' VAR FunArgs '=' Expr { Lambda $3 $5}


FunDecl    : fun VAR FunOptions {% atPos $2 (FunDecl (varTok $2) $3) }
AndFunDecl : and VAR FunOptions {% atPos $2 (FunDecl (varTok $2) $3) }

FunArgs : Pattern                        { [$1]  }
        | Pattern FunArgs                { $1 : $2}

{

-- | Parser monad type alias
type ParseM a = ReaderT ParseEnv (StateT ParseState (Except String)) a

-- Helper to create a located pattern at RTGen position
rtGenPat :: DeclPattern -> LDeclPattern
rtGenPat = Loc (RTGen "parser")

-- Helper to create a located term at RTGen position
rtGenTerm :: Term -> LTerm
rtGenTerm = Loc (RTGen "parser")

-- Error recovery placeholders
-- These create placeholder AST nodes when the parser recovers from an error
-- Using RTGen position since these are generated during error recovery
errorExpr :: L Token -> LTerm
errorExpr _ = Loc (RTGen "error-recovery") (Lit LUnit)  -- Placeholder expression

errorPattern :: L Token -> LDeclPattern
errorPattern _ = Loc (RTGen "error-recovery") ErrorPattern


errorDecl :: L Token -> Decl
errorDecl _ = ErrorDecl

piniDecl :: LTerm -> [Decl] -> [Decl]
piniDecl auth decs =
    let pushDecl = ValDecl (rtGenPat (VarPattern "$pini"))
                           (rtGenTerm (App (rtGenTerm (Var "pinipush")) [auth]))
        popDecl  = ValDecl (rtGenPat Wildcard)
                           (rtGenTerm (App (rtGenTerm (Var "pinipop")) [rtGenTerm (Var "$pini")]))
    in
        (pushDecl:decs) ++ [popDecl]

-- mkSeq now takes the token to get position from
mkSeq :: LTerm -> LTerm -> L Token -> ParseM LTerm
mkSeq t1 t2 tok = do
    p <- pos tok
    let ts = case t2 of
                Loc _ (Seq innerTs) -> innerTs
                _ -> [t2]
    return $ Loc p (Seq (t1 : ts))


fromFact :: [LTerm] -> LTerm
fromFact [x] = x
fromFact xs =
  let (y:ys) = reverse xs
      p = getLoc y  -- Use position from the function term
  in Loc p (App y ys)


-- | Get position from token list
getTokenPosition :: [L Token] -> (Int, Int)
getTokenPosition (l:_) = let (AlexPn _ line col) = getPos l in (line, col)
getTokenPosition [] = (0, 0)

-- | Create ParseErrorInfo from tokens and expected list
makeParseErrorInfo :: ParseEnv -> [L Token] -> [String] -> ParseErrorInfo
makeParseErrorInfo env tokens expected =
    let (line, col) = getTokenPosition tokens
        sourceLines = lines (peSource env)
        maybeToken = case tokens of
          (l:_) -> Just (unPos l)
          []    -> Nothing
    in ParseErrorInfo
          { peiFilename    = peFilename env
          , peiLine        = line
          , peiColumn      = col
          , peiToken       = maybeToken
          , peiExpected    = map cleanExpectedToken expected
          , peiSourceLines = sourceLines
          , peiContext     = Nothing
          }

-- | Record an error from a catch token (used in grammar productions)
-- This is called when catch consumes a token during error recovery
recordError' :: L Token -> ParseM ()
recordError' tok = do
    _ <- recordError [tok] []
    return ()

-- | Record an error with duplicate suppression
-- Returns True if the error was recorded, False if it was a duplicate
recordError :: [L Token] -> [String] -> ParseM Bool
recordError tokens expected = do
    env <- ask
    state <- get
    let (line, col) = getTokenPosition tokens
        isDup = case psLastErrorPos state of
          Nothing -> False
          Just (lastLine, lastCol) ->
            -- Suppress if same line AND close column, or adjacent lines
            (line == lastLine && abs (col - lastCol) < 3) ||
            (line /= lastLine && abs (line - lastLine) < minErrorDistance)
    if isDup
      then return False
      else do
        let err = makeParseErrorInfo env tokens expected
        put state { psErrors = err : psErrors state
                  , psErrorCount = psErrorCount state + 1
                  , psLastErrorPos = Just (line, col) }
        return True

-- | Called when recovery is impossible (final error handler)
-- Note: happyAbort provides only tokens, not expected list
parserAbort :: [L Token] -> ParseM a
parserAbort tokens = do
    -- When called directly by happyAbort, we don't have expected tokens
    -- But we may have accumulated errors already from parserReport calls
    state <- get
    case psErrors state of
      [] -> do
        -- No previous errors recorded, create one without expected info
        _ <- recordError tokens []
        state' <- get
        throwError $ formatAllErrors (reverse $ psErrors state')
      _ ->
        -- Already have errors from parserReport, just output them
        throwError $ formatAllErrors (reverse $ psErrors state)

-- | Called on each error for potential recovery
-- The resume function allows continuing after error
parserReport :: ([L Token], [String]) -> ([L Token] -> ParseM a) -> ParseM a
parserReport (tokens, expected) resume = do
    _ <- recordError tokens expected
    state <- get
    if psErrorCount state >= maxParseErrors
      then parserAbort tokens
      else resume tokens

-- | Legacy parseError for backward compatibility during transition
parseError :: ([L Token], [String]) -> ParseM a
parseError (tokens, _) = parserAbort tokens

-- | Clean up token names from Happy's %token declarations to human-readable form
cleanExpectedToken :: String -> String
cleanExpectedToken "let" = "keyword 'let'"
cleanExpectedToken "in" = "keyword 'in'"
cleanExpectedToken "end" = "keyword 'end'"
cleanExpectedToken "val" = "keyword 'val'"
cleanExpectedToken "fun" = "keyword 'fun'"
cleanExpectedToken "and" = "keyword 'and'"
cleanExpectedToken "if" = "keyword 'if'"
cleanExpectedToken "then" = "keyword 'then'"
cleanExpectedToken "else" = "keyword 'else'"
cleanExpectedToken "case" = "keyword 'case'"
cleanExpectedToken "of" = "keyword 'of'"
cleanExpectedToken "import" = "keyword 'import'"
cleanExpectedToken "fn" = "keyword 'fn'"
cleanExpectedToken "hn" = "keyword 'hn'"
cleanExpectedToken "pini" = "keyword 'pini'"
cleanExpectedToken "when" = "keyword 'when'"
cleanExpectedToken "with" = "keyword 'with'"
cleanExpectedToken "receive" = "keyword 'receive'"
cleanExpectedToken "qualified" = "keyword 'qualified'"
cleanExpectedToken "as" = "keyword 'as'"
cleanExpectedToken "datatype" = "keyword 'datatype'"
cleanExpectedToken "Atoms" = "keyword 'Atoms'"
cleanExpectedToken "true" = "'true'"
cleanExpectedToken "false" = "'false'"
cleanExpectedToken "andalso" = "'andalso'"
cleanExpectedToken "orelse" = "'orelse'"
cleanExpectedToken "div" = "'div'"
cleanExpectedToken "mod" = "'mod'"
cleanExpectedToken "VAR" = "identifier"
cleanExpectedToken "NUM" = "number"
cleanExpectedToken "FLOAT" = "float"
cleanExpectedToken "STRING" = "string"
cleanExpectedToken "LABEL" = "label"
cleanExpectedToken "'=>'" = "'=>'"
cleanExpectedToken "'='" = "'='"
cleanExpectedToken "';'" = "';'"
cleanExpectedToken "'('" = "'('"
cleanExpectedToken "')'" = "')'"
cleanExpectedToken "'['" = "'['"
cleanExpectedToken "']'" = "']'"
cleanExpectedToken "'{'" = "'{'"
cleanExpectedToken "'}'" = "'}'"
cleanExpectedToken "','" = "','"
cleanExpectedToken "'|'" = "'|'"
cleanExpectedToken "'_'" = "'_'"
cleanExpectedToken "'::'" = "'::'"
cleanExpectedToken "'.'" = "'.'"
cleanExpectedToken "'..'" = "'..'"
cleanExpectedToken "'+'" = "'+'"
cleanExpectedToken "'-'" = "'-'"
cleanExpectedToken "'*'" = "'*'"
cleanExpectedToken "'/'" = "'/'"
cleanExpectedToken "'<'" = "'<'"
cleanExpectedToken "'<='" = "'<='"
cleanExpectedToken "'>'" = "'>'"
cleanExpectedToken "'>='" = "'>='"
cleanExpectedToken "'<>'" = "'<>'"
cleanExpectedToken "'@'" = "'@'"
cleanExpectedToken "'^'" = "'^'"
cleanExpectedToken "'&'" = "'&'"
cleanExpectedToken "'`<'" = "'`<' (DC label)"
cleanExpectedToken "'>`'" = "'>`' (DC label end)"
cleanExpectedToken "'andb'" = "'andb'"
cleanExpectedToken "'orb'" = "'orb'"
cleanExpectedToken "'xorb'" = "'xorb'"
cleanExpectedToken "'<<'" = "'<<'"
cleanExpectedToken "'>>'" = "'>>'"
cleanExpectedToken "'~>>'" = "'~>>'"
cleanExpectedToken "'raisedTo'" = "'raisedTo'"
cleanExpectedToken "'isTuple'" = "'isTuple'"
cleanExpectedToken "'isList'" = "'isList'"
cleanExpectedToken "'isRecord'" = "'isRecord'"
cleanExpectedToken "'not'" = "'not'"
cleanExpectedToken "'flowsTo'" = "'flowsTo'"
cleanExpectedToken "'levelOf'" = "'levelOf'"
cleanExpectedToken "'latticeMeet'" = "'glb'"
cleanExpectedToken "'latticeJoin'" = "'lub'"
cleanExpectedToken s = s  -- fallback


parseTokens :: String -> Either String [L Token]
parseTokens = runExcept . scanTokens


parseProg :: FilePath -> String -> Either String Prog
parseProg filename input = runExcept $ do
  tokenStream <- scanTokens input
  let env = ParseEnv { peFilename = filename, peSource = input }
  (ast, finalState) <- runStateT (runReaderT (prog tokenStream) env) initialParseState
  -- If any errors were accumulated, report them all
  case psErrors finalState of
    [] -> return ast
    errs -> throwError $ formatAllErrors (reverse errs)


numTok (L _ (TokenNum x))    = x
floatTok (L _ (TokenFloat x)) = x
strTok (L _ (TokenString x)) = x
varTok (L _ (TokenSym x ))   = x
lblTok (L _ (TokenLabel x))  = x

pos :: L Token -> ParseM PosInf
pos l = do
    env <- ask
    let (AlexPn _ line col) = getPos l
    return $ SrcPosInf (peFilename env) line col

-- | Create a Located value at the position of the given token
atPos :: L Token -> a -> ParseM (Located a)
atPos tok x = do
    p <- pos tok
    return (Loc p x)

-- Check for duplicate atom names and report all duplicates with positions
checkDuplicateAtoms :: [(String, PosInf)] -> ParseM [AtomName]
checkDuplicateAtoms atoms
  | null dups = return names
  | otherwise = throwError $ intercalate "\n" (map formatOne dups)
  where
    names = map fst atoms
    dups = [n | (n:_:_) <- group (sort names)]
    formatOne d =
      let positions = [p | (n, p) <- atoms, n == d]
      in "Duplicate constructor '" ++ d ++ "' at " ++
         intercalate " and " (map show positions)

}
