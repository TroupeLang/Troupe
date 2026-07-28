{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseProg,
  parseTokens,
) where

import Lexer
import Surface
import DCLabels
import Basics
import TroupePositionInfo (Located(..), PosInf(..), noLoc, getLoc)
import ParseError (ParseEnv(..), ParseState(..), ParseErrorInfo(..),
                   formatParseError, formatAllErrors, initialParseState,
                   maxParseErrors, minErrorDistance)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State


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
    NUM   { L _ (TokenNum _) }
    BIGNUM { L _ (TokenBigInt _) }
    FLOAT { L _ (TokenFloat _) }
    STRING{ L _ (TokenString _)}
    VAR   { L _  (TokenSym _) }
    TYVAR { L _  (TokenTyVar _) }
    LABEL { L _  (TokenLabel _) }
    OPSYM { L _  (TokenOperator _) }
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
    infixl { L _ TokenInfixl }
    infixr { L _ TokenInfixr }
    infix  { L _ TokenInfix }
    andb  { L _ TokenBinAnd }
    orb   { L _ TokenBinOr }
    xorb  { L _ TokenBinXor }
    '<<'    { L _ TokenBinShiftLeft }
    '>>'    { L _ TokenBinShiftRight }
    '~>>'   { L _ TokenBinZeroShiftRight }
    '`<'    { L _ TokenDCLabelLeft  } 
    '>`'    { L _ TokenDCLabelRight } 
    '&'     { L _ TokenAmpersand }
    '#root-confidentiality' { L _ TokenDCRootConf }
    '#null-confidentiality' { L _ TokenDCNullConf }
    '#root-integrity' { L _ TokenDCRootInteg }
    '#null-integrity' { L _ TokenDCNullInteg }    

    'raisedTo' { L _ TokenRaisedTo }
    'isTuple' { L _ TokenIsTuple }
    'isList' { L _ TokenIsList }
    'isRecord' { L _ TokenIsRecord }
    'not' { L _ TokenNot }

    '('   { L _ TokenLParen }
    ')'   { L _ TokenRParen }
    ','   { L _ TokenComma }
    '_'   { L _ TokenWildcard }
    '|'   { L _ TokenBar }
    '::'  { L _ TokenColonColon }
    ':'   { L _ TokenColon }
    '['   { L _ TokenLBracket }
    ']'   { L _ TokenRBracket }
    '.'   { L _ TokenDot }
    '..'  { L _ TokenDotDot }
    '{'   { L _ TokenLBrace }
    '}'   { L _ TokenRBrace }



-- Operators

-- Lowest precedence: tags the type-application reduce rules (TyExp -> AppTy,
-- ProdList -> AppTy) so that a following VAR is shifted to extend a postfix
-- type application (`int list`) rather than ending the type. This makes type
-- application greedy, as in SML; see the TyExp productions.
%nonassoc TYAPP_LOW

%nonassoc with
%right '=>'
%right '|'
%right else
-- Chain-closing productions end in nonterminals and would have no precedence;
-- CHAIN_DONE, declared below every operator token, tags them so that every
-- close-vs-extend conflict resolves toward extending: chains are maximal.
-- (Same device as TYAPP_LOW below.)
%nonassoc CHAIN_DONE
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
%left '^'

-- Highest precedence: a type name (VAR) shifted to extend a postfix type
-- application wins over ending the type expression (see TYAPP_LOW above).
%left VAR
%%




Prog : ImportDecl FixityDecls TopDecls Expr
         { Prog (Imports $1) $2 $3 $4 }

-- Fixity declarations form their own header section between imports and
-- datatype declarations: interface metadata lives with the other
-- interface-level constructs.
FixityDecls : {- empty -}              { [] }
   | FixityDecl FixityDecls            { $1 : $2 }

FixityDecl : infixl NUM OpNames        {% mkFixityDecl $1 OpLeft ($2) $3 }
   | infixr NUM OpNames                {% mkFixityDecl $1 OpRight ($2) $3 }
   | infix NUM OpNames                 {% mkFixityDecl $1 OpNon ($2) $3 }

OpNames : OPSYM                        { [opTok $1] }
   | OPSYM OpNames                     { opTok $1 : $2 }

ImportDecl: import OptQualified OptSelection VAR OptAlias ImportDecl
              { (ImportDecl (LibName (varTok $4)) Nothing $5 Nothing $3 $2 [] []) : $6 }
          | import OptQualified OptSelection STRING OptAlias ImportDecl
              { (ImportDecl (LibName (moduleBindName (strTok $4))) (Just (strTok $4)) $5 Nothing $3 $2 [] []) : $6 }
          | { [] }

OptQualified : qualified  { Qualified }
             | { Unqualified }

OptSelection : '{' VarList '}'  { Just $2 }
             | { Nothing }

OptAlias : as VAR   { Just (LibName (varTok $2)) }
         | { Nothing }

VarList : VAR              { [varTok $1] }
        | VAR ',' VarList  { (varTok $1) : $3 }
        | '(' OPSYM ')'              { [opTok $2] }
        | '(' OPSYM ')' ',' VarList  { (opTok $2) : $5 }


-- The declaration section: a sequence of `datatype` declaration groups. The
-- leading `datatype` keyword introduces each group; the result is the list of
-- declaration groups.
TopDecls : {- empty -}                          { [] }
   | datatype DataGroup TopDecls                { $2 : $3 }


-- A single declaration group (the leading `datatype` is consumed by TopDecls);
-- its members are joined by `and`.
DataGroup : DataParams VAR '=' CtorList DataAndRest
        {% do { p <- pos $2
              ; return (SynDataGroup (SynDataDecl p $1 (varTok $2) $4 : $5)) } }

DataAndRest : {- empty -}                       { [] }
   | and DataParams VAR '=' CtorList DataAndRest
        {% do { p <- pos $3
              ; return (SynDataDecl p $2 (varTok $3) $5 : $6) } }

-- Type parameters: none, a single `'a`, or a parenthesized comma list.
DataParams : {- empty -}          { [] }
   | TYVAR                        { [tyvarTok $1] }
   | '(' TyVarList ')'            { $2 }

TyVarList : TYVAR                 { [tyvarTok $1] }
   | TYVAR ',' TyVarList          { tyvarTok $1 : $3 }

CtorList : Ctor                   { [$1] }
   | Ctor '|' CtorList            { $1 : $3 }

Ctor : VAR                        {% do { p <- pos $1
                                        ; return (SynCtor p (varTok $1) Nothing) } }
   | VAR of TyExp                 {% do { p <- pos $1
                                        ; return (SynCtor p (varTok $1) (Just $3)) } }

-- Type expressions (spec §2). Application (juxtaposition) binds tighter than
-- the product `*`; parentheses nest. Products are flat and n-ary.
TyExp : AppTy   %prec TYAPP_LOW   { $1 }
   | AppTy '*' ProdList           { STyProd ($1 : reverse $3) }

ProdList : AppTy   %prec TYAPP_LOW  { [$1] }
   | ProdList '*' AppTy           { $3 : $1 }

AppTy : AtomTy                    { $1 }
   | AppTy QTyName                { STyApp [$1] $2 }

AtomTy : TYVAR                    { STyVar (tyvarTok $1) }
   | QTyName                      { STyName $1 }
   | '(' TyExp ')'               { $2 }
   | '(' TyExp ',' TyArgList ')' QTyName   { STyApp ($2 : reverse $4) $6 }
   | '{' '}'                      { STyRecord [] }
   | '{' TyFieldList '}'          { STyRecord (reverse $2) }

-- Record type fields, accumulated in reverse. Duplicate labels are rejected
-- later, in payload resolution, so the error carries the constructor position.
TyFieldList : VAR ':' TyExp             { [(varTok $1, $3)] }
   | TyFieldList ',' VAR ':' TyExp      { (varTok $3, $5) : $1 }

TyArgList : TyExp                 { [$1] }
   | TyArgList ',' TyExp          { $3 : $1 }

-- A possibly dotted type name: `t`, `X.t`, `X.Y.t`.
QTyName : VAR                     { [varTok $1] }
   | VAR '.' QTyName              { varTok $1 : $3 }


-- Expressions parse as flat operator chains: operands and operators recorded
-- in source order with no grouping. The re-association pass (OpReassoc)
-- rebuilds each chain from the fixity environment once imports are known.
-- Design: _dev_planning/custom-operators/design.md §4.2.
Expr: Chain                        { $1 }
    | catch                        { noLoc (Lit LUnit) }  -- Error recovery
    | Expr ';' Expr               {% mkSeq $1 $3 $2 }

-- The prefix-keyword list is non-empty and spelled out per position: a
-- nullable prefix would force an empty reduction before every operand,
-- which collides with the parenthesized-section productions on '-'
-- ('( - )' vs '( - x )'). With the expansion, every decision is a shift.
Chain : ChainSeq                     %prec CHAIN_DONE  { mkChain (reverse $1) }
      | ChainSeq InfixTok TrailingForm
          { mkChain (reverse (ChOperand $3 : $2 : $1)) }
      | ChainSeq InfixTok PrefixKws TrailingForm
          { mkChain (reverse (ChOperand $4 : ($3 ++ $2 : $1))) }
      | TrailingForm                 %prec CHAIN_DONE
          { mkChain [ChOperand $1] }
      | PrefixKws TrailingForm       %prec CHAIN_DONE
          { mkChain (reverse (ChOperand $2 : $1)) }

-- Built reversed; mkChain reverses back to source order.
ChainSeq : Operand                               { [ChOperand $1] }
         | PrefixKws Operand                     { ChOperand $2 : $1 }
         | ChainSeq InfixTok Operand             { ChOperand $3 : $2 : $1 }
         | ChainSeq InfixTok PrefixKws Operand   { ChOperand $4 : ($3 ++ $2 : $1) }

PrefixKws : PrefixKw                             { [$1] }
          | PrefixKws PrefixKw                   { $2 : $1 }

PrefixKw : 'isTuple'  {% chPrefix $1 IsTuple }
         | 'isList'   {% chPrefix $1 IsList }
         | 'isRecord' {% chPrefix $1 IsRecord }
         | 'not'      {% chPrefix $1 Not }

-- InfixTok is factored: SecOp is every infix operator except '-', which the
-- parenthesized-section production must treat specially ('(' '-' ...) is
-- also the start of a parenthesized negation; an explicit '(' '-' ')'
-- production makes the choice a pure shift decision instead of an LALR
-- conflict on merged lookaheads).
InfixTok : SecOp { $1 }
         | '-'   {% chInfix $1 (ChBin Minus) }

SecOp    : '+'   {% chInfix $1 (ChBin Plus) }
         | '*'   {% chInfix $1 (ChBin Mult) }
         | '/'   {% chInfix $1 (ChBin Div) }
         | div   {% chInfix $1 (ChBin IntDiv) }
         | mod   {% chInfix $1 (ChBin Mod) }
         | '^'   {% chInfix $1 (ChBin Concat) }
         | '='   {% chInfix $1 (ChBin Eq) }
         | '<>'  {% chInfix $1 (ChBin Neq) }
         | '<'   {% chInfix $1 (ChBin Lt) }
         | '>'   {% chInfix $1 (ChBin Gt) }
         | '<='  {% chInfix $1 (ChBin Le) }
         | '>='  {% chInfix $1 (ChBin Ge) }
         | andalso {% chInfix $1 (ChBin And) }
         | orelse  {% chInfix $1 (ChBin Or) }
         | andb  {% chInfix $1 (ChBin BinAnd) }
         | orb   {% chInfix $1 (ChBin BinOr) }
         | xorb  {% chInfix $1 (ChBin BinXor) }
         | '<<'  {% chInfix $1 (ChBin BinShiftLeft) }
         | '>>'  {% chInfix $1 (ChBin BinShiftRight) }
         | '~>>' {% chInfix $1 (ChBin BinZeroShiftRight) }
         | '::'  {% chInfix $1 ChCons }
         | 'raisedTo' {% chInfix $1 (ChBin RaisedTo) }
         | OPSYM {% chInfix $1 (ChUser (opTok $1)) }

-- Operands: application chains (Form), plus the let forms, which are closed
-- by `end` and therefore legal mid-chain.
Operand : Form                                   { $1 }
        | let pini Expr Decs in Expr end  {% atPos $1 (Let (piniDecl $3 $4) $6) }
        | let Decs in Expr end            {% atPos $1 (Let $2 $4) }

-- Open special forms: everything to their right belongs to them, so they can
-- only end a chain; as a left operand they need parentheses (as before).
TrailingForm : if Expr then Expr else Expr {% atPos $1 (If $2 $4 $6) }
    | fn Pattern '=>' Expr        {% atPos $1 (Abs (Lambda [$2] $4)) }
    | hn Pattern '=>' Expr        {% atPos $1 (Hnd (Handler $2 Nothing Nothing $4)) }
    | hn Pattern '|' Pattern '=>' Expr      {% atPos $1 (Hnd (Handler $2 (Just $4) Nothing $6)) }
    | hn Pattern when Expr '=>' Expr        {% atPos $1 (Hnd (Handler $2 Nothing (Just $4) $6)) }
    | hn Pattern '|' Pattern when Expr '=>' Expr      {% atPos $1 (Hnd (Handler $2 (Just $4) (Just $6) $8)) }
    | case Expr of Match          {% atPos $1 (Case $2 $4) }


Match : Pattern '=>' Expr                      { [($1,$3)] }
      | Pattern '=>' Expr '|' Match            { ($1,$3):$5 }
      | ConPat '=>' Expr                       { [($1,$3)] }
      | ConPat '=>' Expr '|' Match             { ($1,$3):$5 }
      -- Error recovery: skip bad case arm content
      | catch                                  { [(noLoc ErrorPattern, noLoc (Lit LUnit))] }


-- Constructor-application patterns (syntactic variants). The qualification is
-- inlined (rather than factored into a nonterminal that reduces from a bare
-- VAR) so that a leading VAR never has a reduce action competing with
-- 'VarPattern'; a constructor is recognised only once an argument pattern or a
-- dotted qualifier follows. Supported forms: bare applied `C p`, qualified
-- applied `T.C p` / `M.T.C p`, and nullary qualified `T.C` / `M.T.C`. A bare
-- nullary constructor is an ordinary variable pattern, resolved later.
ConPat : VAR APat                          {% atPos $1 (ConPattern [varTok $1] (Just $2)) }
       | VAR '.' VAR APat                  {% atPos $1 (ConPattern [varTok $1, varTok $3] (Just $4)) }
       | VAR '.' VAR '.' VAR APat          {% atPos $1 (ConPattern [varTok $1, varTok $3, varTok $5] (Just $6)) }
       | VAR '.' VAR                       {% atPos $1 (ConPattern [varTok $1, varTok $3] Nothing) }
       | VAR '.' VAR '.' VAR               {% atPos $1 (ConPattern [varTok $1, varTok $3, varTok $5] Nothing) }

-- Argument of a constructor pattern: an atomic (self-delimited) pattern. A
-- nested constructor application must be parenthesized here.
APat : VAR                                 {% atPos $1 (VarPattern (varTok $1)) }
     | '_'                                 {% atPos $1 Wildcard }
     | '(' ')'                             {% atPos $1 (ValPattern LUnit) }
     | NUM                                 {% atPos $1 (ValPattern (LNumeric (NumInt (numTok $1)))) }
     | FLOAT                               {% atPos $1 (ValPattern (LNumeric (NumFloat (floatTok $1)))) }
     | STRING                              {% atPos $1 (ValPattern (LString (strTok $1))) }
     | true                                {% atPos $1 (ValPattern (LBool True)) }
     | false                               {% atPos $1 (ValPattern (LBool False)) }
     | LABEL                               {% atPos $1 (ValPattern (LLabel (lblTok $1))) }
     | '`<' DCLabelExp '>`'                {% atPos $1 (ValPattern (LDCLabel $2)) }
     | '(' Pattern ')'                     { $2 }
     | '(' ConPat ')'                      { $2 }
     | '(' CSPattern PatElem ')'           {% atPos $1 (TuplePattern (reverse ($3:$2))) }
     | FieldPattern                        { $1 }
     | BracketListPattern                  { $1 }


Form :: { LTerm }
Form :  '-' Form                    {% atPos $1 (Neg $2) }
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
     | LabelExp                    { ExprComponent $1 }

IntLabelExp :                      { ConstComponent LabelTrue }
     | '#root-integrity'           { ConstComponent LabelFalse }
     | '#null-integrity'           { ConstComponent LabelTrue }
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
     -- A parenthesized operator is a value: the named function for a user
     -- operator, an eta-expanded section for a built-in (fn a => fn b => a + b).
     -- '-' has its own production (see SecOp above).
     | '(' SecOp ')'               {% mkOpAtom $2 }
     | '(' '-' ')'                 {% do { p <- pos $2; mkOpAtom (ChInfix p (ChBin Minus)) } }
     | Atom '.' '(' SecOp ')'      {% mkQualOpAtom $1 $2 $4 }
     | Lit                         { let Loc p l = $1 in Loc p (Lit l) }
     -- A bigint literal desugars to constructing the bigint from its digit
     -- string; bigFromLiteral is total (the lexer guarantees valid digits).
     | BIGNUM                      {% atPos $1 (App (noLoc (Var "bigFromLiteral")) [noLoc (Lit (LString (bigTok $1)))]) }
     | VAR                         {% atPos $1 (Var (varTok $1)) }
     | '(' ')'                     {% atPos $1 (Lit LUnit) }
     | '(' CSExpr Expr ')'         {% atPos $1 (Tuple (reverse ($3:$2)) False) }
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
    | '(' OPSYM ')'                         {% atPos $2 (VarPattern (opTok $2)) }
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
    | '(' CSPattern PatElem ')'             {% atPos $1 (TuplePattern (reverse ($3:$2))) }
    | '(' ConPat ')'                        { $2 }
    | FieldPattern                          { $1 }
    | ListPattern   { $1}

-- An element of a tuple or list pattern: an ordinary pattern or a bare
-- constructor-application pattern. This lets constructor patterns nest inside
-- tuples and lists without parenthesizing each one (e.g. @(SOME x, NONE)@).
PatElem : Pattern                           { $1 }
        | ConPat                            { $1 }


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

-- Bracketed list patterns are self-delimited, so a constructor argument may be
-- one (see 'APat'). The infix cons pattern is not, and is deliberately absent
-- from that layer: were it reachable there, `C p :: rest` would parse as
-- `C (p :: rest)`, silently, since the argument would swallow the cons.
BracketListPattern
    :     '[' ']'                                  {% atPos $1 (ListPattern []) }
    | '[' PatElem ']'                              {% atPos $1 (ListPattern [$2]) }
    | '[' CSPattern PatElem ']'                    {% atPos $1 (ListPattern (reverse ($3:$2))) }

ListPattern
    :     BracketListPattern                       { $1 }
    |     Pattern '::' Pattern                     {% atPos $2 (ConsPattern $1 $3) }


CSPattern : PatElem ','         { [$1] }
    | CSPattern  PatElem ','    { ($2:$1) }


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
           | fun '(' OPSYM ')' FunOptions {% atPos $3 (FunDecl (opTok $3) $5) }
AndFunDecl : and VAR FunOptions {% atPos $2 (FunDecl (varTok $2) $3) }
           | and '(' OPSYM ')' FunOptions {% atPos $3 (FunDecl (opTok $3) $5) }

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

-- | Wrap chain elements (in source order) into a term: a lone operand is
-- itself; anything with an operator or prefix item becomes an OpChain for the
-- re-association pass.
mkChain :: [ChainElem] -> LTerm
mkChain [ChOperand t] = t
mkChain elems@(e : _) = Loc (elemPos e) (OpChain elems)
  where elemPos (ChOperand t)  = getLoc t
        elemPos (ChInfix p _)  = p
        elemPos (ChPrefix p _) = p
mkChain [] = Loc (RTGen "parser") (Lit LUnit)  -- unreachable: grammar yields >= 1 element

chInfix :: L Token -> ChainOp -> ParseM ChainElem
chInfix tok op = do p <- pos tok
                    return (ChInfix p op)

chPrefix :: L Token -> UnaryOp -> ParseM ChainElem
chPrefix tok u = do p <- pos tok
                    return (ChPrefix p u)

-- | A parenthesized operator as a value: a user operator is the named
-- function itself; a built-in becomes an eta-expanded section whose body is
-- a one-operator chain, resolved by re-association like any other chain.
mkOpAtom :: ChainElem -> ParseM LTerm
mkOpAtom (ChInfix p (ChUser v)) = return (Loc p (Var v))
mkOpAtom (ChInfix p op) =
  let a  = Loc p (Var "$opl")
      b  = Loc p (Var "$opr")
      body = Loc p (OpChain [ChOperand a, ChInfix p op, ChOperand b])
      pat v = Loc p (VarPattern v)
      inner = Loc p (Abs (Lambda [pat "$opr"] body))
  in return (Loc p (Abs (Lambda [pat "$opl"] inner)))
mkOpAtom _ = throwError "parser: malformed operator atom"

-- | Qualified prefix access to an operator: @Pretty.( <+> )@ is field
-- projection with the operator name, the path qualified access already
-- takes. Only user operators live in modules; a qualified built-in section
-- is meaningless and reported.
mkQualOpAtom :: LTerm -> L Token -> ChainElem -> ParseM LTerm
mkQualOpAtom receiver dotTok (ChInfix _ (ChUser v)) =
  atPos dotTok (ProjField receiver v)
mkQualOpAtom _ dotTok _ = do
  env <- ask
  let (AlexPn _ line col) = getPos dotTok
  throwError $ peFilename env ++ ":" ++ show line ++ ":" ++ show col
             ++ ": only a user-defined operator can be accessed qualified;"
             ++ " built-in operator sections are written unqualified, e.g. ( + )"

-- | Build a fixity declaration, checking the level range. The level token is
-- passed as the literal NUM lexeme.
mkFixityDecl :: L Token -> OpAssoc -> L Token -> [VarName] -> ParseM FixityDecl
mkFixityDecl kwTok assoc numTok' ops = do
  p <- pos kwTok
  let n = numTok numTok'
  if n < 0 || n > 9
    then do env <- ask
            let (AlexPn _ line col) = getPos numTok'
            throwError $ peFilename env ++ ":" ++ show line ++ ":" ++ show col
                       ++ ": fixity level must be between 0 and 9 (got "
                       ++ show n ++ ")"
    else return (FixityDecl p (Fixity assoc (fromInteger n)) ops)


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
cleanExpectedToken "true" = "'true'"
cleanExpectedToken "false" = "'false'"
cleanExpectedToken "andalso" = "'andalso'"
cleanExpectedToken "orelse" = "'orelse'"
cleanExpectedToken "div" = "'div'"
cleanExpectedToken "mod" = "'mod'"
cleanExpectedToken "VAR" = "identifier"
cleanExpectedToken "OPSYM" = "operator"
cleanExpectedToken "infixl" = "keyword 'infixl'"
cleanExpectedToken "infixr" = "keyword 'infixr'"
cleanExpectedToken "infix" = "keyword 'infix'"
cleanExpectedToken "NUM" = "number"
cleanExpectedToken "BIGNUM" = "bigint literal"
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
bigTok (L _ (TokenBigInt x)) = x
floatTok (L _ (TokenFloat x)) = x
strTok (L _ (TokenString x)) = x
varTok (L _ (TokenSym x ))   = x
opTok (L _ (TokenOperator x)) = x
tyvarTok (L _ (TokenTyVar x)) = x

-- The name a module import binds when no alias is given: the last path segment.
moduleBindName :: String -> String
moduleBindName = reverse . takeWhile (/= '/') . reverse
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

}
