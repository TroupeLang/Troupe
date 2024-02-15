{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseProg,
  parseTokens,
) where

import Lexer
import Direct
import Basics
import TroupePositionInfo

import Control.Monad.Except

}

-- Entry point
%name prog

-- Lexer structure
%tokentype { L Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

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
    true  { L _ TokenTrue }
    false { L _ TokenFalse }
    andalso { L _ TokenAndAlso }
    orelse  { L _ TokenOrElse }
    NUM   { L _ (TokenNum _) }
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



    'raisedTo' { L _ TokenRaisedTo }
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
    '{'   { L _ TokenLBrace }
    '}'   { L _ TokenRBrace }



-- Operators


%nonassoc with
%right '=>' 
%right '|'
%right ';'
%right else 
%left andalso orelse
%nonassoc '=' '<=' '>=' '<>' '<' '>' '@'
%left andb orb xorb
%left '<<' '>>' '~>>'
%left '+' '-' 
%left '*' '/' div mod
%right '::'
%right '.'

%left 'raisedTo'
%left 'isTuple'
%left 'isList'
%left 'isRecord'
%left '^'
%%




Prog : ImportDecl AtomsDecl Expr                       { Prog (Imports $1) (Atoms $2) $3 }

ImportDecl: import  VAR ImportDecl { ((LibName (varTok $2), Nothing)): $3  }
          | { [] }


AtomsDecl : datatype Atoms '=' VAR AtomsList    { (varTok $4):$5 }
          |  {[]}

AtomsList : { [] }
          | '|' VAR AtomsList  { (varTok $2): $3 }


Expr: Form                        { $1 }
    | let pini Expr Decs in Expr end  { Let (piniDecl $3 $4)  $6 }
    | let Decs in Expr end        { Let $2 $4 }
    | if Expr then Expr else Expr { If $2 $4 $6 }
    | fn Pattern '=>' Expr                  { Abs (Lambda [$2] $4)}
    | hn Pattern '=>' Expr                  { Hnd (Handler $2 Nothing Nothing $4)}
    | hn Pattern '|' Pattern '=>' Expr      { Hnd (Handler $2 (Just $4) Nothing $6) }
    | hn Pattern when Expr '=>' Expr        { Hnd (Handler $2 Nothing (Just $4) $6)}
    | hn Pattern '|' Pattern when Expr '=>' Expr      { Hnd (Handler $2 (Just $4) (Just $6) $8)}
    | case Expr of Match          { Case $2 $4 (pos $1) }
    | Expr ';' Expr               { mkSeq $1 $3 }
    | Expr '-' Expr               { Bin Minus $1 $3 }
    | Expr '+' Expr               { Bin Plus $1 $3 }
    | Expr '>=' Expr              { Bin Ge $1 $3 }
    | Expr '*' Expr               { Bin Mult $1 $3 }
    | Expr '/' Expr               { Bin Div $1 $3 }
    | Expr div Expr             { Bin IntDiv $1 $3}
    | Expr mod Expr             { Bin Mod $1 $3}
    | Expr '^' Expr               { Bin Concat $1 $3 }
    | Expr '=' Expr               { Bin Eq $1 $3 }
    | Expr '<=' Expr              { Bin Le $1 $3 }
     
    | Expr '<' Expr               { Bin Lt $1 $3 }
    | Expr '>' Expr               { Bin Gt $1 $3 }
    | Expr '<>' Expr              { Bin Neq $1 $3 }
    | Expr andalso Expr           { Bin And $1 $3 }
    | Expr orelse  Expr           { Bin Or $1 $3 }

    | Expr andb Expr              { Bin BinAnd $1 $3 }
    | Expr orb Expr               { Bin BinOr $1 $3 }
    | Expr xorb Expr              { Bin BinXor $1 $3 }
    | Expr '<<' Expr              { Bin BinShiftLeft $1 $3 }
    | Expr '>>' Expr              { Bin BinShiftRight $1 $3 }
    | Expr '~>>' Expr             { Bin BinZeroShiftRight $1 $3 }
    | Expr '::' Expr              { ListCons $1 $3 }
    | Expr 'raisedTo' Expr        { Bin RaisedTo $1 $3 }
    | 'isTuple' Expr              { Un IsTuple $2 }
    | 'isList' Expr              { Un IsList $2 }
    | 'isRecord' Expr              { Un IsRecord $2 }


Match : Pattern '=>' Expr                      { [($1,$3)] }
      | Pattern '=>' Expr '|' Match            { ($1,$3):$5 }


Form :: { Term }
Form :  '-' Form                    { Un UnMinus $2 }
     | Fact                        { fromFact $1 }


Fact : Fact Atom                   { $2 : $1 }
     | Atom                        { [$1] }




Lit:   NUM                         { LInt (numTok $1) (pos $1) }
     | STRING                      { LString (strTok $1) }
     | true                        { LBool True }
     | false                       { LBool False }
     | LABEL                       { LLabel (lblTok $1) }


Atom : '(' Expr ')'                { $2 }
     | Lit                         { Lit $1 }
     | VAR                         { Var (varTok $1) }
     | '(' ')'                     { Lit LUnit }
     | '(' CSExpr Expr ')'         { Tuple (reverse ($3:$2)) }
     | '{' '}'                     { Record [] }
     | RecordExpr                  { $1 }
     | ListExpr                    { $1 }
     | Atom '.' VAR                { ProjField $1 (varTok $3) }
     | Atom '.' NUM                { ProjIdx $1 $ fromInteger (numTok $3) }


RecordExpr 
     : '{' RecordFields  '}'                           { Record $2 }
     | '{' Atom with RecordFields'}'                   { WithRecord $2 $4 }
     

RecordFields
     : Field                           { [$1] }
     | Field ',' RecordFields          { $1 : $3 }


Field 
     : VAR                         { (varTok $1, Nothing) }
     | VAR '=' Expr                { (varTok $1, Just $3) }
     


ListExpr :: {Term}
ListExpr : '[' ']'                 { List []   }
     | '[' Expr ']'                { List [$2] }
     | '[' CSExpr Expr ']'         { List (reverse ($3:$2)) }

CSExpr : Expr ','                  { [$1] }
     | CSExpr Expr ','             { ($2:$1) }


Pattern : VAR                               { VarPattern (varTok $1) }
    | '(' Pattern ')'                       { $2 }
    | Pattern '@' LABEL                     { AtPattern $1 (lblTok $3) }
    | '(' ')'                               { ValPattern LUnit }
    | '_'                                   { Wildcard }
    | Lit                                   { ValPattern $1 }
    | '(' CSPattern Pattern ')'             { TuplePattern (reverse ($3:$2)) }
    | '{' FieldPatterns '}'                 { RecordPattern $2 }
    | ListPattern   { $1}


FieldPatterns
    : FieldPat                         { [$1] }
    | FieldPat ',' FieldPatterns       { $1: $3 } 

FieldPat 
    : VAR              {(varTok $1, Nothing) }
    | VAR '=' Pattern  {(varTok $1, Just $3) }

ListPattern:  '[' ']'                              { ListPattern [] }
    | '[' Pattern ']'                              { ListPattern [$2] }
    | '[' CSPattern Pattern ']'                    { ListPattern (reverse ($3:$2)) }
    |     Pattern '::' Pattern                     { ConsPattern  $1 $3 }


CSPattern : Pattern ','         { [$1] }
    | CSPattern  Pattern ','    { ($2:$1) }


Dec : val Pattern '=' Expr      { ValDecl $2 $4 (pos $1 )}
    | FunDecs                      { FunDecs $1 }

Decs : Dec                         { [$1] }
     | Dec Decs                    { $1 : $2 }

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


FunDecl    : fun VAR FunOptions { FunDecl (varTok $2) $3 (pos $2) }
AndFunDecl : and VAR FunOptions { FunDecl (varTok $2) $3 (pos $2) }

FunArgs : Pattern                        { [$1]  }
        | Pattern FunArgs                { $1 : $2}

{


piniDecl auth decs = 
    let pushDecl = ValDecl (VarPattern "$pini") (App  (Var "pinipush") [auth] ) (RTGen "parser")
        popDecl  = ValDecl Wildcard (App (Var "pinipop") [Var "$pini"]) (RTGen "parser")
    in 
        (pushDecl:decs) ++ [popDecl]

mkSeq ::Term -> Term ->Term
mkSeq t1 t2 =
    let ts = case t2 of (Seq ts) -> ts
                        _ -> [t2]
    in Seq (t1: ts)


fromFact [x] = x
fromFact xs =
  let (y:ys) = reverse xs
  in App y ys


parseError :: [L Token] -> Except String a
parseError (l:ls) = do
    let (AlexPn _ line col) = getPos l
    let tks = unPos l 
    throwError $ show line ++ ":" ++ show col  ++ " unexpected token " ++ (show tks)
parseError [] = throwError "Unexpected end of input"


parseTokens :: String -> Either String [L Token]
parseTokens = runExcept . scanTokens


parseProg :: String -> Either String Prog
parseProg input = runExcept $ do
  tokenStream <- scanTokens input
  prog tokenStream


numTok (L _ (TokenNum x))    = x
strTok (L _ (TokenString x)) = x
varTok (L _ (TokenSym x ))   = x
lblTok (L _ (TokenLabel x))  = x

pos l = let (AlexPn _ line col ) = getPos l 
        in SrcPosInf "" line col 


}
