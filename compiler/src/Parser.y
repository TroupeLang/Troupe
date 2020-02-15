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

    'raisedTo' { L _ TokenRaisedTo }

    '('   { L _ TokenLParen }
    ')'   { L _ TokenRParen }
    ','   { L _ TokenComma }
    '_'   { L _ TokenWildcard }
    '|'   { L _ TokenBar }
    '::'  { L _ TokenColonColon }
    '['   { L _ TokenLBracket }
    ']'   { L _ TokenRBracket }



-- Operators
%right ';'
%left andalso orelse
%nonassoc '=' '<=' '>=' '<>' '<' '>' '@'
%left '+' '-'
%left '*' '/'
%right '::'

%left 'raisedTo'
%left '^'
%%




Prog : ImportDecl AtomsDecl Expr                       { Prog (Imports $1) (Atoms $2) $3 }

ImportDecl: import  VAR ImportDecl { ((LibName (varTok $2), Nothing)): $3  }
          | { [] }


AtomsDecl : datatype Atoms '=' VAR AtomsList    { (varTok $4):$5 }
          |  {[]}

AtomsList : { [] }
          | '|' VAR AtomsList  { (varTok $2): $3 }


Expr:Form                         { $1 }
    | let pini Expr Decs in Expr end  { Let (piniDecl $3 $4)  $6 }
    | let Decs in Expr end        { Let $2 $4 }
    | if Expr then Expr else Expr { If $2 $4 $6 }
    | fn Pattern '=>' Expr                  { Abs (Lambda [$2] $4)}
    | hn Pattern '=>' Expr                  { Hnd (Handler $2 Nothing Nothing $4)}
    | hn Pattern '|' Pattern '=>' Expr      { Hnd (Handler $2 (Just $4) Nothing $6) }
    | hn Pattern when Expr '=>' Expr        { Hnd (Handler $2 Nothing (Just $4) $6)}
    | hn Pattern '|' Pattern when Expr '=>' Expr      { Hnd (Handler $2 (Just $4) (Just $6) $8)}
    | case Expr of Match          { Case $2 $4 (pos $1) }

-- Note there is a shift/reduce conflict in this grammar
-- This is a side-effect of the SML/NJ grammar

Match : Pattern '=>' Expr                      { [($1,$3)] }
      | Pattern '=>' Expr '|' Match            { ($1,$3):$5 }


Form :: { Term }
Form : Form '+' Form               { Bin Plus $1 $3 }
     | Form '-' Form               { Bin Minus $1 $3 }
     | Form '*' Form               { Bin Mult $1 $3 }
     | Form '/' Form               { Bin Div $1 $3 }
     | Form '^' Form               { Bin Concat $1 $3 }
     | Form '=' Form               { Bin Eq $1 $3 }
     | Form '<=' Form              { Bin Le $1 $3 }
     | Form '>=' Form              { Bin Ge $1 $3 }
     | Form '<' Form               { Bin Lt $1 $3 }
     | Form '>' Form               { Bin Gt $1 $3 }
     | Form '<>' Form              { Bin Neq $1 $3 }
     | Form andalso Form           { Bin And $1 $3 }
     | Form orelse  Form           { Bin Or $1 $3 }
     | Form '::' Form              { ListCons $1 $3 }
     | Form ';' Form               { mkSeq $1 $3 }
     | Form 'raisedTo' Form        { Bin RaisedTo $1 $3 }
     | '-' Form                    { Un UnMinus $2 }
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
     | ListExpr                    { $1 }

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
    | ListPattern   { $1}


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
