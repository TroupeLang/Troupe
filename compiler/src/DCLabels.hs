{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module DCLabels
  ( DCLabelExp(..)
  , LabelExp (..)
  , LabelOp(..)
  , LabelConst(..)
  , LabelComponent(..)
  , ppDCLabelExp
  , ppDCLabelExpLit
  , labelExpToCNF
  , mkCNF
  , dcLabelExpToDCLabel
  , dcLabelEq
  , cnfEq
  , cnfImplies
  , v1LabelEq
  , v1LabelToDCLabelExp
  , CNF(..)
  , DisjTags(..)
  , DCLabel(..)) where
import GHC.Generics(Generic)
import Sexp
import Data.List (sort, nub, dropWhileEnd)
import Data.List.Utils (split)
import Data.Char (toLower, isSpace)
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (text, hsep)
import Data.Aeson

type Tag = String
data LabelOp = Conj | Disj
     deriving (Eq, Generic, Ord)

data LabelExp
     = TagExp Tag
     | OpExp LabelOp LabelExp LabelExp
     deriving (Eq, Generic, Ord)



data LabelConst = LabelTrue | LabelFalse 
     deriving (Eq, Generic, Ord)

instance Show LabelConst where 
     show LabelTrue = "#true"
     show LabelFalse = "#false"


newtype DisjTags = DisjTags [Tag]
     deriving (Eq, Generic, Ord, Show)

newtype CNF = CNF [DisjTags]
     deriving (Eq, Generic, Ord, Show)

--- Normalization and conversion from labelExp to CNF 
--- 

--- Auxiliary functions 
lowerString = map toLower
snub = sort.nub


--- Conversion from labelExps to CNF

labelExpToCNF :: LabelExp -> CNF
labelExpToCNF (TagExp t) = CNF [DisjTags [lowerString t]]
labelExpToCNF (OpExp op e1 e2) =
    let CNF c1 = labelExpToCNF e1
        CNF c2 = labelExpToCNF e2
    in CNF $ nub $
         case op of
           Conj -> c1 ++ c2
           Disj ->
             [DisjTags $ snub (d1 ++ d2)
                 | DisjTags d1 <- c1, DisjTags d2 <- c2 ]

-- | Build a CNF directly from clauses of principal strings, applying the same
-- syntactic normalization as 'labelExpToCNF': lowercase each tag, then sort and
-- dedup within each clause ('snub'), then dedup the clauses ('nub'). An empty
-- clause list yields @CNF []@ (TRUE); a list containing an empty clause yields a
-- CNF with an empty 'DisjTags' (FALSE), matching 'labelConstToCNF'.
mkCNF :: [[String]] -> CNF
mkCNF clauses = CNF $ nub [ DisjTags (snub (map lowerString c)) | c <- clauses ]

newtype DCLabel = DCLabel (CNF,CNF)
     deriving (Eq, Generic, Ord, Show)


-- DCLabelExp corresponds to the label as it appears in the source; we
-- therefore keep the string representation for potential use in error
-- reporting (2025-05-13; AA)

-- data DCLabelExp = DCLabelExp String (LabelExp, LabelExp)
data LabelComponent
    = ExprComponent LabelExp
    | ConstComponent LabelConst
    deriving (Eq, Generic, Ord)

newtype DCLabelExp =
     DCLabelExp (LabelComponent, LabelComponent)
        deriving (Eq, Generic, Ord)

labelConstToCNF :: LabelConst -> CNF 
labelConstToCNF (LabelTrue) = CNF []
labelConstToCNF (LabelFalse) = CNF [DisjTags []]

dcLabelExpToDCLabel :: DCLabelExp -> DCLabel
dcLabelExpToDCLabel (DCLabelExp (e1,e2)) =
    let f e = case e of
                 ExprComponent le -> labelExpToCNF le
                 ConstComponent lc -> labelConstToCNF lc
    in DCLabel(f e1, f e2)


-- | Semantic equality for DCLabelExp (compare via normalized CNF)
dcLabelEq :: DCLabelExp -> DCLabelExp -> Bool
dcLabelEq d1 d2 =
    let DCLabel (c1, i1) = dcLabelExpToDCLabel d1
        DCLabel (c2, i2) = dcLabelExpToDCLabel d2
    in cnfEq c1 c2 && cnfEq i1 i2

-- | Semantic equality for CNF (bidirectional implication)
cnfEq :: CNF -> CNF -> Bool
cnfEq x y = cnfImplies x y && cnfImplies y x

-- | Semantic implication for CNF formulas.
--
-- For positive CNF, a clause C₁ implies clause C₂ iff literals(C₁) ⊆ literals(C₂),
-- and a CNF F₁ implies F₂ iff every clause in F₂ is subsumed by some clause in F₁.
--
-- See: Stefan et al., "Disjunction Category Labels", NordSec 2011
-- https://link.springer.com/chapter/10.1007/978-3-642-29615-4_16
cnfImplies :: CNF -> CNF -> Bool
cnfImplies (CNF xClauses) (CNF yClauses) =
    all (\yClause -> any (\xClause -> disjSubsetOf xClause yClause) xClauses) yClauses
  where
    -- A disjunction x is a subset of y if all tags in x appear in y
    -- (meaning x is more specific than y, so x implies y)
    disjSubsetOf (DisjTags xs) (DisjTags ys) = all (`elem` ys) xs


-- instance Show DCLabelExp where 
--     show (DCLabelExp s ) = s 

instance Show LabelOp where
  show Conj = "&"
  show Disj = "|"


opPrec :: LabelOp -> Int
opPrec Conj = 100
opPrec Disj = 10


-- pretty printing 
--

ppLabelExp' :: Int -> LabelExp -> PP.Doc 
ppLabelExp' _ (TagExp t) = text t 
ppLabelExp' parenPrec (OpExp o e1 e2) = 
    let thisPrec = opPrec o 
        thisTxt = (text.show) o 
        p1 = ppLabelExp' thisPrec e1 
        p2 = ppLabelExp' thisPrec e2 
    in PP.maybeParens (thisPrec < parenPrec) $ 
           hsep [ p1, thisTxt, p2 ]

ppLabelExp :: LabelExp -> PP.Doc 
ppLabelExp = ppLabelExp' 0


ppDCLabelExp :: DCLabelExp -> PP.Doc
ppDCLabelExp (DCLabelExp (e1, e2))  =
     hsep [ text "<"
          , ppMLabelExp confConst e1
          , text ";"
          , ppMLabelExp intConst e2
          , text ">"
          ]
        where
          -- Constant components are rendered with the dimension-specific
          -- spellings the grammar accepts (ConfLabelExp / IntLabelExp in
          -- Parser.y), not `show`'s display-only #true/#false, so the printed
          -- label reparses to the same DCLabelExp.
          ppMLabelExp _        (ExprComponent e) = ppLabelExp e
          ppMLabelExp constText (ConstComponent s) = text (constText s)
          confConst LabelTrue  = "#null-confidentiality"
          confConst LabelFalse = "#root-confidentiality"
          intConst  LabelTrue  = "#null-integrity"
          intConst  LabelFalse = "#root-integrity"

ppDCLabelExpLit e = 
     text "`" PP.<> (ppDCLabelExp e) PP.<> text "`"


instance Show LabelExp where 
     show = PP.render. ppLabelExp

instance Show DCLabelExp where 
     show = PP.render . ppDCLabelExp

instance ToJSON DisjTags where 
     toJSON (DisjTags ts) = toJSON ts 
instance ToJSON CNF where 
     toJSON (CNF cats) = 
          toJSON (map toJSON cats)

instance ToJSON DCLabel where
     toJSON ( DCLabel (c, i)) =
          object [ "confidentiality" .= c
                 , "integrity" .= i]


-------------------------------------------------------
-- V1 Label support
-- V1 labels like "{alice, bob}" are syntactic sugar for
-- DC labels "<alice & bob ; alice & bob>"
-------------------------------------------------------

-- | Semantic equality for V1 label strings
-- V1 labels like "{alice, bob}" are semantically equivalent to "{bob, alice}"
v1LabelEq :: String -> String -> Bool
v1LabelEq l1 l2 = normalizeV1Label l1 == normalizeV1Label l2

-- | Normalize V1 label string for semantic comparison
-- Parses comma-separated principal names, normalizes them
-- (lowercase, trimmed, sorted, deduplicated)
normalizeV1Label :: String -> [String]
normalizeV1Label s = snub $ filter (not . null) $ map (lowerString . trim) $ split "," (stripBraces s)
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    stripBraces = dropWhileEnd (== '}') . dropWhile (== '{')

-- | Convert V1 label string to DCLabelExp for cross-syntax comparison
-- V1 "{}" means IFC_BOT = <True; False> (most public, least trusted)
-- V1 "{alice, bob}" means <alice & bob ; alice & bob>
v1LabelToDCLabelExp :: String -> DCLabelExp
v1LabelToDCLabelExp s =
    let tags = normalizeV1Label s
    in case tags of
        []  -> DCLabelExp (ConstComponent LabelTrue, ConstComponent LabelFalse)  -- IFC_BOT
        [t] -> let e = ExprComponent (TagExp t) in DCLabelExp (e, e)
        ts  -> let e = ExprComponent (foldr1 (\a b -> OpExp Conj a b) (map TagExp ts))
               in DCLabelExp (e, e)

------------------------------------------------------------
-- s-expression serialization (see "Sexp")
------------------------------------------------------------

instance Sexp DCLabelExp where
  toSexp (DCLabelExp (c, i)) = Lst [Atom "dclabel", toSexp c, toSexp i]
  fromSexp (Lst [Atom "dclabel", c1, c2]) = do
    a <- fromSexp c1
    b <- fromSexp c2
    Right (DCLabelExp (a, b))
  fromSexp d = Left ("expected (dclabel COMPONENT COMPONENT), got " ++ headHint d)

instance Sexp LabelComponent where
  toSexp (ConstComponent LabelTrue)  = Atom "#true"
  toSexp (ConstComponent LabelFalse) = Atom "#false"
  toSexp (ExprComponent le)          = toSexp le
  fromSexp (Atom "#true")  = Right (ConstComponent LabelTrue)
  fromSexp (Atom "#false") = Right (ConstComponent LabelFalse)
  fromSexp d               = ExprComponent <$> fromSexp d

instance Sexp LabelExp where
  toSexp (TagExp t)       = Lst [Atom "tag", Str t]
  toSexp (OpExp Conj a b) = Lst [Atom "and", toSexp a, toSexp b]
  toSexp (OpExp Disj a b) = Lst [Atom "or", toSexp a, toSexp b]
  fromSexp (Lst [Atom "tag", sD])     = TagExp <$> asName sD
  fromSexp (Lst (Atom "and" : args))  = nary Conj args
  fromSexp (Lst (Atom "or"  : args))  = nary Disj args
  fromSexp d = Left ("not a valid label expression, got " ++ headHint d)

-- | Decode an @and@/@or@ form. Accepts n-ary sugar (right-nested); the
-- encoder only ever emits the binary form.
nary :: LabelOp -> [Datum] -> Either String LabelExp
nary op ds = do
  es <- mapM fromSexp ds
  case es of
    (x : y : rest) -> Right (foldr1 (OpExp op) (x : y : rest))
    _              -> Left "and/or requires at least two operands"
