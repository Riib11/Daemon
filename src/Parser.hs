{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Parser where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Char
import           System.IO.Unsafe

import           Lexer

{-
  AST Types
-}

data Expn = ExpnDisj Disj
          | ExpnLet Name Expn Expn
          | ExpnIf Expn Expn Expn
          deriving (Show)

data Disj = DisjOr Disj Conj
          | DisjConj Conj
          deriving (Show)

data Conj = ConjAnd Conj Cmpn
          | ConjCmpn Cmpn
          deriving (Show)

data Cmpn = CmpnEq Addn Addn
          | CmpnLt Addn Addn
          | CmpnAddn Addn
          deriving (Show)

data Addn = AddnPlus Addn Mult
          | AddnMinus Addn Mult
          | AddnMult Mult
          deriving (Show)

data Mult = MultTimes Mult Nega
          | MultDiv   Mult Nega
          | MultMod   Mult Nega
          | MultNega  Nega
          deriving (Show)

data Nega = NegaNot Expo
          | NegaExpo Expo
          deriving (Show)

data Expo = ExpoExpo Atom Expo
          | ExpoAtom Atom
          deriving (Show)

data Atom = AtomVar String
          | AtomInt Int
          | AtomBool Bool
          | AtomAssoc Expn
          deriving (Show)

type Name = String

{-
  Parser
-}

data ParseStatus a = ParseOk a
                   | ParseError String [String]
                   deriving (Functor)

instance Show a => Show (ParseStatus a) where
  show (ParseOk a)         = show a
  show (ParseError msg ss) = "ParseError: "++msg++"; at: ..."++unwords ss++"<end>"

instance Applicative ParseStatus where
  pure a = ParseOk a
  ParseError msg ss <*> _                 = ParseError msg ss
  _                 <*> ParseError msg ss = ParseError msg ss
  ParseOk f         <*> ParseOk a         = ParseOk (f a)

instance Monad ParseStatus where
  ParseOk a         >>= fPb = fPb a
  ParseError msg ss >>= fPb = ParseError msg ss

type Parser a = StateT [String] ParseStatus a

{-
  Grammar Parsers
-}

parse :: [String] -> ParseStatus Expn
parse = evalStateT (do { e <- parseExpn ; parseEnd ; return e })

-- <expn> ::= <disj>
--          | let val <name> = <expn> in <expn> end
--          | if <expn> then <expn> else <expn>
parseExpn :: Parser Expn
parseExpn = do
  getNext >>= \case
    "let" -> do
      parseToken "let"
      parseToken "val"
      x <- parseName
      parseToken "="
      e <- parseExpn
      parseToken "in"
      e' <- parseExpn
      parseToken "end"
      return $ ExpnLet x e e'

    "if" -> do
      parseToken "if"
      e <- parseExpn
      parseToken "then"
      e1 <- parseExpn
      parseToken "else"
      e2 <- parseExpn
      return $ ExpnIf e e1 e2

    _ -> ExpnDisj <$> parseDisj

-- <disj> ::= <disj> orelse <conj>
--          | <conj>
parseDisj :: Parser Disj
parseDisj = DisjConj <$> parseConj >>= recurse where
  recurse e = final e $ do
    tok <- getNext
    if "orelse" == tok then do { parseToken "orelse" ; e' <- parseConj ; recurse (DisjOr e e') }
      else return e

-- <conj> ::= <conj> andalso <cmpn>
--          | <cmpn>
parseConj :: Parser Conj
parseConj = ConjCmpn <$> parseCmpn >>= recurse where
  recurse e = final e $ do
    tok <- getNext
    if "andalso" == tok then do { parseToken "andalso" ; e' <- parseCmpn ; recurse (ConjAnd e e') }
      else return e

-- <cmpn> ::= <addn> = <addn>
--          | <addn> < <addn>
--          | <addn>
parseCmpn :: Parser Cmpn
parseCmpn = do
  e <- parseAddn
  tok <- getNext
  if "=" == tok then do { parseToken "=" ; e' <- parseAddn ; return $ CmpnEq e e' } else
    if "<" == tok then do { parseToken "<" ; e' <- parseAddn ; return $ CmpnLt e e' }
      else return $ CmpnAddn e

-- <addn> ::= <addn> + <mult>
--          | <addn> - <mult>
--          | <mult>
parseAddn :: Parser Addn
parseAddn = AddnMult <$> parseMult >>= recurse where
  recurse :: Addn -> Parser Addn
  recurse e = final e $ do
    tok <- getNext
    if "+" == tok then do { parseToken "+" ; e' <- parseMult ; recurse (AddnPlus e e') } else
      if "-" == tok then do { parseToken "-" ; e' <- parseMult ; recurse (AddnMinus e e') }
        else return e

-- <mult> ::= <mult> * <nega>
--          | <mult> div <nega>
--          | <mult> mod <nega>
--          | <nega>
parseMult :: Parser Mult
parseMult = MultNega <$> parseNega >>= recurse where
  recurse e = final e $ do
    tok <- getNext
    if "*" == tok then do { parseToken "*" ; e' <- parseNega ; recurse (MultTimes e e') } else
      if "div" == tok then do { parseToken "div" ; e' <- parseNega ; recurse (MultDiv e e') } else
        if "mod" == tok then do { parseToken "mod" ; e' <- parseNega ; recurse (MultMod e e') }
          else return e

-- <nega> ::= not <expo>
--          | <expo>
parseNega :: Parser Nega
parseNega = do
  tok <- getNext
  if "not" == tok
    then do
      parseToken "not"
      NegaNot <$> parseExpo
    else
      NegaExpo <$> parseExpo

-- <expo> ::= <atom> ** <expo>
--          | <atom>
parseExpo :: Parser Expo
parseExpo = do
  e <- parseAtom
  final (ExpoAtom e) $ do
    tok <- getNext
    if "**" == tok then do { parseToken "**" ; ExpoExpo e <$> parseExpo }
      else return $ ExpoAtom e

-- <atom> ::= <name>
--          | <int>
--          | <bool>
--          | ( <expn> )
parseAtom :: Parser Atom
parseAtom = do
  tok <- getNext
  if isInt tok then AtomInt <$> parseInt else
    if isBool tok then AtomBool <$> parseBool else
      if isName tok then AtomVar <$> parseName else
        if "(" == tok then AtomAssoc <$> parseAssoc
          else parseError $ "expected <atom>, but found "++show tok

parseInt :: Parser Int
parseInt = do
  tok <- getNext
  if isInt tok
    then do parseToken tok ; return $ read tok
    else parseError $ "expected <int>, but found "++show tok

parseBool :: Parser Bool
parseBool = do
  tok <- getNext
  if isBool tok
    then do parseToken tok ; return $ tok == "true"
    else parseError $ "expected <bool>, but found "++show tok

parseName :: Parser Name
parseName = do
  tok <- getNext
  if isName tok
    then do parseToken tok ; parseToken tok
    else parseError $ "expected <name>, but found "++show tok

parseAssoc :: Parser Expn
parseAssoc = do
  parseToken "("
  e <- parseExpn
  parseToken ")"
  return e

{-
  Token Checks
-}

type TokenCheck = String -> Bool

isInt :: TokenCheck
isInt = all isDigit

isBool :: TokenCheck
isBool = (`elem` ["true", "false"])

isName :: TokenCheck
isName = not . (`elem` reserveds)

{-
  Utility Parsers
-}

parseToken :: String -> Parser String
parseToken tok = do
  tok' <- getNext
  if tok == tok'
    then do modify (drop 1) ; return tok
    else parseError $ "expected "++show tok++" but found "++show tok'

parseEnd :: Parser ()
parseEnd = do
  toks <- get
  unless (null toks) . parseError $ "expected end, but some tokens left unparsed: "++show toks

atEnd :: Parser Bool
atEnd = null <$> get

getNext :: Parser String
getNext =
  get >>= \case
    [] -> parseError "tried to get next token when at end"
    (s:ss) -> return s

parseError :: String -> Parser a
parseError msg = get >>= lift . ParseError msg

final :: a -> Parser a -> Parser a
final a p = atEnd >>= \case True -> return a ; False -> p

{-
  Pretty Printing
-}

indent :: Int -> String
indent i = concat $ replicate i "  "

showPretty :: Expn -> String
showPretty = spTree 0 . treeOf where
  spTree :: Int -> Tree String -> String
  spTree i = \case
    Leaf s -> indent i++s
    Branch s ts -> unlines $ [indent i++s] ++ map (spTree (i+1)) ts

data Tree a = Branch String [Tree a] | Leaf a

treeOf :: Expn -> Tree String
treeOf = tExpn where
  tExpn :: Expn -> Tree String
  tExpn = \case
    ExpnLet n e1 e2 -> Branch "Let" [tName n, tExpn e1, tExpn e2]
    ExpnIf e e1 e2 -> Branch "If" [tExpn e, tExpn e2, tExpn e2]
    ExpnDisj e -> tDisj e

  tDisj = \case
    DisjOr e1 e2 -> Branch "Or" [tDisj e1, tConj e2]
    DisjConj e -> tConj e

  tConj = \case
    ConjAnd e1 e2 -> Branch "And" [tConj e1, tCmpn e2]
    ConjCmpn e -> tCmpn e

  tCmpn = \case
    CmpnEq e1 e2 -> Branch "Equals" [tAddn e1, tAddn e2]
    CmpnLt e1 e2 -> Branch "LessThan" [tAddn e1, tAddn e2]
    CmpnAddn e -> tAddn e

  tAddn = \case
    AddnPlus e1 e2 -> Branch "Plus" [tAddn e1, tMult e2]
    AddnMinus e1 e2 -> Branch "Minus" [tAddn e1, tMult e2]
    AddnMult e -> tMult e

  tMult = \case
    MultTimes e1 e2 -> Branch "Times" [tMult e1, tNega e2]
    MultDiv e1 e2 -> Branch "Div" [tMult e1, tNega e2]
    MultMod e1 e2 -> Branch "Mod" [tMult e1, tNega e2]
    MultNega e -> tNega e

  tNega = \case
    NegaNot e -> Branch "Not" [tExpo e]
    NegaExpo e -> tExpo e

  tExpo = \case
    ExpoExpo e1 e2 -> Branch "Exponent" [tAtom e1, tExpo e2]
    ExpoAtom e -> tAtom e

  tAtom = \case
    AtomVar n -> Branch "Var" [Leaf n]
    AtomInt i -> Branch "Int" [Leaf $ show i]
    AtomBool b -> Branch "Bool" [Leaf $ show b]
    AtomAssoc e -> tExpn e

  tName n = Branch "Var" [Leaf n]
