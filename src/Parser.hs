{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Parser where

import           Control.Monad.Trans
import           Control.Monad.Trans.State

import           Lexer

{-
  AST Types
-}

data Expn = ExpnDisj Disj
          | ExpnLet Name Expn Expn
          | ExpnIf Expn Expn Expn

data Disj = DisjOr Disj Conj
          | DisjConj Conj

data Conj = ConjAnd Conj Cmpn
          | ConjCmpn

data Cmpn = CmpnEq Addn Addn
          | CmpnLt Addn Addn
          | CmpnAddn Addn

data Addn = AddnPlus Addn Addn
          | AddnMinus Addn Addn
          | AddnMult Mult

data Mult = MultTimes Mult Nega
          | MultDiv   Mult
          | MultMod   Mult Nega
          | MultNega  Nega

data Nega = NegaNot Expo
          | NegaExpo Expo

data Expo = ExpoExpo Atom Expo
          | ExpoAtom Atom

data Atom = AtomVar String
          | AtomInt Int
          | AtomBool Bool
          | AtomExpn Expn

type Name = String

{-
  Parser
-}

parse = parseExpn

data ParseStatus a = ParseOk a
                   | ParseError String
                   deriving (Functor, Show)

instance Applicative ParseStatus where
  pure a = ParseOk a
  ParseError msg <*> _              = ParseError msg
  _              <*> ParseError msg = ParseError msg
  ParseOk f      <*> ParseOk a      = ParseOk (f a)

instance Monad ParseStatus where
  ParseOk a      >>= fPb = fPb a
  ParseError msg >>= fPb = ParseError msg

type Parser a = StateT [String] ParseStatus a

{-
  Grammar Parsers
-}

parseToken :: String -> Parser String
parseToken tok = do
  tok' <- next
  if tok == tok'
    then return tok
    else parseError $ "expected "++show tok++" but found "++show tok'

parseExpn :: Parser Expn
parseExpn = do
  next >>= \case
    "let" -> do
      parseToken "let"
      parseToken "val"
      x <- parseName
      parseToken "="
      e <- parseExpn
      parseToken "in"
      ep <- parseExpn
      parseToken "end"
      return $ ExpnLet x e ep

    "if" -> do
      parseToken "if"
      e <- parseExpn
      parseToken "then"
      e1 <- parseExpn
      parseToken "else"
      e2 <- parseExpn
      return $ ExpnIf e e1 e2

    _ -> ExpnDisj <$> parseDisj

parseDisj :: Parser Disj
parseDisj = do fail "TODO"

parseConj :: Parser Conj
parseConj = do fail "TODO"

parseCmpn :: Parser Cmpn
parseCmpn = do fail "TODO"

parseAddn :: Parser Addn
parseAddn = do fail "TODO"

parseMult :: Parser Mult
parseMult = do fail "TODO"

parseNega :: Parser Nega
parseNega = do fail "TODO"

parseExpo :: Parser Expo
parseExpo = do fail "TODO"

parseAtom :: Parser Atom
parseAtom =
  

parseName :: Parser Name
parseName = do
  tok <- next
  if isName tok
    then parseToken tok
    else parseError $ "expected <name>, but found "++show tok

{-
  Checks
-}

type TokenCheck = String -> Bool

isName :: TokenCheck
isName = not . (`elem` reserveds)

{-
  Utility Parsers
-}

end :: Parser Bool
end =
  get >>= \case
    [] -> return True
    _  -> return False

next :: Parser String
next =
  get >>= \case
    [] -> parseError "tried to get next token when at end"
    (s:ss) -> return s

parseError :: String -> Parser a
parseError = lift . ParseError
