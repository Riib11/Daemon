{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List                 as List
import           Data.Map                  as Map hiding (foldl)
import           System.IO.Unsafe

import           Lexer                     hiding (lexed)
import           Location
import           Token

{-
  # Grammar Types
-}

data Program = Expression Expression deriving (Show)

data Expression = UnaryOperation UnaryOperator Expression
                | BinaryOperation BinaryOperator Expression Expression
                | Binding Name Expression Expression
                | Branching Expression Expression Expression
                | Atom Atom
                deriving (Show)

data UnaryOperator = Not deriving (Show)

data BinaryOperator = And | Or
                    | Equals | LessThan
                    | Plus | Minus
                    | Times | Divide
                    | Modulus | Exponential
                    deriving (Show)

data Atom = Variable Name
          | Integer Int
          | Boolean Bool
          | Associated Expression
          deriving (Show)

type Name = String

{-
  ## Symbols
-}

class Symbolic a where
  symbol :: a -> Token

instance Symbolic UnaryOperator where
  symbol = \case
    Not -> "not"

instance Symbolic BinaryOperator where
  symbol = \case
    And -> "andalso"
    Or -> "orelse"
    Equals -> "="
    LessThan -> "<"
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Divide -> "/"
    Modulus -> "%"
    Exponential -> "**"

{-
  # ParseState
-}

data ParseState = ParseState
  { _tokenIndex :: Int
  , _lexed      :: Map Location Token }
  deriving (Show)

makeLenses ''ParseState

{-
  # ParseStatus
-}

data ParseStatus a = Ok a | Error Location String

instance Functor ParseStatus where
  f `fmap` Ok a          = Ok (f a)
  f `fmap` Error loc msg = Error loc msg

instance Applicative ParseStatus where
  pure = Ok
  Ok f           <*> Ok a          = Ok (f a)
  Ok _           <*> Error loc msg = Error loc msg
  Error loc msg  <*> _             = Error loc msg

instance Monad ParseStatus where
  Ok a           >>= f = f a
  Error loc msg  >>= _ = Error loc msg

instance Show a => Show (ParseStatus a) where
  show (Ok a)          = show a
  show (Error loc msg) = "[!] Error at "++show loc++" "++msg

{-
  # Parser
-}

type Parser = StateT ParseState ParseStatus

{-
  # Parsing
-}

parse :: Lexed -> ParseStatus Program
parse lxd = evalStateT parseProgram initParseState where
  initParseState = ParseState 0 lxd

parseProgram :: Parser Program
parseProgram = Expression <$> parseExpression


parseExpression :: Parser Expression
parseExpression = getNext >>= \case
  -- let val «Name» = «Expression» in «Expression» end
  "let" -> do
           parseToken "let"
           parseToken "val"
           name <- parseName
           parseToken "="
           value <- parseExpression
           parseToken "in"
           child <- parseExpression
           return $ Binding name value child
  -- if «Expression» then «Expression» else «Expression»
  "if"  -> do
           parseToken "if"
           cnd <- parseExpression
           parseToken "then"
           thn <- parseExpression
           parseToken "else"
           els <- parseExpression
           return $ Branching cnd thn els

  _     -> parseDisjunction

parseDisjunction :: Parser Expression
parseDisjunction = parseLeftAssociativeBinaryOperation [Or] parseConjunction

parseConjunction :: Parser Expression
parseConjunction = parseLeftAssociativeBinaryOperation [And] parseComparison

parseComparison :: Parser Expression
parseComparison = parseLeftAssociativeBinaryOperation [Equals, LessThan] parseAddition

parseAddition :: Parser Expression
parseAddition = parseLeftAssociativeBinaryOperation [Plus, Minus] parseMultiplication

parseMultiplication :: Parser Expression
parseMultiplication = parseLeftAssociativeBinaryOperation [Times, Divide, Modulus] parseNegation

parseNegation :: Parser Expression
parseNegation = parseUnaryOperation [Not] parseExponential

parseExponential :: Parser Expression
parseExponential = parseRightAssociativeBinaryOperation [Exponential] (Atom <$> parseAtom)

parseAtom :: Parser Atom
parseAtom = do
  tok <- getNext
  let f :: Maybe Atom -> Parser (Maybe Atom) -> Parser (Maybe Atom)
      f Nothing  tryParse = tryParse
      f (Just a) _        = return $ Just a
  foldM f Nothing [ tryParseIntAtom
                  , tryParseBoolAtom
                  , tryParseVariableAtom
                  , tryParseAssociatedAtom ] >>= \case
    Nothing -> errorUnexpected "<atom>" tok
    Just a -> return a

tryParseIntAtom :: Parser (Maybe Atom)
tryParseIntAtom = do
  tok <- getNext
  if all isDigit tok
    then do parseNext ; return . Just $ Integer (read tok :: Int)
    else return Nothing

tryParseBoolAtom :: Parser (Maybe Atom)
tryParseBoolAtom =
  getNext >>= \case
    "true" -> do parseNext ; return . Just $ Boolean True
    "false" -> do parseNext ; return . Just $ Boolean False
    _ -> return Nothing

tryParseVariableAtom :: Parser (Maybe Atom)
tryParseVariableAtom = do
  tok <- getNext
  case tokenTypeOf tok of
    Normal -> return . Just $ Variable tok
    _      -> return Nothing

tryParseAssociatedAtom :: Parser (Maybe Atom)
tryParseAssociatedAtom = do
  getNext >>= \case
    "(" -> do parseNext ; e <- parseExpression ; parseToken ")" ; return . Just $ Associated e
    _ -> return Nothing

parseName :: Parser Name
parseName = parseNext

{-
  ## Parsing Helpers
-}


-- TODO: right now, this is actually right-associative
parseLeftAssociativeBinaryOperation :: [BinaryOperator] -> Parser Expression -> Parser Expression
parseLeftAssociativeBinaryOperation os p = do
  let recurse e = do
        getMaybeNext >>= \case
          Nothing -> return e
          Just tok -> do
            let f (Just o) _ = Just o
                f Nothing  o = if symbol o == tok then Just o else Nothing
            case foldl f Nothing os of
              Just o  -> do
                         parseToken (symbol o)
                         e' <- p
                         recurse (BinaryOperation o e e')
              Nothing -> return e
    in do e <- p ; recurse e


parseRightAssociativeBinaryOperation :: [BinaryOperator] -> Parser Expression -> Parser Expression
parseRightAssociativeBinaryOperation os p = do
  e <- p
  getMaybeNext >>= \case
    Nothing -> return e
    Just tok -> do
      let f (Just o) _ = Just o
          f Nothing  o = if symbol o == tok then Just o else Nothing
      case foldl f Nothing os of
        Just o  -> do
                   parseToken (symbol o)
                   BinaryOperation o e <$> parseRightAssociativeBinaryOperation os p
        Nothing -> return e

parseUnaryOperation :: [UnaryOperator] -> Parser Expression -> Parser Expression
parseUnaryOperation os p =
  getMaybeNext >>= \case
    Nothing -> p
    Just tok -> do
      let f (Just o) _ = Just o
          f Nothing  o = if symbol o == tok then Just o else Nothing
      case foldl f Nothing os of
        Just o  -> do parseToken (symbol o) ; UnaryOperation o <$> p
        Nothing -> p

{-
  ## Parsing Utilities
-}

getNext :: Parser Token
getNext = do
  lexed <- use lexed
  loc <- getLocation
  return $ lexed!loc

getMaybeNext :: Parser (Maybe Token)
getMaybeNext = atEndOfInput >>= \case
  True -> return Nothing
  False -> Just <$> getNext

parseNext :: Parser Token
parseNext = do
  tok <- getNext
  incrementLocation
  return tok

tryParseToken :: Token -> Parser (Maybe Token)
tryParseToken tok = do
  tok' <- getNext
  if tok == tok'
    then Just <$> parseNext
    else return Nothing

parseToken :: Token -> Parser Token
parseToken tok = do
  tok' <- getNext
  if tok == tok'
    then parseNext
    else errorUnexpected tok tok'

parseEndOfInput :: Parser ()
parseEndOfInput = do
  atEndOfInput >>= \case
    True -> return ()
    False -> errorOutOfBounds

atEndOfInput :: Parser Bool
atEndOfInput = not <$> inBounds

getLocation :: Parser Location
getLocation = do
  i <- use tokenIndex
  inBounds >>= \case
    True -> uses lexed (keys >>> (!!i))
    False -> errorOutOfBounds

inBounds :: Parser Bool
inBounds = do
  i <- use tokenIndex
  l <- uses lexed (length . keys)
  return $ i < l

incrementLocation :: Parser ()
incrementLocation = tokenIndex += 1

{-
  ## Parsing Errors
-}

errorUnexpected :: Token -> Token -> Parser a
errorUnexpected tokExp tokUnexp = errorHere $ "unexpected: "++show tokUnexp++", expected: "++show tokExp

errorOutOfBounds :: Parser a
errorOutOfBounds = errorHere $ "attempted to parse past end of input"

errorHere :: String -> Parser a
errorHere msg = do
  loc <- getLocation
  lift $ Error loc msg
