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
import           Data.Map

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
parseDisjunction = do
  e <- parseConjunction
  getMaybeNext >>= \case
    Just "orelse" -> BinaryOperation Or e <$> parseDisjunction
    _ -> return e

parseConjunction :: Parser Expression
parseConjunction = error "TODO"

parseComparison :: Parser Expression
parseComparison = error "TODO"

parseAddition :: Parser Expression
parseAddition = error "TODO"

parseMultiplication :: Parser Expression
parseMultiplication = error "TODO"

parseNegative :: Parser Expression
parseNegative = error "TODO"

parseExponential :: Parser Expression
parseExponential = error "TODO"

parseAtom :: Parser Atom
parseAtom = error "TODO"

parseName :: Parser Name
parseName = parseNext

{-
  ## Parsing Utilities
-}

getNext :: Parser Token
getNext = do
  lexed <- use lexed
  loc <- getLocation
  return $ lexed ! loc

getMaybeNext :: Parser (Maybe Token)
getMaybeNext = atEndOfInput >>= \case
  True -> return Nothing
  False -> Just <$> getNext

parseNext :: Parser Token
parseNext = do
  tok <- getNext
  incrementLocation
  return tok

parseToken :: Token -> Parser Token
parseToken tok = do
  tok' <- getNext
  if tok == tok'
    then parseNext
    else errorUnexpected tok tok'

atEndOfInput :: Parser Bool
atEndOfInput = not <$> inBounds

getLocation :: Parser Location
getLocation = inBounds >>= \case
  True -> uses lexed (keys >>> (!i))
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
errorUnexpected tokExp toUnexp = errorHere $ "unexpected: "++show tokExp++", expected: "++show tokExp

errorOutOfBounds :: Parser a
errorOutOfBounds = errorHere $ "attempted to parse past end of input"

errorHere :: String -> Parser a
errorHere msg = do
  loc <- getLocation
  lift . Error loc msg
