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
import           Data.Map                  as Map hiding (foldl, map)
import           System.IO.Unsafe

import           Lexer                     hiding (lexed)
import           Location
import           Token

{-
  # Grammar Types
-}

data Program
  = Expression Expression
  deriving (Show, Eq, Ord)

data Expression
  = Binding Name Expression Expression
  | Application Expression Expression
  | Lambda Name Expression
  | Branching Expression Expression Expression
  | BinaryOperation BinaryOperator Expression Expression
  | UnaryOperation UnaryOperator Expression
  | Literal Token
  | Associated Expression
  deriving (Show, Eq, Ord)

data UnaryOperator = Not deriving (Show, Eq, Ord)

data BinaryOperator
  = And | Or
  | Equals | LessThan
  | Plus | Minus
  | Times | Divide
  | Modulus | Exponential
  deriving (Show, Eq, Ord)

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

data ParseStatus a = Ok a | Error (Location, String)

instance Functor ParseStatus where
  f `fmap` Ok a    = Ok (f a)
  f `fmap` Error e = Error e

instance Applicative ParseStatus where
  pure = Ok
  Ok f    <*> Ok a    = Ok (f a)
  Ok _    <*> Error e = Error e
  Error e <*> _       = Error e

instance Monad ParseStatus where
  Ok a    >>= f = f a
  Error e >>= _ = Error e

instance Show a => Show (ParseStatus a) where
  show (Ok a)             = show a
  show (Error (loc, msg)) = "[!] Error at "++show loc++", "++msg

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

{-
  ## Parsing Program
-}

parseProgram :: Parser Program
parseProgram = do
  prgm <- Expression <$> parseExpression
  parseEndOfInput
  return prgm

{-
  ## Parsing Steps
-}

parseExpression :: Parser Expression
parseExpression = do
  -- return $! unsafePerformIO (print "parseExpression")
  parseApplication
  $ parseStructural
  $ parseLambda
  $ parseDisjunction
  $ parseConjunction
  $ parseComparison
  $ parseAddition
  $ parseMultiplication
  $ parseNegation
  $ parseExponential
  $ parseLiteral


parseApplication :: Parser Expression -> Parser Expression
parseApplication p' = do
  -- return $! unsafePerformIO (print "parseApplication")
  e <- p'
  atEndOfInput >>= \case
    True -> return e
    False -> tryParse (parseApplication p') >>= \case
      -- «Expression» «Expression»
      Just e' -> return $ Application e e'
      --
      Nothing -> return e


parseStructural :: Parser Expression -> Parser Expression
parseStructural p' = getNext >>= \case
  -- let val «Name» = «Expression» in «Expression» end
  "let" -> do
           parseToken "let"
           parseToken "val"
           name <- parseName
           parseToken "="
           value <- p'
           parseToken "in"
           child <- p'
           return $ Binding name value child
  -- if «Expression» then «Expression» else «Expression»
  "if"  -> do
           parseToken "if"
           cnd <- p'
           parseToken "then"
           thn <- p'
           parseToken "else"
           els <- p'
           return $ Branching cnd thn els
  --
  _ -> p'


parseLambda :: Parser Expression -> Parser Expression
parseLambda p' = do
  -- return $! unsafePerformIO (print "parseLambda")
  tok <- getNext
  getMaybeNextNext >>= \case
    -- «Expression» => «Expression»
    Just "=>" -> do parseToken tok ; parseToken "=>" ; Lambda tok <$> p'
    --
    _ -> p'


parseDisjunction :: Parser Expression -> Parser Expression
parseDisjunction = parseLeftAssociativeBinaryOperation [Or]


parseConjunction :: Parser Expression -> Parser Expression
parseConjunction = parseLeftAssociativeBinaryOperation [And]


parseComparison :: Parser Expression -> Parser Expression
parseComparison = parseLeftAssociativeBinaryOperation [Equals, LessThan]


parseAddition :: Parser Expression -> Parser Expression
parseAddition = parseLeftAssociativeBinaryOperation [Plus, Minus]


parseMultiplication :: Parser Expression -> Parser Expression
parseMultiplication = parseLeftAssociativeBinaryOperation [Times, Divide, Modulus]


parseNegation :: Parser Expression -> Parser Expression
parseNegation = parseUnaryOperation [Not]


parseExponential :: Parser Expression -> Parser Expression
parseExponential = parseRightAssociativeBinaryOperation [Exponential]


parseLiteral :: Parser Expression
parseLiteral = do
  tok <- getNext
  case tokenTypeOf tok of
    Normal -> Literal <$> parseNext
    _      -> errorUnexpected "«Literal»" tok


parseAssociated :: Parser Expression
parseAssociated = do
  getNext >>= \case
    "(" -> do parseNext ; e <- parseExpression ; parseToken ")" ; return $ Associated e
    tok -> errorUnexpected "( «Expression» )" tok


parseName :: Parser Name
parseName = parseNext


{-
  ## Parsing Helpers
-}


subParse :: Parser a -> Parser (Maybe (a, ParseState))
subParse p = do
  -- return $! unsafePerformIO (print "subParse")
  ps <- get
  case runStateT p ps of
    Ok (a, ps') -> return . Just $ (a, ps')
    Error _     -> return Nothing


tryParse :: Parser a -> Parser (Maybe a)
tryParse p = subParse p >>= \case
  Just (a, ps) -> do put ps ; return $ Just a
  Nothing -> return Nothing


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

getMaybeNextNext :: Parser (Maybe Token)
getMaybeNextNext = do
  i <- use tokenIndex
  l <- uses lexed (length . keys)
  if i < l - 1
    then Just . (!!(i+1)) <$> uses lexed elems
    else return Nothing


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
parseEndOfInput =
  atEndOfInput >>= \case
    True -> return ()
    False -> errorUnexpectedContinuedInput =<< getNext

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
errorUnexpected tokExp tokUnexp = errorHere $ "unexpected "++show tokUnexp++", expected "++show tokExp++"."

errorUnexpectedContinuedInput :: Token -> Parser a
errorUnexpectedContinuedInput tokUnexp = errorHere $ "unexpected "++show tokUnexp++", expected end of input."

errorOutOfBounds :: Parser a
errorOutOfBounds = errorHere $ "attempted to parse past end of input."

errorHere :: String -> Parser a
errorHere msg = do
  loc <- getLocation
  lift $ Error (loc, msg)
