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
import           Data.Map                  as Map hiding (drop, foldl, foldr,
                                                   map)
import           System.IO.Unsafe

import           Lexer                     hiding (lexed)
import           Location
import           Pretty
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
  ## Pretty
-}

instance Pretty Program where
  pretty (Expression e) = pretty e

instance Pretty Expression where
  pretty (Binding n e e')          = pretty n++" := "++pretty e++" ; "++pretty e'
  pretty (Application e e')        = "("++pretty e++" "++pretty e'++")"
  pretty (Lambda n e)              = "("++pretty n++" => "++pretty e++")"
  pretty (Branching e e1 e2)       = "(if "++pretty e++" then ("++pretty e1++" else ("++pretty e2++"))"
  pretty (BinaryOperation o e1 e2) = "("++pretty e1++" "++pretty o++" "++pretty e2++")"
  pretty (UnaryOperation o e)      = "("++pretty o++" "++pretty e++")"
  pretty (Literal t)               = pretty t
  pretty (Associated e)            = "("++pretty e++")"

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

instance Pretty UnaryOperator where
  pretty = symbol

instance Pretty BinaryOperator where
  pretty = symbol

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

data ParseStatus a = Ok a | Error (String, String)

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
  show (Error (loc, msg)) = "[!] "++loc++" " ++ msg

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
  parseStructural
  $ parseLambda
  $ parseApplication
  $ parseDisjunction
  $ parseConjunction
  $ parseComparison
  $ parseAddition
  $ parseMultiplication
  $ parseNegation
  $ parseExponential
  $ parseAssociated
  $ parseLiteral


parseStructural :: Parser Expression -> Parser Expression
parseStructural p' =
  let
    -- let <Name> = <Expression> in <Expression> end
    parseBinding = do
      name <- parseName
      parseToken ":="
      value <- parseExpression
      parseToken ";"
      child <- parseExpression
      return $ Binding name value child

    -- if <Expression> then <Expression> else <Expression>
    parseBranching = do
      parseToken "if"
      cnd <- parseExpression
      parseToken "then"
      thn <- parseExpression
      parseToken "else"
      els <- parseExpression
      return $ Branching cnd thn els

  in getMaybeNextNext >>= \case
        Just ":="  -> parseBinding
        _          -> getNext >>= \case
                        "if" -> parseBranching
                        _ -> p'


parseLambda :: Parser Expression -> Parser Expression
parseLambda p' = do
  tok <- getNext
  getMaybeNextNext >>= \case
    -- <Expression> => <Expression>
    Just "=>" -> do parseToken tok ; parseToken "=>" ; Lambda tok <$> parseLambda p'
    --
    _ -> p'


parseApplication :: Parser Expression -> Parser Expression
parseApplication p' =
  let recurse :: Expression -> Parser Expression
      recurse e = (tryParse $ p') >>= \case
                      Just e' -> recurse $ Application e e'
                      Nothing -> return e
  in recurse =<< p'


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


parseAssociated :: Parser Expression -> Parser Expression
parseAssociated p' = do
  getNext >>= \case
    "(" -> do parseNext ; e <- parseExpression ; parseToken ")" ; return $ Associated e
    _ -> p'


parseLiteral :: Parser Expression
parseLiteral = do
  tok <- getNext
  case tokenTypeOf tok of
    Normal -> Literal <$> parseNext
    _      -> errorUnexpected "<Literal>" tok


parseName :: Parser Name
parseName = parseNext


{-
  ## Parsing Helpers
-}


subParse :: Parser a -> Parser (ParseStatus(a, ParseState))
subParse p = runStateT p <$> get


tryParse :: Parser a -> Parser (Maybe a)
tryParse p = subParse p >>= \case
  Ok (a, ps) -> do put ps ; return $ Just a
  _ -> return Nothing


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
getNext = atEndOfInput >>= \case
  False -> do i <- use tokenIndex ; ts <- uses lexed elems ; return $ ts!!i
  True  -> errorOutOfBounds


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
  getMaybeNext >>= \case
    Just tok' -> if tok == tok'
                   then parseNext
                   else errorUnexpected tok tok'
    Nothing -> errorUnexpected tok "<EOI>"


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
  ts <- uses lexed elems
  return $ i < length ts

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
  i <- use tokenIndex
  locs <- uses lexed keys
  if i < length locs
    then lift $ Error (show $ locs!!i, msg)
    else lift $ Error ("EOI", msg)


{-
  # Debug
-}

debugLexed :: Parser ()
debugLexed = do
  i <- use tokenIndex
  ts <- uses lexed elems
  return $! unsafePerformIO $! print (drop i ts)
