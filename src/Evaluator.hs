{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluator where

import           Control.Arrow
import           Control.Lens              hiding (Lazy)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List                 as List hiding (union)
import           Data.Map                  as Map hiding (foldl, map)
import           System.IO.Unsafe

import           Data.Char
import qualified Lexer
import           Location
import qualified Parser                    as P
import           Token

{-
  Evaluation Types
-}

data Program
  = Value Value
  deriving (Show)

data Value
  = Integer Int
  | Boolean Bool
  | Closure Name P.Expression EvaluationContext
  | Lazy P.Expression EvaluationContext
  deriving (Show)

type Name = String

{-
  # EvaluatorState
-}

data EvaluatorState = EvaluatorState
  { _program :: P.Program
  , _context :: EvaluationContext }
  deriving (Show)

type EvaluationContext = Map Name Value

makeLenses ''EvaluatorState

{-
  # EvaluationStatus
-}


data EvaluatorStatus a = Ok a | Error (P.Expression, String)

instance Functor EvaluatorStatus where
  f `fmap` Ok a    = Ok (f a)
  f `fmap` Error e = Error e

instance Applicative EvaluatorStatus where
  pure = Ok
  Ok f    <*> Ok a    = Ok (f a)
  Ok _    <*> Error e = Error e
  Error e <*> _       = Error e

instance Monad EvaluatorStatus where
  Ok a    >>= f = f a
  Error e >>= _ = Error e

instance Show a => Show (EvaluatorStatus a) where
  show (Ok a)           = show a
  show (Error (e, msg)) = "[!] Error in "++show e++", "++msg

{-
  # Evaluator
-}

type Evaluator = StateT EvaluatorState EvaluatorStatus

{-
  # Evaluation
-}

evaluate :: P.Program -> EvaluatorStatus Program
evaluate prgm = evalStateT evaluateProgram initEvaluatorState where
  initEvaluatorState = EvaluatorState prgm empty

{-
  ## Evaluation Program
-}

evaluateProgram :: Evaluator Program
evaluateProgram = use program >>= \case
  P.Expression expr -> Value <$> evaluateExpression expr


subEvaluate :: Evaluator a -> Evaluator a
subEvaluate evtr = do
  st <- get
  lift $ evalStateT evtr st


evaluateExpression :: P.Expression -> Evaluator Value
evaluateExpression expr = case expr of
  P.Binding n e f          -> do
                              eV <- evaluateExpression e
                              context . at n .= Just eV
                              evaluateExpression f

  P.Application e f        -> do
                              fV <- evaluateExpression f
                              (n, e, ctx) <- evaluateClosure e
                              evaluateApplication n fV e ctx

  P.Lambda n e              -> Closure n e <$> use context

  P.Branching c e f         -> evaluateBoolean e >>= \case
                                 True  -> evaluateExpression e
                                 False -> evaluateExpression f

  P.BinaryOperation o e f   -> evaluateBinaryOperation o e f

  P.UnaryOperation o e      -> evaluateUnaryOperation o e

  P.Literal t               -> evaluateLiteral t

  P.Associated e            -> evaluateExpression e


evaluateClosure :: P.Expression -> Evaluator (P.Name, P.Expression, EvaluationContext)
evaluateClosure e = evaluateExpression e >>= \case
  Closure n e ctx -> return (n, e, ctx)
  eV -> errorUnexpected e "«Closure»" eV


--  [n => v](e with ctx)
evaluateApplication :: P.Name -> Value -> P.Expression -> EvaluationContext -> Evaluator Value
evaluateApplication n v e ctx = subEvaluate $ do
  context %= union ctx
  context . at n .= Just v
  evaluateExpression e


integerBinaryOperators :: Map P.BinaryOperator (Int -> Int -> Int)
integerBinaryOperators = fromList
  [ (P.Plus, (+))
  , (P.Minus, (-))
  , (P.Times, (*))
  , (P.Divide, (div))
  , (P.Modulus, (mod)) ]
  -- , (P.Exponential, (**)) ]

booleanBinaryOperators :: Map P.BinaryOperator (Bool -> Bool -> Bool)
booleanBinaryOperators = fromList
  [ (P.And, (&&))
  , (P.Or, (||)) ]

evaluateBinaryOperation :: P.BinaryOperator -> P.Expression -> P.Expression -> Evaluator Value
evaluateBinaryOperation o e f =
  if o `elem` keys integerBinaryOperators then do
    x <- evaluateInteger e
    y <- evaluateInteger f
    return . Integer . uncurry (integerBinaryOperators!o) $ (x, y)
  else if o `elem` keys booleanBinaryOperators then do
    a <- evaluateBoolean e
    b <- evaluateBoolean f
    return . Boolean . uncurry (booleanBinaryOperators!o) $ (a, b)
  else
    fail "TODO"

evaluateUnaryOperation :: P.UnaryOperator -> P.Expression -> Evaluator Value
evaluateUnaryOperation o e = fail "TODO"

evaluateInteger :: P.Expression -> Evaluator Int
evaluateInteger e = evaluateExpression e >>= \case
  Integer i -> return i
  v -> errorUnexpected e "«Integer»" v

evaluateBoolean :: P.Expression -> Evaluator Bool
evaluateBoolean e = evaluateExpression e >>= \case
  Boolean b -> return b
  v -> errorUnexpected e "«Boolean»" v

evaluateLiteral :: Token -> Evaluator Value
evaluateLiteral t = do
  mbV <- use $ context . at t
  case mbV of
    Just v -> return v
    Nothing -> case t of
      "true" -> return $ Boolean True
      "false" -> return $ Boolean False
      _ -> if all isDigit t
        then return $ Integer (read t :: Int)
        else errorUndefined t

{-
  ## Evaluation Utilities
-}

coerceValue :: Value -> Evaluator Value
coerceValue = \case
  Lazy e ctx -> coerceValue =<< evaluateLazy e ctx
  v -> return v

evaluateLazy :: P.Expression -> EvaluationContext -> Evaluator Value
evaluateLazy e ctx = subEvaluate $ do
  context %= union ctx
  evaluateExpression e

{-
  # Errors
-}

errorUnexpected :: (Show b, Show a) => P.Expression -> a -> b -> Evaluator c
errorUnexpected expr exp unexp = lift $ Error (expr, "Expected "++show exp++", but found "++show unexp++".")

errorUndefined :: Token -> Evaluator a
errorUndefined t = lift $ Error (P.Literal t, "Undefined "++show t++".")
