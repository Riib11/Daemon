{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Checker where

import           Control.Arrow
import           Control.Lens              hiding (Lazy)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List                 as List
import           Data.Map                  as Map hiding (foldl, map)
import           System.IO.Unsafe

import qualified Lexer
import           Location
import qualified Parser
import           Tipe
import           Token

{-
  # CheckerState
-}

data CheckerState = CheckerState
  { _program :: Parser.Program
  , _context :: CheckerContext }
  deriving (Show)

type CheckerContext = Map Parser.Expression Tipe

makeLenses ''CheckerState

{-
  # CheckingStatus
-}

data CheckerStatus a = Ok a | Error (Parser.Expression, String)

instance Functor CheckerStatus where
  f `fmap` Ok a    = Ok (f a)
  f `fmap` Error e = Error e

instance Applicative CheckerStatus where
  pure = Ok
  Ok f    <*> Ok a    = Ok (f a)
  Ok _    <*> Error e = Error e
  Error e <*> _       = Error e

instance Monad CheckerStatus where
  Ok a    >>= f = f a
  Error e >>= _ = Error e

instance Show a => Show (CheckerStatus a) where
  show (Ok a)    = show a
  show (Error e) = show e
{-
  # Checker
-}

type Checker = StateT CheckerState CheckerStatus

{-
  # Checking
-}

check :: Parser.Program -> CheckerStatus ()
check prgm = return ()

-- check :: Parser.Program -> CheckerStatus ()
-- check prgm = evalStateT checkProgram initCheckerContext where
--   initCheckerContext = CheckerState prgm empty
--
--
-- checkProgram :: CheckerStatus ()
-- checkProgram = use program >>= \case
--   Parser.Expression e -> do checkExpression e ; return ()
--
-- checkExpression :: Parser.Expression -> CheckerStatus Tipe
-- checkExpression expr = context . at expr >>= \case
--   Just exprTipe -> return exprTipe
--   Nothing -> case expr of
--     Parser.Binding n e f         -> do
--                                     eT <- checkExpression e
--                                     context . at e .= Just eT
--                                     fT <- checkExpression f
--                                     context . at expr .= Just fT
--                                     return fT
--
--     Parser.Application e f       -> do
--                                     (eT, e'T) <- checkLambda e
--                                     fT <- checkExpression f
--                                     unify eT fT
--                                     return e'T
--
--     Parser.Lambda n e            -> do
--                                     nT <- checkExpression (Literal n)
--                                     eT <- checkExpression e
--                                     context . at expr .= Just $ Function nT eT
--                                     return eT
--
--
--     Parser.Branching c e f       -> fail "TODO"
--     Parser.BinaryOperation o e f -> fail "TODO"
--     Parser.UnaryOperation o e    -> fail "TODO"
--     Parser.Literal t             -> fail "TODO"
--     Parser.Associated e          -> fail "TODO"
--     expr -> fail $ "checkExpression not implemented for \""++show expr++"\"."
--
-- checkLambda :: Parser.Expression -> Checker (Tipe, Tipe)
-- checkLambda e = error "TODO"
--
-- checkUnification :: Parser.Expression -> Parser.Expression -> Checker ()
-- checkUnification e f = error "TODO"
