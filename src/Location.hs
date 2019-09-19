{-# LANGUAGE TemplateHaskell #-}

module Location where

import           Control.Lens

{-
  # Location

  Represents a token location in the input.
-}

data Location = Location
  { _line   :: Int
  , _column :: Int }
  deriving (Eq, Ord)

makeLenses ''Location

instance Show Location where
  show loc = "((line "++show (loc^.line)++", column "++show (loc^.column)++"))"
