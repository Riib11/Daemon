module Tipe where

{-
  # Tipe

  I spell it "Tipe" rather than "Type" because "Type" is a package in the core ghc library.
-}

data Tipe
  = Primitive PrimitiveTipe
  | Function Tipe Tipe
  deriving (Show, Eq, Ord)

data PrimitiveTipe
  = Int | Bool
  deriving (Show, Eq, Ord)
