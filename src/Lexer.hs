{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import           Control.Lens
import           Control.Monad.State
import           Data.Map            hiding (filter, foldl, map)
import           Data.Maybe
import           System.IO.Unsafe

import           Location
import           Token

{-
  # LexState

  Keeps track of lexing progress, and attackes
-}

data LexState = LexState
  { _input    :: String
  , _location :: Location
  , _working  :: Maybe Token
  , _lexed    :: Lexed }
  deriving (Show)

type Lexer a = State LexState a

type Lexed = Map Location Token

makeLenses ''LexState

lex :: String -> Lexed
lex str = execState lexMain initLexState ^. lexed where
  initLexState = LexState str (Location 0 0) Nothing empty

{-
  # Lexing
-}

lexMain :: Lexer ()
lexMain = use input >>= \case
  "" -> do appendWorking ; return () -- done
  _  -> do lexNext ; lexMain         -- recurse

lexNext :: Lexer ()
lexNext = do
  inpt <- use input
  case extractTypedToken inpt of
    (Normal, tok, input')             -> do
                                         mergeToken tok         -- merge token
                                         input .= input'        -- update input

    (Delimeter Reserved, tok, input') -> do
                                         appendWorking          -- append working
                                         mergeToken tok         -- merge reserved token
                                         input .= input'        -- update input

    (Delimeter Space, tok, input')    -> do
                                         appendWorking          -- append working
                                         -- appendToken tok        -- append space token
                                         input .= input'        -- update input

    (Delimeter Newline, tok, input')  -> do
                                         appendWorking          -- append working
                                         -- appendToken tok        -- append newline token
                                         location . line += 1   -- increment line
                                         location . column .= 0 -- reset column
                                         input .= input'        -- update input


-- if the given string begins with a special token (e.g. delimeter),
--     then return the token type, the token, and the string afterwards
--     else return the Normal token type, the first character, and the string afterwards
extractTypedToken :: String -> (TokenType, Token, String)
extractTypedToken str@(c:str') =
  case filter (isJust . snd) . map (_2 %~ extractToken' str) $ delimeters of
    []                           -> (Normal, [c], str')
    (delTyp, Just (tok, str')):_ -> (Delimeter delTyp, tok, str')

-- if the given token begings the given string,
--   then return Just the token and the rest of the string afterwards
--   else return Nothing
extractToken :: Token -> String -> Maybe (Token, String)
extractToken tok str = recurse tok str where
  recurse [] ys         = Just (tok, ys)
  recurse xs []         = Nothing
  recurse (x:xs) (y:ys) = if x == y then recurse xs ys else Nothing

extractToken' = flip extractToken -- reversed

-- append the working lextoken to the current line
appendWorking :: Lexer ()
appendWorking = use working >>= \case
  Nothing -> return ()
  Just wrkTok -> do appendToken wrkTok ; working .= Nothing

-- merge given token to the working lextoken
mergeToken :: Token -> Lexer ()
mergeToken tok =
  working %= \case
    Nothing -> Just tok
    Just wrkTok -> Just $ wrkTok++tok

-- append the given lextoken to the current line
appendToken :: Token -> Lexer ()
appendToken tok = do
  loc <- use location
  lexed . at loc ?= tok
  location . column += length tok

debugLexState :: Lexer ()
debugLexState = do
  ls <- get
  return $! unsafePerformIO $ print ls

{-
  # Display
-}

displayLexed :: Lexed -> String
displayLexed = show . elems
