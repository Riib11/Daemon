{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import           Control.Lens
import           Control.Monad.State
import           Data.Map            hiding (filter, map)
import           Data.Maybe

import           Token

{-
  # LexStatus

  Keeps track of lexing progress, and attackes
-}

data LexLine = LexLine Int [LexToken] deriving (Show) -- numbered lines
data LexToken = LexToken Int Token deriving (Show) -- numbered columns

tokenOf :: LexToken -> Token
tokenOf (LexToken _ tok) = tok

data LexStatus = LexStatus
  { _input         :: String
  , _currentLine   :: Int
  , _currentColumn :: Int
  , _working       :: Maybe LexToken
  , _lexed         :: Map Int LexLine }
  deriving (Show)

type Lexer a = State LexStatus a

makeLenses ''LexStatus

lex :: String -> LexStatus
lex str = execState lex' initLexStatus where
  initLexStatus = LexStatus
    { _input         = str
    , _currentLine   = 0
    , _currentColumn = 0
    , _working       = Nothing
    , _lexed         = empty }

{-
  # Lexing
-}

lex' :: Lexer ()
lex' = uses input >>= \case
  "" -> do appendWorking ; return () -- done
  _  -> do lexNext ; lex'        -- recurse

lexNext :: Lexer ()
lexNext = do
  iCol <- uses currentColumn
  (extractTypedToken <$> uses input) >>= \case
    (Normal, tok) -> do
      mergeLexToken $ LexToken iCol tok -- merge token
      currentColumn += length tok       -- increment column
    (Delimeter Reserved, tok) -> do
      appendWorking                     -- append working
      mergeLexToken $ LexToken iCol tok -- merge reserved token
      currentColumn += length tok       -- increment column
    (Delimeter Space, tok) -> do
      appendWorking                     -- append working
      currentColumn += length tok       -- increment column
    (Delimeter Newline, tok) -> do
      appendWorking                     -- append working
      currentLine += 1                  -- increment line
      currentColumn .= 0                -- reset column

-- if the given string begins with a special token (e.g. delimeter),
--     then return the token type, the token, and the string afterwards
--     else return the Normal token type, the first character, and the string afterwards
extractTypedToken :: String -> (TokenType, Token, String)
extractTypedToken str@(c:str') =
  case filter (isJust . snd) . map (_2 %~ extractToken' str) $ delimeters of
    []                           -> (Normal, [c], str')
    (tokTyp, Just (tok, str')):_ -> (tokTyp, tok, str')

-- if the given token begings the given string,
--   then return Just the token and the rest of the string afterwards
--   else return Nothing
extractToken :: Token -> String -> Maybe (Token, String)
extractToken tok str = recurse tok str where
  recurse [] ys         = Just (tok, ys)
  recurse xs []         = Nothing
  recurse (x:xs) (y:ys) = if x == y then recurse xs ys else Nothing

extractToken' str tok = extractToken tok str -- reversed

-- append the working lextoken to the current line
appendWorking :: Lexer ()
appendWorking = uses working >>= \case
  Nothing -> return ()
  Just lexTok -> appendLexToken lexTok

-- merge given token to the working lextoken
mergeLexToken :: LexToken -> Lexer ()
mergeLexToken lexTok =
  working %= \case
    Nothing -> Just lexTok
    Just (LexToken iCol tok) -> Just (LexToken iCol $ tok ++ tokenOf lexTok)

-- append the given lextoken to the current line
appendLexToken :: LexToken -> Lexer ()
appendLexToken lexTok = do
  iLine <- uses currentLine
  lexed . at iLine %= \case
    Nothing -> [lexTok]
    Just lts -> lexTok:lts
