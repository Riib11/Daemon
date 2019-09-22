module Main where

import           Prelude   hiding (lex)
import           System.IO (hFlush, stdout)

import           Evaluator
import           Lexer
import           Parser
import           Pretty

title = "|| Daemon --- interpreter"
titleBorder = replicate (length title + 5) '='

main :: IO ()
main = do
  putStrLn $ titleBorder
  putStrLn $ "|| Daemon --- interpreter"
  putStrLn $ titleBorder
  loop

loop :: IO ()
loop = do
  prompt
  input <- getLine
  -- lex
  let lexed = lex input
  putStrLn $ "lexed:     "++displayLexed lexed
  -- parse
  case parse lexed of
    Parser.Ok parsPrgm -> do
      putStrLn $ "parsed:    "++pretty parsPrgm
      -- evaluate
      case evaluate parsPrgm of
        Evaluator.Ok evalPrgm ->
          putStrLn $ "evaluated: "++pretty evalPrgm
        eErr -> putStrLn $ "evaluated: "++show eErr
    pErr -> putStrLn $ "parsed:    "++show pErr
  --
  loop

prompt :: IO ()
prompt = do
  putStr "> "
  hFlush stdout
