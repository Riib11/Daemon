module Main where

import           Prelude   hiding (lex)
import           System.IO (hFlush, stdout)

import           Evaluator
import           Lexer
import           Parser

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
  let lexed = lex input
  let parsed = parse lexed
  putStrLn $ "lexed:     "++show lexed
  putStrLn $ "parsed:    "++show parsed
  case parsed of
    Parser.Ok prgm -> do
      let evaluated = evaluate prgm
      putStrLn $ "evaluated: "++show evaluated
    _ -> return ()
  loop

prompt :: IO ()
prompt = do
  putStr "> "
  hFlush stdout
