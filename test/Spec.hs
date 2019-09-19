import           Control.Monad.Trans.State

import           Control.Lens
import           Lexer
import           Parser
import           Prelude                   hiding (lex)

main :: IO ()
main = do
  putStrLn $ '\n' : replicate 40 '='
  print . view lexed . lex $ "hello world"
  print . view lexed . lex $ "1 + 2 + 3"
  print . view lexed . lex $ "(1 + 2) + (3 + 4 + (5 + 6))"
  -- let str = "1 ** 2"
  -- let lexed = lex str
  -- let parseStatus = runStateT parseExpn lexed
  -- putStrLn $ "lexed:    "++show lexed
  -- putStrLn $ "parsed:   "++show parseStatus
  -- case parseStatus of
  --   ParseOk (e, _) -> putStrLn $ showPretty e
  --   _              -> putStr ""
  putStrLn $ replicate 40 '='
