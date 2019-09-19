import           Control.Monad.Trans.State

import           Control.Lens
import           Lexer
import           Parser
import           Prelude                   hiding (lex)

main :: IO ()
main = do
  putStrLn $ '\n' : replicate 40 '='

  -- print . lex $ "hello\nworld"
  -- print . lex $ "1 + 2 + 3"
  -- print . lex $ "(1 + 2) + (3 + 4 + (5 + 6))"

  let input = "true + true"
  putStrLn $ "input:  "++show input
  let lexed = lex input
  putStrLn $ "lexed:  "++show lexed
  let parsed = parse lexed
  putStrLn $ "parsed: "++show parsed

  putStrLn $ replicate 40 '='
