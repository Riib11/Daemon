import           Control.Monad.Trans.State

import           Control.Lens
import           Lexer
import           Parser
import           Prelude                   hiding (lex)

main :: IO ()
main = do
  putStrLn $ '\n' : replicate 40 '='

  testInput "x"
  testInput "x := 1; y := 2; 3"
  testInput "x y"

  putStrLn $ replicate 40 '='


testInput :: String -> IO ()
testInput input = do
  putStrLn $ "input:  "++show input
  let lexed = lex input
  putStrLn $ "lexed:  "++displayLexed lexed
  let parsed = parse lexed
  putStrLn $ "parsed: "++show parsed
