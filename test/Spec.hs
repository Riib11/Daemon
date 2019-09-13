import           Lexer
import           Prelude hiding (lex)

main :: IO ()
main = do
  putStrLn $ '\n' : replicate 40 '='
  print $ lex "hello world"
  print $ lex "1 + 2 + 3"
  print $ lex "(1 + 2) + (3 + 4 + (5 + 6))"
  putStrLn $ replicate 40 '='
