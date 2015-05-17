import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
