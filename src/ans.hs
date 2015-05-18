module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

data NumBase = Bin | Oct | Dec | Hex deriving (Show)

parseNumBase :: Parser NumBase
parseNumBase = do
  char '#'
  base <- oneOf "bodx"
  return $ case base of
    'b' -> Bin
    'o' -> Oct
    'd' -> Dec
    'x' -> Hex

type Digits = String

parseNumBin :: Parser Integer
parseNumBin = do
  d <- many1 $ oneOf "01"
  return $ fst $ (!! 0) $ readInt 2 isDigit ((\x -> x-48) . ord) d

--parseNumGen :: (Num a, Eq a) => (String -> [(a, String)]) -> [Char] -> Parser a
parseNumGen :: (String -> [(Integer, String)]) -> [Char] -> Parser Integer
parseNumGen rd xs = do
  d <- many1 $ oneOf xs
  return $ fst $ (!! 0) $ rd d

parseNumOct = parseNumGen readOct ['0'..'7']
parseNumDec = parseNumGen readDec ['0'..'9']
parseNumHex = parseNumGen readHex $ ['0'..'9'] ++ ['a'..'f']

parserFabric :: NumBase -> Parser Integer
parserFabric Bin = parseNumBin
parserFabric Oct = parseNumOct
parserFabric Dec = parseNumDec
parserFabric Hex = parseNumHex

parseExpr :: Parser Integer
parseExpr = do
  base <- parseNumBase
  val <- parserFabric base
  return val

readExpr :: String -> String
readExpr input = do
  case parse parseExpr "lisp" input of
   Right v -> show v
   Left err -> "err"

