module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Error
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))


data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | Float Double
               | Complex (Complex Double)
               | Rational (Ratio Integer)
               | String String
               | Bool Bool
               deriving Show
           

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseEscaped :: Parser Char
parseEscaped = do
  char '\\'
  symbol

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\"" <|> parseEscaped
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom

data Radix = Bin | Oct | Dec | Hex deriving (Show)

parseRadixStrong :: Parser Radix
parseRadixStrong = do
  char '#'
  base <- oneOf "bodx"
  return $ case base of
    'b' -> Bin
    'o' -> Oct
    'd' -> Dec
    'x' -> Hex

parseRadix :: Parser Radix
parseRadix = option Dec parseRadixStrong                               

data Exactness = Exact | Inexact | Unknown deriving Show

parseExactnessStrong :: Parser Exactness
parseExactnessStrong = do
  char '#'
  exactness <- oneOf "ei"
  return $ case exactness of
            'e' -> Exact
            'i' -> Inexact

parseExactness :: Parser Exactness
parseExactness = option Unknown parseExactnessStrong

data NumberPrefix = NumberPrefix { radix :: Radix, exactness :: Exactness }

parseRadixThenExactness :: Parser NumberPrefix
parseRadixThenExactness = do
  radix <- parseRadix
  exactness <- parseExactness
  return $ NumberPrefix { radix = radix, exactness = exactness }

parseExactnessThenRadix :: Parser NumberPrefix
parseExactnessThenRadix = do
  exactness <- parseExactness
  radix <- parseRadix
  return $ NumberPrefix { radix = radix, exactness = exactness }

parseNumberPrefix :: Parser NumberPrefix
parseNumberPrefix = try parseRadixThenExactness <|> parseExactnessThenRadix

literalsToPrefix :: Maybe Char -> Maybe Char -> NumberPrefix
literalsToPrefix Nothing Nothing = NumberPrefix { radix = Dec, exactness = Unknown }
literalsToPrefix (Just a) Nothing | a `elem` ['e', 'i'] = 

data RadixOrExactness = Radix Radix | Exactness Exactness

charToRadixOrExactness :: Char -> RadixOrExactness
charToRadixOrExactness c =
    let f 'e' = Exactness Exact
        f 'i' = Exactness Inexact
        f 'b' = Radix Bin
        f 'o' = Radix Oct
        f 'd' = Radix Dec
        f 'x' = Radix Hex
    in f c
                                      
parsePrefixLiteral :: Parser Prefix
parsePrefixLiteral = oneOf "eibodx"

parseOnePrefix :: Parser RadixOrExactness
parseOnePrefix = do
  char '#'
  c <- parsePrefixLiteral
  return $ charToRadixOrExactness c

parseNumberPrefixOpt :: Parser (Maybe NumberPrefix)
parseNumberPrefixOpt = do
  fst <- optionMaybe parseOnePrefix
  fst >>= \fst -> let snd = optionMaybe parseOnePrefix in
                  case (fst, snd) of (Exactness e, r) -> NumberPrefix { radix = fromMaybe Dec r, exactness = e }
                                     (Radix r, e) -> NumberPrefix { radix = r, exactness = fromMaybe Unknown e }

parseNumberPrefix :: Parser NumberPrefix
parseNumberPrefix = do
  optPrefix <- parseNumberPrefixOpt
  return $ fromMaybe NumberPrefix { radix = Dec, exactness = Unknown } optPrefix
                 
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _ -> "Found value"
