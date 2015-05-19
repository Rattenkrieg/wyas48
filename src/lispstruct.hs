import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))


data NumBase = 2 | 8 | 10 | 16

data Number = Num NumBase Prefix Complex

data Complex = Real NumBase Real
             | RealAt NumBase Real Real
             | RealPlus NumBase Real Imag
             | RealMinus NumBase Real Imag
             | ImagPos NumBase Imag
             | ImagNeg NumBase Imag

data Real = Real Sign NumBase Ureal

data Sign = Plus | Minus | NoSign

data Ureal = Uint NumBase Uinteger | UintDiv NumBase Uinteger Uinteger | Dec NumBase Decimal

data Uinteger = Uinteger NumBase Digits

data Digits = Digits NumBase DigitMany1

data DigitMany1 = Digit | Digit DigitMany1

data Digit = Digit2 | Digit8 | Digit10 | Digit16

data Digit2 = 0 | 1

data Decimal = Dec Uinteger Exponent
             | Dot Uinteger Suffix
             | Dec Float Suffix


data Imag = Imag | UrealThenImag Ureal

  

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number (Number Float)
               | Number (Number Integer)
               | String String
               | Bool Bool
               deriving Show


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
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom


data RadixPrefix = Bin | Oct | Dec | Hex deriving Show

parseRadix :: Parser RadixPrefix
parseRadix = do
  char '#'
  prefix <- oneOf "bodx"
  return $ case prefix of
            'b' -> Bin
            'o' -> Oct
            'd' -> Dec
            'x' -> Hex

parseExactness :: Parser Bool
parseExactness = do
  char '#'
  exactness <- oneOf "ei"
  return $ case exactness of
            'e' -> True
            'i' -> False

data NumberPrefix = NumberPrefix { radix :: RadixPrefix, exactness :: Bool}

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
parseNumberPrefix = parseRadixThenExactness <|> parseExactnessThenRadix
  

data ExponentMarker = Exponent | Short | Float | Double | Long deriving Show

data Sign = Plus | Minus deriving Show

data Suffix = Suffix { expMarker :: ExponentMarker, sign :: Sign, expVal :: Integer } deriving (Show)

parseSuffix :: Parser Suffix
parseSuffix = do
  expRaw <- oneOf "esfdlESFDL"
  signRaw <- option '+' oneOf "+-"
  return $ Suffix { expMarker = expM, sign = signM}
    where
      expM = case expRaw of
        'E' -> Exponent
        'e' -> Exponent
        'S' -> Short
        's' -> Short
        'F' -> Float
        'f' -> Float
        'D' -> Double
        'd' -> Double
        'L' -> Long
        'l' -> Long
      signM = case signRaw of
        '+' -> Plus
        '-' -> Minus

parseComplex a :: Parser (Number a)
parseComplex a = do
  real <- parseReal

parseReal a :: Parser (Num a)
parseReal = do
  signRaw <- option '+' oneOf "+-"
  uintDiv <- parseUinteger

parseUintegerDiv :: Parser 

parseUinteger :: Parser Char -> ([Char] -> Integer) -> Parser (Number In
parseUinteger digitsBase reader = do
  digits <- many1 digitsBase
  optSharp <- many '#'
  return $ reader digits

parseNumber :: Parser LispVal
parseNumber = do
  prefix <- parseNumberPrefix
  complex <- parseComplex
  

parseNumber''' :: Parser LispVal
parseNumber''' = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
  numStr <- many1 digit
  let num = read numStr
  return $ Number num

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber''


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right v -> "Found value" ++ show v
