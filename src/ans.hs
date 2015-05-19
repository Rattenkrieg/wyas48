module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
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

optSpaces :: Parser ()
optSpaces = option " " spaces

data Radix = Bin | Oct | Dec | Hex deriving (Show)

parseRadix :: Parser Radix
parseRadix = do
  char '#'
  base <- oneOf "bodx"
  return $ case base of
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

data NumberPrefix = NumberPrefix { radix :: Radix, exactness :: Bool}

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

parseNumBin :: Parser LispVal
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

parserIntFabric :: Radix -> Parser Integer
parserIntFabric Bin = parseNumBin
parserIntFabric Oct = parseNumOct
parserIntFabric Dec = parseNumDec
parserIntFabric Hex = parseNumHex

parseNumPyramid :: Radix -> Parser LispVal
parseNumPyramid r =
  parseReal r <|>
  parseRealAtReal r <|>
  parseRealPlusImag r <|>
  parseRealMinusImag r <|>
  parsePlusImag r <|>
  parseMinusImag r

parseNumPyramid radix =
  foldl (\parser parserfabric -> parser <|> (parserfabric radix))
  lookAhead anyToken
  [parseRealF, parseRealAtRealmF, parseRealPlusImagF, parseRealMinusImagF, parsePlusImagF, parseMinusImagF]

parseRealF :: Radix -> Parser LispVal
parseRealF r = do
  sign <- parseSign
  num <- parseURealF r
  return $ case sign of
    '-' -> negate num
    _ -> num

parseUrealF :: Radix -> Parser LispVal
parseUrealF r =
  (parseUintegerF r) <|>
  (parseURationalF r) <|>
  (parseDecimalF r)

parseUintegerF :: Radix -> Parser Integer
parseUintegerF r = do
  uint <- parserIntFabric r
  many $ char '#'
  return uint

parseURationalF :: Radix -> Parser (Ratio Integer)
parseURationalF r = do
  uint1 <- parseUintegerF r
  optSpaces
  char '/'
  optSpaces
  uint2 <- parseUintegerF r
  return $ uint1 % uint2

parseDecimalF :: Radix -> Parser Integer
parseDecimalF Dec = 
  parseUintegerExp <|>
  parseDotDecSuff <|>
  parseDecDotDecSuff <|>
  parseDecStuffDot
parseDecimalF _ = return $ Expect "Dec radix"

divByTons :: Integer -> Double
divByTons i = fromInteger i / (10 ^ (ceiling $ logBase 10 i))

parseDotDecSuff :: Parser Double
parseDotDecSuff = do
  char '.'
  mant <- parseUintegerF Dec
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Some suff -> v where
      v = case sign expSuffix of
        Plus -> i
        Minus -> negate i
          where i = divByTons mant * (10 ^ (expVal suff))
    None -> divByTons mant

parseDecDotDecSuff :: Parser Double
parseDecDotDecSuff = do
  int1 <- parseNumDec
  char '.'
  int2 <- option 0 parseNumDec
  many $ char '#'
  expSuffix <- optionMaybe parseExponent
  return $
    let num = (fromInteger int1) + if int2 == 0 then 0 else divByTons int2 in
     case expSuffix of
      Some suff -> v where
        v = case sign expSuffix of
          Plus -> i
          Minus -> negate i
            where i = num * (10 ^ (expVal stuff))
      None -> num

parseDecStuffDot :: Parser Double
parseDecStuffDot = do
  int1 <- parseNumDec
  many1 & char '#'
  char '.'
  many $ char '#'
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Some suff -> (fromInteger int1) * (10 ^ (expVal suff))
    None -> fromInteger int1

parseUintegerExp :: Parser Integer
parseUintegerExp = do
  mant <- parseUintegerF Dec
  expSuffix <- parseExponent
  let int = mant * (10 ^ (expVal expSuffix))
  return $ case sign expSuffix of
    Plus -> int
    Minus -> negate int

parseExpSuffix :: 

parseExponent :: Parser Exponent
parseExponent = do
  expMarkRaw <- oneOf "esfdlESFDL"
  signRaw <- option '+' oneOf "+-"
  expValRaw <- parseNumDec
  return $ Exponent { expMarker = expM, sign = signM, expVal = expValRaw }
    where
      expM = case expMarkRaw of
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

data ExponentMarker = Exponent | Short | Float | Double | Long deriving Show

data Sign = Plus | Minus deriving Show

data Exponent = Exponent { expMarker :: ExponentMarker, sign :: Sign, expVal :: Integer } deriving (Show)

parseExpr :: Parser LispVal
parseExpr = do
  prefix <- parseNumPrefix
  let rad = radix prefix
  let exact = exactness prefix
  val <- parseNumPyramid rad
  return val

readExpr :: String -> String
readExpr input = do
  case parse parseExpr "lisp" input of
   Right v -> show v
   Left err -> "err"

