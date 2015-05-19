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

parserFabric :: Radix -> Parser Integer
parserFabric Bin = parseNumBin
parserFabric Oct = parseNumOct
parserFabric Dec = parseNumDec
parserFabric Hex = parseNumHex

parseNumPyramid :: (Num a, Eq a) => Radix -> Parser a
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

parseRealF :: (Num a, Eq a) => Radix -> Parser a
parseRealF r = do
  sign <- parseSign
  num <- parseURealF r
  return $ case sign of
    '-' -> negate num
    _ -> num

parseUrealF :: (Num a, Eq a) => Radix -> Parser a
parseUrealF r =
  (parseUintegerF r) <|>
  (parseURationalF r) <|>
  (parseDecimalF r)

parseUintegerF :: (Num a, Eq a) => Radix -> Parser a
parseUintegerF r = do
  uint <- parserFabric r
  sharps <- many $ char '#'
  return uint

parseURationalF :: (Num a, Eq a) => Radix -> Parser a
parseURationalF r = do
  uint1 <- parseUintegerF r
  sp1 <- optSpaces
  div <- char '/'
  sp2 <- optSpaces
  uint2 <- parseUintegerF r

parseDecimalF :: (Num a, Eq a) => Radix -> Parser a
parseDecimalF Dec =
  uint <- ParseUintegerF Dec
parseDecimalF _ = return $ Expect "Dec radix"

parseUintegerExp :: (Num a, Eq a) => Parser a
parseUintegerExp =
  uint <- parseUintegerF Dec
  expSuffix <- parseSuffix


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

  

data ExponentMarker = Exponent | Short | Float | Double | Long deriving Show

data Sign = Plus | Minus deriving Show

data Suffix = Suffix { expMarker :: ExponentMarker, sign :: Sign, expVal :: Integer } deriving (Show)

parseExpr :: (Num a, Eq a) => Parser a
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

