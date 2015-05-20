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

optSpaces :: Parser ()
optSpaces = optional $ many space

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

parserIntFabric :: Radix -> Parser Integer
parserIntFabric Bin = parseNumBin
parserIntFabric Oct = parseNumOct
parserIntFabric Dec = parseNumDec
parserIntFabric Hex = parseNumHex

parseNumPyramid :: Radix -> Parser LispVal
parseNumPyramid r =
  (parseRealF r) <|>
  (parseRealAtRealF r) <|>
  (parseRealThenImagF r) <|>
  (parseSignedImagF r)

{--
parseNumPyramid radix =
  foldl (\parser parserfabric -> parser <|> (parserfabric radix))
  lookAhead anyToken
  [parseRealF, parseRealAtRealF, parseRealPlusImagF, parseRealMinusImagF, parsePlusImagF, parseMinusImagF]
--}

parseRealAtRealF :: Radix -> Parser LispVal
parseRealAtRealF r = do
  real1 <- parseRealF r
  optSpaces
  char '@'
  optSpaces
  real2 <- parseRealF r
  return $ Complex ((fromLispReal real1) :+ (fromLispReal real2))

--fromLispReal :: (Fractional a) => LispVal -> a
fromLispReal (Number n) = fromInteger n
fromLispReal (Float f) = f
fromLispReal (Rational r) = fromRational r
fromLispReal _ = 0 -- no way!

makeComplex :: LispVal -> LispVal -> LispVal
makeComplex r1 r2 = Complex ((fromLispReal r1) :+ (fromLispReal r2))

parseRealThenImagF :: Radix -> Parser LispVal
parseRealThenImagF r = do
  real <- parseRealF r
  imag <- parseSignedImagF r
  return $ makeComplex real imag

parseSignedImagF :: Radix -> Parser LispVal
parseSignedImagF r = do
  sign <- oneOf "+-"
  val <- option (Number 0) (parseUrealF r)
  char 'i'
  return $ case sign of
    '-' -> negateL val
    '+' -> val
    
parseRealF :: Radix -> Parser LispVal
parseRealF r = do
  sign <- option '+' (oneOf "+-")
  num <- parseUrealF r
  return $ case sign of
    '-' -> negateL num
    _ -> num

negateL :: LispVal -> LispVal
negateL (Number i) = Number $ negate i
negateL (Float d) = Float $ negate d
negateL (Complex c) = Complex $ negate c
negateL (Rational r) = Rational $ negate r
negateL other = other

parseUrealF :: Radix -> Parser LispVal
parseUrealF r =
  (>>=) (parseUintegerF r) (return . Number) <|>
  (>>=) (parseURationalF r) (return . Rational) <|>
  (>>=) (parseDecimalF r) (return . Float)

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

parseDecimalF :: Radix -> Parser Double
parseDecimalF Dec = 
  (>>=) parseUintegerExp (return . fromInteger) <|>
  parseDotDecSuff <|>
  parseDecDotDecSuff <|>
  parseDecStuffDot
parseDecimalF _ = do
  string "Expecting Decimal"
  return 0

--divByTons :: Integer -> Double
divByTons i =
  d / (fromInteger (10 ^ (ceiling (logBase 10 d))))
  where d = fromInteger i

parseDotDecSuff :: Parser Double
parseDotDecSuff = do
  char '.'
  mant <- parseUintegerF Dec
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Just suff -> v where
      v = case sign suff of
        Plus -> divByTons $ mant * (10 ^ (expVal suff))
        Minus -> negate $ divByTons $ mant * (10 ^ (expVal suff))
    Nothing -> divByTons mant

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
      Just suff -> v where
        v = case sign suff of
          Plus -> num * (10 ^ (expVal suff))
          Minus -> negate $ num * (10 ^ (expVal suff))
      Nothing -> num

parseDecStuffDot :: Parser Double
parseDecStuffDot = do
  int1 <- parseNumDec
  many1 $ char '#'
  char '.'
  many $ char '#'
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Just suff -> (fromInteger int1) * (10 ^ (expVal suff))
    Nothing -> fromInteger int1

parseUintegerExp :: Parser Integer
parseUintegerExp = do
  mant <- parseUintegerF Dec
  expSuffix <- parseExponent
  let int = mant * (10 ^ (expVal expSuffix))
  return $ case sign expSuffix of
    Plus -> int
    Minus -> negate int

parseExponent :: Parser Exponent
parseExponent = do
  expMarkRaw <- oneOf "esfdlESFDL"
  let expM = case expMarkRaw of
        'E' -> EM
        'e' -> EM
        'S' -> SM
        's' -> SM
        'F' -> FM
        'f' -> FM
        'D' -> DM
        'd' -> DM
        'L' -> LM
        'l' -> LM
  signRaw <- option '+' (oneOf "+-")
  let signM = case signRaw of
        '+' -> Plus
        '-' -> Minus
  expValRaw <- parseNumDec
  return $ Exponent { expMarker = expM, sign = signM, expVal = expValRaw }      
      

data ExponentMarker = EM | SM | FM | DM | LM deriving Show

data Sign = Plus | Minus deriving Show

data Exponent = Exponent { expMarker :: ExponentMarker, sign :: Sign, expVal :: Integer } deriving (Show)

parseExpr :: Parser LispVal
parseExpr = do
  prefix <- parseNumberPrefix
  let rad = radix prefix
  let exact = exactness prefix
  val <- parseNumPyramid rad
  return val

readExpr :: String -> String
readExpr input = do
  case parse parseExpr "lisp" input of
   Right v -> show v
   Left err -> "err"

