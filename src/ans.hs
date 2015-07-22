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

parseExactnessStrong :: Parser Bool
parseExactnessStrong = do
  char '#'
  exactness <- oneOf "ei"
  return $ case exactness of
            'e' -> True
            'i' -> False

parseExactness :: Parser Bool
parseExactness = option False parseExactnessStrong

data NumberPrefix = NumberPrefix { radix :: Radix, exactness :: Bool }

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
  parseUintegerExp <|>
  parseDotDecSuff <|>
  parseDecDotDecSuff <|>
  parseDecStuffDot
parseDecimalF _ = do
  string "Expecting Decimal"
  return 0

divByTons :: Double -> Double
divByTons i = i / (10 ** (fromInteger (1 + (ceiling (logBase 10 i)))))

parseDotDecSuff :: Parser Double
parseDotDecSuff = do
  char '.'
  mant <- parseUintegerF Dec
  many $ char '#'
  let v = fromInteger mant
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Just suff -> divByTons $ v * (evalExp suff)
    Nothing -> divByTons v

parseDecDotDecSuff :: Parser Double
parseDecDotDecSuff = do
  int1 <- parseNumDec
  char '.'
  int2 <- option 0 parseNumDec
  let v = fromInteger int2
  many $ char '#'
  expSuffix <- optionMaybe parseExponent
  let i1 = fromInteger int1
  let i2 = fromInteger int2
  return $
    let num = i1 + if int2 == 0 then i2 else divByTons i2 in
     case expSuffix of
      Just suff -> v where
        v = case sign suff of
          Plus -> num * (evalExp suff)
          Minus -> negate $ num * (evalExp suff)
      Nothing -> num

parseDecStuffDot :: Parser Double
parseDecStuffDot = do
  int1 <- parseNumDec
  many1 $ char '#'
  char '.'
  many $ char '#'
  expSuffix <- optionMaybe parseExponent
  return $ case expSuffix of
    Just suff -> (fromInteger int1) * (evalExp suff)
    Nothing -> fromInteger int1

parseUintegerExp :: Parser Double
parseUintegerExp = do
  mant <- parseUintegerF Dec
  expSuffix <- parseExponent
  return $ fromInteger mant * (evalExp expSuffix)

--evalExp :: Exponent -> Integer
evalExp e = 10 ** (fromInteger suff)
  where suff = case sign e of
                Plus -> expVal e
                Minus -> negate $ expVal e

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
   Left err -> show err

