module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Error
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex
import Data.Maybe

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | String String
               | Bool Bool
               | NumberE Integer
               | DoubleI Double
               | SingleI Float
               | ComplexI (Complex Double)
               | ComplexEI (Complex Integer)
               | ComplexER (Complex (Ratio Integer))
               | RationalE (Ratio Integer)
               deriving Show
{-
lispNumNegate :: LispVal -> LispVal
lispNumNegate = neg
    where neg (NumberE i) = NumberE $ negate i
          neg (DoubleI d) = DoubleI $ negate d
          neg (SingleI f) = SingleI $ negate f
          neg (ComplexI c) = ComplexI $ negate c
          neg (ComplexEI c) = ComplexEI $ negate c
          neg (ComplexER c) = ComplexER $ negate c
          neg (RationalE r) = RationalE $ negate r
          neg v = v
-}

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseEscaped :: Parser Char
parseEscaped = char '\\' >> symbol

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

data Radix = Radix { converter :: ReadS Integer, parser :: Parser [Char] }
readCons base xs = readInt base (`elem` xs) digitToInt
makeRadix :: Integer -> [Char] -> Radix                   
makeRadix base domain = Radix { converter = readCons base domain, parser = many1 $ oneOf domain }
readB = makeRadix 2 "01"
readH = makeRadix 16 (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
readO = makeRadix 8 ['0'..'7']
readD = makeRadix 10 ['0'..'9']

data Exactness = Exact | Inexact | Unknown deriving Show

data NumberPrefix = NumberPrefix { radix :: Radix, exactness :: Exactness }

instance Show NumberPrefix where
    show n = "radix fun " ++ show (exactness n)
                          
data RadixOrExactness = R Radix | E Exactness

instance Show RadixOrExactness where
    show re = case re of R _ -> "radix fun"
                         E e -> show e      

charToRadixOrExactness :: Char -> RadixOrExactness
charToRadixOrExactness c =
    let f 'e' = E Exact
        f 'i' = E Inexact
        f 'b' = R readB
        f 'o' = R readO
        f 'd' = R readD
        f 'x' = R readH
    in f c
                                      
parsePrefixLiteral :: Parser Char
parsePrefixLiteral = oneOf "eibodx"

parseOnePrefix :: Parser RadixOrExactness
parseOnePrefix = do
  char '#'
  c <- parsePrefixLiteral
  return $ charToRadixOrExactness c

manyTake p n = scan n
    where
      scan n | n <= 0 = return []
      scan n = do
        x <- optionMaybe p
        case x of Just v -> scan (n - 1) >>= (\vs -> return (v:vs))
                  Nothing -> return []

parseLispNum :: Parser LispVal
parseLispNum = do
  pref <- parseNumberPrefix
  num <- parseNumber pref
  return num
                             
parseNumberPrefix :: Parser NumberPrefix
parseNumberPrefix = do
  rexs <- manyTake parseOnePrefix 2
  return $ foldl (\pref re -> case re of E e -> NumberPrefix { radix = radix pref, exactness = e }
                                         R r -> NumberPrefix { radix = r, exactness = exactness pref })
                       NumberPrefix { radix = readD, exactness = Unknown } rexs

parseNumber :: NumberPrefix -> Parser LispVal
parseNumber pref =
    case e of Exact -> parseExact r
              Inexact -> parseInexact r
              _ -> try (parseExact r) <|> (parseInexact r)
    where r = radix pref
          e = exactness pref

parseExact :: Radix -> Parser LispVal
parseExact r = try (parseComplexExact r) <|> (parseRationalExact r)

parseComplexExact :: Radix -> Parser LispVal
parseComplexExact r = do
  real <- parseRationalExact r
  sign <- (char '-' >> (return negate)) <|> (char '+' >> (return id))
  imag <- parseUnsignedRational sign r
  char 'i'
  return $ case (real, imag) of (RationalE r, RationalE i) -> ComplexER $ r :+ i
                                (NumberE r, RationalE i) -> ComplexER $ (r % 1) :+ i
                                (RationalE r, NumberE i) -> ComplexER $ r :+ (i % 1)
                                (NumberE r, NumberE i) -> ComplexEI $ r :+ i

parseRationalExact :: Radix -> Parser LispVal
parseRationalExact r = do
  sign <- parseSign
  rat <- parseUnsignedRational sign r
  return rat

--parseUnsignedRational :: (Num a) => (a -> a) -> Radix -> Parser LispVal
parseUnsignedRational s r = 
  parser r >>= \num ->
  (do
    char '/'
    den <- parser r
    return $ RationalE $ (s . fst . head $ converter r num) % (fst . head $ converter r den))
  <|> (return $ NumberE (s . fst . head $ converter r num))
               
parseInexact :: Radix -> Parser LispVal
parseInexact = parseExact
              
parseSign :: Num a => Parser (a -> a)
parseSign = liftM signToFn (option '+' (oneOf "+-"))
            where signToFn '+' = id
                  signToFn _ = negate

                              
{-                               
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> (parseNumber NumberPrefix { radix = readD, exactness = Unknown })
            
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right pref -> "lol" --show . fst . (radix pref $ "1")
            
testParse :: String -> String
testParse input = case parse parseLispNum "lisp" input of
                    Left err -> "No match: " ++ show err
                    Right v -> show v
