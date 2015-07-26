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
import Control.Applicative hiding ((<|>), many)

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
               | ComplexI (Complex Double)
               | ComplexEI (Complex Integer)
               | ComplexER (Complex (Ratio Integer))
               | RationalE (Ratio Integer)
               deriving Show

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

data Exactness = Exact | Inexact | Unknown deriving Show
                          
data RadixOrExactness = R Integer | E Exactness deriving Show

charToRadixOrExactness :: Char -> RadixOrExactness
charToRadixOrExactness c =
    let f 'e' = E Exact
        f 'i' = E Inexact
        f 'b' = R 2
        f 'o' = R 8
        f 'd' = R 10
        f 'x' = R 16
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

parseNumberPrefix = do 
  rexs <- manyTake parseOnePrefix 2
  return $ foldl (\(ex, base) re -> case re of E Inexact -> (inexactTrail, base)
                                               E _ -> (exactTrail, base)
                                               R r -> (ex, r))
    (exactTrail, 10) rexs
  
parseNumber (e, r) = parseComplex e r 

exactTrail :: Parser [Char]
exactTrail = return []

inexactTrail :: Parser [Char]
inexactTrail = liftM (map (\_ -> '0')) $ many $ char '#'

digs :: [Char] -> Parser [Char]
digs = many1 . oneOf

parseReal :: Char -> Parser [Char] -> Integer -> Parser LispVal
parseReal sign exct base = decimalTrail sign exct "0" [] <|> (readReal' sign exct base)

readReal' :: Char -> Parser [Char] -> Integer -> Parser LispVal
readReal' sign exct base = do
  n1 <- digs domain
  inx1 <- exct
  let n1inx = n1 ++ inx1
  return n1inx >>= \n1raw ->
    (do
        char '/'
        n2raw <- digs domain
        inx2 <- exct
        let n1 = convert $ sign : n1raw
        let n2 = convert $ n2raw ++ inx2
        return $ if length (inx1 ++ inx2) == 0 then RationalE (n1 % n2)
                 else DoubleI (fromIntegral n1 / fromIntegral n2))
    <|>
    (decimalTrail sign exct n1 inx1)
    <|>
    (return $ NumberE $ convert $ sign : n1)
    where domain = case base of 2 -> "01"
                                8 -> ['0'..'7']
                                10 -> ['0'..'9']
                                16 -> ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
          convert = fst . head . readInt base (`elem` domain) digitToInt

decimalTrail :: Char -> Parser [Char] -> [Char] -> [Char] -> Parser LispVal
decimalTrail sign exct n1 inx1 = do
  char '.'
  exp <- if length inx1 > 0 then exct else (++) <$> many digit <*> exct
  suff <- liftM3 (\a b c -> a ++ b ++ c) (oneOf "eEsSfFdDlL" *> return "e") (pure <$> option '+' (oneOf "+-")) (many1 digit)
  let decRaw = n1 ++ suff
  return $ DoubleI $ fst . head . readFloat $ sign : decRaw

parseComplex exct base = do
  signR <- option '+' (oneOf "+-")
  parseReal signR exct base >>= \real ->
    (do
        signI <- option '+' (oneOf "+-")
        imag <- parseReal signI exct base
        char 'i'
        return $ case (real, imag) of (RationalE r, RationalE i) -> ComplexER $ r :+ i
                                      (NumberE r, RationalE i) -> ComplexER $ (r % 1) :+ i
                                      (RationalE r, NumberE i) -> ComplexER $ r :+ (i % 1)
                                      (NumberE r, NumberE i) -> ComplexEI $ r :+ i
                                      (RationalE r, DoubleI i) -> ComplexI $ fromIntegral (numerator r) / fromIntegral (denominator r) :+ i
                                      (DoubleI r, RationalE i) -> ComplexI $ r :+ fromIntegral (numerator i) / fromIntegral (denominator i)
                                      (DoubleI r, DoubleI i) -> ComplexI $ r :+ i
                                      (DoubleI r, NumberE i) -> ComplexI $ r :+ fromIntegral i
                                      (NumberE r, DoubleI i) -> ComplexI $ fromIntegral r :+ i)
    <|>
    (do
        char 'i'
        return $ case real of (RationalE i) -> ComplexER $ 0 :+ i
                              (NumberE i) -> ComplexEI $ 0 :+ i
                              (DoubleI i) -> ComplexI $ 0 :+ i)        
    <|> return real

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right pref -> "lol" --show . fst . (radix pref $ "1")
            
testParse :: String -> String
testParse input = case parse parseLispNum "lisp" input of
                    Left err -> "No match: " ++ show err
                    Right v -> show v
