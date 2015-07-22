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

data LispNum = Number Integer
               | Double Double
               | Single Float
               | Complex (Complex Double)
               | Rational (Ratio Integer)

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number LispNum
               | String String
               | Bool Bool
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

type Radix = Radix { convert :: ReadS Integer, parse :: Parser Integer }
readCons :: Integer -> [Char] -> Radix
readCons base xs = readInt base (`elem` xs) digitToInt
makeRadix base domain = Radix { convert = readCons base domain, parse = many1 domain }
readB = makeRadix 2 "01"
readH = makeRadix 16 (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
readO = makeRadix 8 ['0'..'7']
readD = makeRadix 10 ['0'..'9']

data Exactness = Exact | Inexact | Unknown deriving Show

data NumberPrefix = NumberPrefix { radix :: Radix, exactness :: Exactness }

instance Show NumberPrefix where
    show n = "radix fun " ++ show (exactness n)
                          
data RadixOrExactness = Radix Radix | Exactness Exactness

instance Show RadixOrExactness where
    show re = case re of Radix _ -> "radix fun"
                         Exactness e -> show e      

charToRadixOrExactness :: Char -> RadixOrExactness
charToRadixOrExactness c =
    let f 'e' = Exactness Exact
        f 'i' = Exactness Inexact
        f 'b' = Radix readB
        f 'o' = Radix readO
        f 'd' = Radix readD
        f 'x' = Radix readH
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

parseNumberPrefix :: Parser NumberPrefix
parseNumberPrefix = do
  rexs <- manyTake parseOnePrefix 2
  return $ foldl (\pref re -> case re of Exactness e -> NumberPrefix { radix = radix pref, exactness = e }
                                         Radix r -> NumberPrefix { radix = r, exactness = exactness pref })
                       NumberPrefix { radix = readD, exactness = Unknown } rexs

parseNumber :: NumberPrefix -> Parser LispVal
parseNumber pref =
    case e of Exact -> parseExact r
              Inexact -> parseInexact r
              _ -> parseExact r <|> (parseInexact r)
    where r = radix pref
          e = exactness pref

parseExact :: Radix -> Parser LispVal
parseExact r = do
  sign <- parseSign
  leadDigits <- parse r
  let inexactRemainder = liftM ((+1) . count) (many $ char '#')
  let inexactDecPoint = char '.'
  (do
    remainder <- inexactRemainder
    let exp = inexactDecPoint >> parse r
    return $ sign leadDigits * remainder + exp)
  <|> return $ Number $ sign leadDigits
  

parseExactImag :: Radix -> Parser LispVal
parseExactImag r = 
               
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
            
testParse :: String -> String -> String
testParse input raw = case parse parseNumberPrefix "lisp" input of
                        Left err -> "No match: " ++ show err
                        Right pref -> foldl ((. (show . fst)) . (++)) "" (radix pref raw)
