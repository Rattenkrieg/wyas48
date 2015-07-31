module Parser where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Error
import Numeric
import Data.Char
import Data.Maybe
import Control.Applicative hiding ((<|>), many)
import Ast

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

{-         
parseAtom :: Parser LispVal
parseAtom = do first <- pure <$> letter <|> ((:) <$> char '#' <*> (pure <$> noneOf "iedhob"))
               rest <- many (letter <|> digit <|> symbol)
               let atom = first ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom
-}

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> (oneOf "!$%&|*+-/:<=>?@^_~")
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
                         
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
parseReal sign exct base = decimalTrail sign exct "0" [] False <|> (readReal' sign exct base)
                           
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
        let n1 = apply sign (convert n1raw)
        let n2 = convert $ n2raw ++ inx2
        return $ if length (inx1 ++ inx2) == 0 then RationalE (n1 % n2)
                 else DoubleI (fromIntegral n1 / fromIntegral n2))
    <|>
    (decimalTrail sign exct n1inx inx1 True)
    <|>
    (return $ NumberE $ apply sign (convert n1inx))
    where domain = case base of 2 -> "01"
                                8 -> ['0'..'7']
                                10 -> ['0'..'9']
                                16 -> ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
          convert = fst . head . readInt base (`elem` domain) digitToInt
          apply '-' n = negate n
          apply _ n = n

decimalTrail :: Char -> Parser [Char] -> [Char] -> [Char] -> Bool -> Parser LispVal
decimalTrail sign exct n1 inx1 hasHead = do
  char '.'
  exp <- if length inx1 > 0 then exct else
             (++) <$> (if hasHead then option "0" (many1 digit) else many1 digit) <*> exct
  suff <- suff <|> (return "")
  let decRaw = n1 ++ "." ++ exp ++ suff
  return $ DoubleI $ (apply sign) . fst . head . readFloat $ decRaw
    where apply '-' n = negate n
          apply _ n = n

suff :: Parser [Char]
suff = liftM3 (\a b c -> a ++ b ++ c) (oneOf "eEsSfFdDlL" *> return "e") (pure <$> option '+' (oneOf "+-")) (many1 digit)
                      
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

parseListTail :: [LispVal] -> Parser LispVal
parseListTail xs =
    (do
      spaces
      isDottedTail <- (try (char '.' >> spaces >> return True)) <|> (return False)
      tail <- parseExpr
      if isDottedTail then (char ')') >> (return $ DottedList xs tail) else parseListTail (xs ++ [tail]))
     <|>
     (do
       char ')'
       return $ List xs)

parseList :: Parser LispVal
parseList = do
  char '('
  head <- parseExpr
  x <- parseListTail [head]
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
         
parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseQuoted
            <|> parseList
            <|> parseLispNum
            <|> parseQuasiQuoted
            <|> parseUnquoted

parseUnquoted :: Parser LispVal
parseUnquoted =
    (char ',' >>
              ((char '@' >> liftM (\x -> List [Atom "unquote-splicing", x]) parseList)
              <|> liftM (\x -> List [Atom "unquote", x]) parseExpr))

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = char '`' >> liftM (\x -> List [Atom "quasiquoted", x]) parseExpr
    

testParse :: String -> String
testParse input = case parse parseLispNum "lisp" input of
                    Left err -> "No match: " ++ show err
                    Right v -> show v
