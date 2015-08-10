module Eval where
import Ast
import Data.Ratio
import Control.Monad.Error    
import Text.ParserCombinators.Parsec
import Control.Monad

type ThrowsError = Either LispError                                              
    
eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>=  apply func
eval lispVal = return lispVal
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
           
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args)" func)
                  ($ args) (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (Ast.+)),
              ("-", numericBinop (Ast.-)),
              ("*", numericBinop (Ast.*)),
              ("/", numericBinop (Ast./)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem)),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)]

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = return $ foldl1 (\a b -> genericOp a b op) params

--genericOp :: LispVal -> LispVal -> (LispNum -> LispNum -> LispNum) -> LispVal
genericOp (Number a) (Number b) op = Number (a `op` b)
genericOp a (Number b) op = Number ((NumberE 0) `op` b)
genericOp (Number a) b op = Number (a `op` (NumberE 0))
genericOp _ _ op = Number (NumberE 0)

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom a)] = return $ String a
symbolToString (x:xs) = throwError $ TypeMismatch "lol" x
                            
stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String s)] = return $ Atom s
stringToSymbol (x:xs) = throwError $ TypeMismatch "lol" x


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ (unwords $ map show found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


