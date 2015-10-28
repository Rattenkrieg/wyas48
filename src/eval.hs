{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Eval where
import Scheme.Ast as Ast
import Data.Ratio
import Control.Monad.Error    
import Text.ParserCombinators.Parsec
import Control.Monad

type ThrowsError = Either LispError                                              
    
eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True -> eval conseq
    wtf -> throwError $ TypeMismatch "Bool" wtf
eval (List (Atom "case" : key : clauses)) = caseClause key clauses
eval (List (Atom "cond" : clauses)) = condClause clauses
eval (List (Atom func : args)) = mapM eval args >>= apply func                 
eval lispVal = return lispVal
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
           
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args)" func)
                  ($ args) (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem)),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              --("make-string", makeString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]
             
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

--caseClause :: LispVal -> [LispVal] -> ThrowsError LispVal
caseClause test ((List (List datums : exprs)) : clauses) = do
    evaled <- mapM ((>>= eqv) . sequence . (: [key]) . eval) datums
    if any isTrue evaled
    then last `fmap` mapM eval exprs
    else caseClause test clauses
        where key = eval test
              isTrue (Bool True) = True
              isTrue _ = False
caseClause _ ((List (Atom "else" : exprs)) : []) = last `fmap` mapM eval exprs
caseClause _ [] = throwError $ BadSpecialForm "no else clause after failed test clauses" (Atom "case")
caseClause _ ((List (Atom "else" : exprs)) : xs) = throwError $ BadSpecialForm "more test clauses after else clause in cond for" (head xs)    

condClause :: [LispVal] -> ThrowsError LispVal
condClause (List [test, Atom "=>", Atom expr] : xs) = do
  result <- eval test
  case result of
    Bool False -> condClause xs
    v -> apply expr [v]
condClause ((List (Atom "else" : exprs)) : []) = last `fmap` mapM eval exprs
condClause ((List (Atom "else" : exprs)) : xs) = throwError $ BadSpecialForm "more test clauses after else clause in cond for" (head xs)
condClause ((List (test : exprs)) : xs) = do
  result <- eval test
  case result of
   Bool True -> last `fmap` mapM eval exprs
   Bool False -> condClause xs
   wtf -> throwError $ TypeMismatch "Bool" wtf
condClause [] = throwError $ BadSpecialForm "no else clause after failed test clauses" (Atom "cond")

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
  `catchError` (const $ return False)
  --where eqq (Number a) (Number b) = a == b
    --    eqq a b = a Prelude.== b
             
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
      
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
                
unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
             
numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--genericOp :: LispVal -> LispVal -> (LispNum -> LispNum -> LispNum) -> LispVal
genericOp (Number a) (Number b) op = Number (a `op` b)
{-
genericOp a (Number b) op = Number ((NumberE 0) `op` b)
genericOp (Number a) b op = Number (a `op` (NumberE 0))
genericOp _ _ op = Number (NumberE 0)
-}
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
{-
makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number (NumberE k)] = return $ take k $ repeat "_"
makeString [Number (NumberE k), String s] = return $ take k $ repeat $ head s
makeString ((String s):xs) = head s ++ 
-}           
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
