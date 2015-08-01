module Eval where
import Ast
import Data.Ratio    
    
eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval lispVal = lispVal

              
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (div)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem))]

numericBinop :: Num a => (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op = foldl1 (\a b -> genericOp a b op)

genericOp a@(LispNum _) b@(LispNum _) op = a `op` b
genericOp a@(LispVal _) b@(LispNum _) op = (NumberE 0) `op` b
genericOp a@(LispNum _) b@(LispVal _) op = genericOp b a
genericOp _ _ op = NumberE 0
