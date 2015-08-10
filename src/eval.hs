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
primitives = [("+", numericBinop (Ast.+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (Ast.*)),
              ("/", numericBinop (Ast./)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem))]

--numericBinop :: Num a => (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op = foldl1 (\a b -> genericOp a b op)

--genericOp :: LispVal -> LispVal -> (LispNum -> LispNum -> LispNum) -> LispVal
genericOp (Number a) (Number b) op = Number (a `op` b)
genericOp a (Number b) op = Number ((NumberE 0) `op` b)
genericOp (Number a) b op = Number (a `op` (NumberE 0))
genericOp _ _ op = Number (NumberE 0)
