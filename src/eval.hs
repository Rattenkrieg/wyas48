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
numericBinop op params = foldl1 (\a b -> commonNumOp a b op) params

commonNumOp :: Num a => LispVal -> LispVal -> (a -> a -> a) -> LispVal
commonNumOp (NumberE i1) (NumberE i2) op = NumberE $ i1 `op` i2
commonNumOp (NumberE i1) (DoubleI d2) op = DoubleI $ (fromIntegral i1) `op` d2
commonNumOp d@(DoubleI _) n@(NumberE _) op = commonNumOp n d op
commonNumOp (DoubleI d1) (DoubleI d2) op = DoubleI $ d1 `op` d2
commonNumOp (NumberE i1) (RationalE r2) op = RationalE $ (i1 % 1) `op` r2
commonNumOp r@(RationalE _) n@(NumberE _) op = commonNumOp n r op
commonNumOp (RationalE r1) (DoubleI d2) op = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) `op` d2
commonNumOp d@(DoubleI _) r@(RationalE _) op = commonNumOp r d op
commonNumOp (RationalE r1) (RationalE r2) op = RationalE $ r1 `op` r2
