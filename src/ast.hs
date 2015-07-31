module Ast where
import Data.Ratio
import Data.Complex    

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

    
