module Ast where
import Data.Ratio
import Data.Complex

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | String String
               | Bool Bool
               | Number LispNum
                 deriving Show

data LispNum = NumberE Integer
             | DoubleI Double
             | ComplexI (Complex Double)
             | ComplexEI (Complex Integer)
             | ComplexER (Complex (Ratio Integer))
             | RationalE (Ratio Integer)
               deriving Show

instance Num LispNum where
{- negate-}
negate (NumberE a) = NumberE $ Prelude.negate a
negate (DoubleI a) = DoubleI $ Prelude.negate a
negate (ComplexI a) = ComplexI $ Prelude.negate a
negate (ComplexEI a) = ComplexEI $ Prelude.negate a
negate (ComplexER a) = ComplexER $ Prelude.negate a
negate (RationalE a) = RationalE $ Prelude.negate a
{- (+) -}
(+) (NumberE i1) (NumberE i2) = NumberE $ i1 Prelude.+ i2
(+) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) Prelude.+ d2
(+) d@(DoubleI _) n@(NumberE _) =  n Prelude.+ d
(+) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 Prelude.+ d2
(+) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) Prelude.+ r2
(+) r@(RationalE _) n@(NumberE _) = n Prelude.+ r
(+) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) Prelude.+ d2
(+) d@(DoubleI _) r@(RationalE _) = r Prelude.+ d
(+) (RationalE r1) (RationalE r2) = RationalE $ r1 Prelude.+ r2
{- (*) -}
(*) (NumberE i1) (NumberE i2) = NumberE $ i1 Prelude.* i2
(*) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) Prelude.* d2
(*) d@(DoubleI _) n@(NumberE _) =  n Prelude.* d
(*) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 Prelude.* d2
(*) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) Prelude.* r2
(*) r@(RationalE _) n@(NumberE _) = n Prelude.* r
(*) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) Prelude.* d2
(*) d@(DoubleI _) r@(RationalE _) = r Prelude.* d
(*) (RationalE r1) (RationalE r2) = RationalE $ r1 Prelude.* r2
{- fromInteger -}
fromInteger = NumberE
{- abs -}
abs (NumberE i) = NumberE $ Prelude.abs i
abs (DoubleI d) = DoubleI $ Prelude.abs d
abs (ComplexI d) = ComplexI $ Prelude.abs d
abs (ComplexEI ci) = ComplexEI $ Prelude.abs ci
abs (ComplexER cri) = ComplexER $ Prelude.abs cri
abs (RationalE ri) = RationalE $ Prelude.abs ri
{- signum -}
signum (NumberE i) = NumberE $ Prelude.signum i
signum (DoubleI d) = DoubleI $ Prelude.signum d
signum (ComplexI d) = ComplexI $ Prelude.signum d
signum (ComplexEI (a :+ b)) = Ast.signum $ ComplexI ((Prelude.fromInteger a) :+ (Prelude.fromInteger b))
signum (ComplexER (a :+ b)) = Ast.signum $ ComplexI ((fromRational a) :+ (fromRational b))
signum (RationalE ri) = RationalE $ Prelude.signum ri
