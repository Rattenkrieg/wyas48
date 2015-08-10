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


instance Eq LispNum where
{- (==) -}    
(==) (NumberE i1) (NumberE i2) = i1 Prelude.== i2
(==) (NumberE i1) (DoubleI d2) = Prelude.fromIntegral i1 Prelude.== d2
(==) (DoubleI d1) (NumberE i1) = d1 Prelude.== (fromIntegral i1)
(==) (DoubleI d1) (DoubleI d2) = d1 Prelude.== d2
(==) (NumberE i1) (RationalE r2) = (i1 % 1) Prelude.== r2
(==) (RationalE r1) (NumberE i2) = r1 Prelude.== (i2 % 1)
(==) (RationalE r1) (DoubleI d2) = ((fromIntegral $ numerator r1) Prelude./ (fromIntegral $ denominator r1)) Prelude.== d2
(==) (DoubleI d1) (RationalE r2) = d1 Prelude.== ((fromIntegral $ numerator r2) Prelude./ (fromIntegral $ denominator r2))
(==) (RationalE r1) (RationalE r2) = r1 Prelude.== r2

instance Ord LispNum where
{- compare -}    
compare (NumberE i1) (NumberE i2) = Prelude.compare i1 i2
compare (NumberE i1) (DoubleI d2) = Prelude.compare (Prelude.fromIntegral i1) d2
compare (DoubleI d1) (NumberE i1) = Prelude.compare d1 (fromIntegral i1)
compare (DoubleI d1) (DoubleI d2) = Prelude.compare d1 d2
compare (NumberE i1) (RationalE r2) = Prelude.compare (i1 % 1) r2
compare (RationalE r1) (NumberE i2) = Prelude.compare r1 (i2 % 1)
compare (RationalE r1) (DoubleI d2) = Prelude.compare ((fromIntegral $ numerator r1) Prelude./ (fromIntegral $ denominator r1)) d2
compare (DoubleI d1) (RationalE r2) = Prelude.compare d1 ((fromIntegral $ numerator r2) Prelude./ (fromIntegral $ denominator r2))
compare (RationalE r1) (RationalE r2) = Prelude.compare r1 r2
               
instance Real LispNum where
{- toInteger -}
toRational (NumberE i) = Prelude.toRational i
toRational (DoubleI d) = Prelude.toRational d
toRational (ComplexI (r :+ i)) = Prelude.toRational r
toRational (ComplexEI (r :+ i)) = Prelude.toRational r
toRational (ComplexER (r :+ i)) = Prelude.toRational r
toRational (RationalE ri) = Prelude.toRational ri    

instance Enum LispNum where
toEnum = NumberE
fromEnum (NumberE i) = i
                            
instance Integral LispNum where
{- toInteger -}
toInteger (NumberE i) = i
toInteger (DoubleI d) = Prelude.round d
toInteger (ComplexI (r :+ i)) = Prelude.round r
toInteger (ComplexEI (r :+ i)) = r
toInteger (ComplexER (r :+ i)) = (numerator r) `div` (denominator r)
toInteger (RationalE r) = (numerator r) `div` (denominator r)

instance Num LispNum where
{- negate-}
negate (NumberE a) = NumberE $ Prelude.negate a
negate (DoubleI a) = DoubleI $ Prelude.negate a
negate (ComplexI a) = ComplexI $ Prelude.negate a
negate (ComplexEI (a :+ b)) = ComplexI $ Prelude.negate ((Prelude.fromInteger a) :+ (Prelude.fromInteger b))
negate (ComplexER (a :+ b)) = ComplexI $ Prelude.negate ((Prelude.fromRational a) :+ (Prelude.fromRational b))
negate (RationalE a) = RationalE $ Prelude.negate a
{- (+) -}
(+) (NumberE i1) (NumberE i2) = NumberE $ i1 Prelude.+ i2
(+) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) Prelude.+ d2
(+) (DoubleI d1) (NumberE i2) = DoubleI $ d1 Prelude.+ (fromIntegral i2)
(+) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 Prelude.+ d2
(+) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) Prelude.+ r2
(+) (RationalE r1) (NumberE i2) = RationalE $ r1 Prelude.+ (i2 % 1)
(+) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) Prelude./ (fromIntegral $ denominator r1) Prelude.+ d2
(+) d@(DoubleI _) r@(RationalE _) = r Ast.+ d
(+) (RationalE r1) (RationalE r2) = RationalE $ r1 Prelude.+ r2
{- (-) -}
(-) l1 l2 = l1 Ast.+ (Ast.negate l2)
{- (*) -}
(*) (NumberE i1) (NumberE i2) = NumberE $ i1 Prelude.* i2
(*) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) Prelude.* d2
(*) d@(DoubleI _) n@(NumberE _) =  n Prelude.* d
(*) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 Prelude.* d2
(*) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) Prelude.* r2
(*) r@(RationalE _) n@(NumberE _) = n Prelude.* r
(*) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) Prelude./ (fromIntegral $ denominator r1) Prelude.* d2
(*) d@(DoubleI _) r@(RationalE _) = r Prelude.* d
(*) (RationalE r1) (RationalE r2) = RationalE $ r1 Prelude.* r2
{- fromInteger -}
fromInteger = NumberE
{- abs -}
abs (NumberE i) = NumberE $ Prelude.abs i
abs (DoubleI d) = DoubleI $ Prelude.abs d
abs (ComplexI d) = ComplexI $ Prelude.abs d
abs (ComplexEI (a :+ b)) = ComplexI $ Prelude.abs ((Prelude.fromInteger a) :+ (Prelude.fromInteger b))
abs (ComplexER (a :+ b)) = ComplexI $ Prelude.abs ((Prelude.fromRational a) :+ (Prelude.fromRational b))
abs (RationalE ri) = RationalE $ Prelude.abs ri
{- signum -}
signum (NumberE i) = NumberE $ Prelude.signum i
signum (DoubleI d) = DoubleI $ Prelude.signum d
signum (ComplexI d) = ComplexI $ Prelude.signum d
signum (ComplexEI (a :+ b)) = Ast.signum $ ComplexI ((Prelude.fromInteger a) :+ (Prelude.fromInteger b))
signum (ComplexER (a :+ b)) = Ast.signum $ ComplexI ((Prelude.fromRational a) :+ (Prelude.fromRational b))
signum (RationalE ri) = RationalE $ Prelude.signum ri

instance Fractional LispNum where
{- (*) -}
(/) (NumberE i1) (NumberE i2) = DoubleI $ (Prelude.fromIntegral i1) Prelude./ (Prelude.fromIntegral i2)
(/) (NumberE i1) (DoubleI d2) = DoubleI $ (Prelude.fromIntegral i1) Prelude./ d2
(/) (DoubleI d1) (NumberE i1) = DoubleI $ d1 Prelude./ (fromIntegral i1)
(/) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 Prelude./ d2
(/) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) Prelude./ r2
(/) (RationalE r1) (NumberE i2) = RationalE $ r1 Prelude./ (i2 % 1)
(/) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) Prelude./ (fromIntegral $ denominator r1) Prelude./ d2
(/) (DoubleI d1) (RationalE r2) = DoubleI $ d1 Prelude./ ((fromIntegral $ numerator r2) Prelude./ (fromIntegral $ denominator r2))
(/) (RationalE r1) (RationalE r2) = RationalE $ r1 Prelude./ r2
{- fromRational -}
fromRational = DoubleI . Prelude.fromRational
