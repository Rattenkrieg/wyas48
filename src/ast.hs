module Scheme.Ast where
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
    (NumberE i1) == (NumberE i2) = i1 == i2
    (NumberE i1) == (DoubleI d2) = fromIntegral i1 == d2
    (DoubleI d1) == (NumberE i1) = d1 == (fromIntegral i1)
    (DoubleI d1) == (DoubleI d2) = d1 == d2
    (NumberE i1) == (RationalE r2) = (i1 % 1) == r2
    (RationalE r1) == (NumberE i2) = r1 == (i2 % 1)
    (RationalE r1) == (DoubleI d2) = ((fromIntegral $ numerator r1) / (fromIntegral $ denominator r1)) == d2
    (DoubleI d1) == (RationalE r2) = d1 == ((fromIntegral $ numerator r2) / (fromIntegral $ denominator r2))
    (RationalE r1) == (RationalE r2) = r1 == r2
--x /= y = not (x == y)

instance Ord LispNum where
    compare (NumberE i1) (NumberE i2) = compare i1 i2
    compare (NumberE i1) (DoubleI d2) = compare (fromIntegral i1) d2
    compare (DoubleI d1) (NumberE i1) = compare d1 (fromIntegral i1)
    compare (DoubleI d1) (DoubleI d2) = compare d1 d2
    compare (NumberE i1) (RationalE r2) = compare (i1 % 1) r2
    compare (RationalE r1) (NumberE i2) = compare r1 (i2 % 1)
    compare (RationalE r1) (DoubleI d2) = compare ((fromIntegral $ numerator r1) / (fromIntegral $ denominator r1)) d2
    compare (DoubleI d1) (RationalE r2) = compare d1 ((fromIntegral $ numerator r2) / (fromIntegral $ denominator r2))
    compare (RationalE r1) (RationalE r2) = compare r1 r2

instance Real LispNum where
    toRational (NumberE i) = toRational i
    toRational (DoubleI d) = toRational d
    toRational (ComplexI (r :+ i)) = toRational r
    toRational (ComplexEI (r :+ i)) = toRational r
    toRational (ComplexER (r :+ i)) = toRational r
    toRational (RationalE ri) = toRational ri    

                                
instance Enum LispNum where
    toEnum = NumberE . fromIntegral
    fromEnum (NumberE i) = fromInteger i
                            
instance Integral LispNum where
    toInteger (NumberE i) = i
    toInteger (DoubleI d) = round d
    toInteger (ComplexI (r :+ i)) = round r
    toInteger (ComplexEI (r :+ i)) = r
    toInteger (ComplexER (r :+ i)) = (numerator r) `div` (denominator r)
    toInteger (RationalE r) = (numerator r) `div` (denominator r)
    
instance Num LispNum where
    negate (NumberE a) = NumberE $ negate a
    negate (DoubleI a) = DoubleI $ negate a
    negate (ComplexI a) = ComplexI $ negate a
    negate (ComplexEI (a :+ b)) = ComplexI $ negate ((fromInteger a) :+ (fromInteger b))
    negate (ComplexER (a :+ b)) = ComplexI $ negate ((fromRational a) :+ (fromRational b))
    negate (RationalE a) = RationalE $ negate a
    (+) (NumberE i1) (NumberE i2) = NumberE $ i1 + i2
    (+) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) + d2
    (+) (DoubleI d1) (NumberE i2) = DoubleI $ d1 + (fromIntegral i2)
    (+) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 + d2
    (+) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) + r2
    (+) (RationalE r1) (NumberE i2) = RationalE $ r1 + (i2 % 1)
    (+) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) + d2
    (+) d@(DoubleI _) r@(RationalE _) = r + d
    (+) (RationalE r1) (RationalE r2) = RationalE $ r1 + r2
    (*) (NumberE i1) (NumberE i2) = NumberE $ i1 * i2
    (*) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) * d2
    (*) d@(DoubleI _) n@(NumberE _) =  n * d
    (*) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 * d2
    (*) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) * r2
    (*) r@(RationalE _) n@(NumberE _) = n * r
    (*) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) * d2
    (*) d@(DoubleI _) r@(RationalE _) = r * d
    (*) (RationalE r1) (RationalE r2) = RationalE $ r1 * r2
    fromInteger = NumberE
    abs (NumberE i) = NumberE $ abs i
    abs (DoubleI d) = DoubleI $ abs d
    abs (ComplexI d) = ComplexI $ abs d
    abs (ComplexEI (a :+ b)) = ComplexI $ abs ((fromInteger a) :+ (fromInteger b))
    abs (ComplexER (a :+ b)) = ComplexI $ abs ((fromRational a) :+ (fromRational b))
    abs (RationalE ri) = RationalE $ abs ri
    signum (NumberE i) = NumberE $ signum i
    signum (DoubleI d) = DoubleI $ signum d
    signum (ComplexI d) = ComplexI $ signum d
    signum (ComplexEI (a :+ b)) = signum $ ComplexI ((fromInteger a) :+ (fromInteger b))
    signum (ComplexER (a :+ b)) = signum $ ComplexI ((fromRational a) :+ (fromRational b))
    signum (RationalE ri) = RationalE $ signum ri

instance Fractional LispNum where
    (/) (NumberE i1) (NumberE i2) = DoubleI $ (fromIntegral i1) / (fromIntegral i2)
    (/) (NumberE i1) (DoubleI d2) = DoubleI $ (fromIntegral i1) / d2
    (/) (DoubleI d1) (NumberE i1) = DoubleI $ d1 / (fromIntegral i1)
    (/) (DoubleI d1) (DoubleI d2) = DoubleI $ d1 / d2
    (/) (NumberE i1) (RationalE r2) = RationalE $ (i1 % 1) / r2
    (/) (RationalE r1) (NumberE i2) = RationalE $ r1 / (i2 % 1)
    (/) (RationalE r1) (DoubleI d2) = DoubleI $ (fromIntegral $ numerator r1) / (fromIntegral $ denominator r1) / d2
    (/) (DoubleI d1) (RationalE r2) = DoubleI $ d1 / ((fromIntegral $ numerator r2) / (fromIntegral $ denominator r2))
    (/) (RationalE r1) (RationalE r2) = RationalE $ r1 / r2
    fromRational = DoubleI . fromRational
