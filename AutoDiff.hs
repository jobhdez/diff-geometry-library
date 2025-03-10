module AutoDiff where

{--
this a port of my dual number implementation
for the coalton programming language.
--}

data Dual a =
  Dual a a
  deriving (Show, Eq)

square :: Num a => a -> a
square a = a*a

instance Num a => Num (Dual a) where
  p1 + p2 = dualNumAdd p1 p2
  p1 - p2 = dualNumSub p1 p2
  p1 * p2 = dualNumMul p1 p2
  signum p1 = dualNumSignum p1
  abs p1 = dualNumAbs p1
  fromInteger n = dualNumFromInteger n
  
dualNumAdd :: Num a => (Dual a) -> (Dual a) -> (Dual a)
dualNumAdd (Dual p1 d1) (Dual p2 d2) =
  Dual (p1 + p2) (d1 + d2)

dualNumSub :: Num a => (Dual a) -> (Dual a) -> (Dual a)
dualNumSub (Dual p1 d1) (Dual p2 d2) =
  Dual (p1 - p2) (p2 - d2)

dualNumMul :: Num a => (Dual a) -> (Dual a) -> (Dual a)
dualNumMul (Dual p1 d1) (Dual p2 d2) =
  Dual (p1 * p2) ((p1 * d2) + (d1 * p2))
  
dualNumSignum :: Num a => (Dual a) -> (Dual a)
dualNumSignum (Dual p1 d1) =
  Dual (signum p1) 0
dualNumAbs :: Num a => (Dual a) -> Dual a
dualNumAbs (Dual p1 d1) =
  Dual (abs p1) 0

dualNumFromInteger :: Num a => Integer -> Dual a
dualNumFromInteger a =
  Dual (fromInteger a) 0

instance (Fractional a) => Fractional (Dual a) where
  p1 / p2 = dualFracDiv p1 p2
  fromRational n = dualFracRational n
  recip p1 = dualRecip p1

dualFracDiv :: Fractional a => (Dual a) -> (Dual a) -> (Dual a)
dualFracDiv (Dual p1 d1) (Dual p2 d2) =
  Dual (p1 / d1) (((d1 * p2) - (p1 * d2)) / square p2)

dualFracRational :: Fractional a => Rational -> Dual a
dualFracRational n =
  Dual (fromRational n) 0
  
dualRecip :: Fractional a => Dual a -> Dual a
dualRecip (Dual p1 d1) =
  Dual (recip p1) ((negate d1) / (square p1))

class Trig a where
  sin' :: a -> a
  cos' :: a -> a
  tan' :: a -> a
  asin' :: a -> a
  acos' :: a -> a
  atan' :: a -> a
  pi' :: a

instance Trig Double where
  sin' x = sin x
  cos' x = cos x
  tan' x = tan x
  asin' x = asin x
  acos' x = acos x
  atan' x = atan x
  pi' = pi

instance Trig Float where
  sin' x = sin x
  cos' x = cos x
  tan' x = tan x
  asin' x = asin x
  acos' x = acos x
  atan' x = atan x
  pi' = pi
  
instance (Floating a, Num a, Trig a) => (Trig (Dual a)) where
  sin' p1 = dualSin p1
  cos' p1 = dualCos p1
  tan' p1 = dualTan p1
  asin' p1 = dualAsin p1
  acos' p1 = dualAcos p1
  atan' p1 = dualAtan p1
  pi' = (Dual pi 0)

dualSin :: Floating a => Dual a -> Dual a
dualSin (Dual p1 d1) =
  Dual (sin p1) (d1 * (cos p1))

dualCos :: Floating a => Dual a -> Dual a
dualCos (Dual p1 d1) =
  Dual (cos p1) (negate (d1 * (sin p1)))

dualTan :: Floating a => Dual a -> Dual a
dualTan (Dual p1 d1) =
  Dual (asin p1) (d1 / ((cos p1) * (cos p1)))

dualAsin :: Floating a => Dual a -> Dual a
dualAsin (Dual p1 d1) =
  Dual (asin p1) (d1 / (sqrt (1 - (p1 * p1))))

dualAcos :: Floating a => Dual a -> Dual a
dualAcos (Dual p1 d1) =
  Dual (acos p1) (negate (d1 / (sqrt (1 - (p1 * p1)))))

dualAtan :: Floating a => Dual a -> Dual a
dualAtan (Dual p1 d1) =
  Dual (atan p1) (d1 / (1 + (p1 * p1)))

-- example
s :: Double
s = sin(3 + 1)
sd :: Double
sd = cos(3 + 1)
dual :: Dual Double
dual = Dual s sd

-- example 1
-- f(x) = 4x + 3
-- f(3)
-- 3 -> dual
---- dual = (Dual 3 1)
----   1 is the derivative of 3
---- mulitply by constant 3
---- dual2 = (Dual 3  0)
---- dual * dual2

{--
ghci> dual = Dual 4 1
dual = Dual 4 1
ghci> dual2 = Dual 3 0
dual2 = Dual 3 0
ghci> dual3 = dual * dual2
dual3 = dual * dual2
ghci> dual3
dual3
Dual 12 3
ghci> df = Dual 2 0
df = Dual 2 0
ghci> dual3 + df
dual3 + df
Dual 14 3
---}

{--
z = f(x y) = x^2 + xy + y^2

fn = partialDiff(z, x)

z -> f(x) = x^2 + x*1 + 1^2

fn(1) ->

ghci> d1 = Dual 1 1 * Dual 1 1

ghci> d2 = Dual 1 1 * Dual 1 0
d2 = Dual 1 1 * Dual 1 0
ghci> d3 = d1 + d2
d3 = d1 + d2
ghci> d3 + Dual 1 0
d3 + Dual 1 0
Dual 3 3
ghci> d3
d3
Dual 2 3
--}
{--
f(y) = 2 + 1y

y = 1
d = Dual 1 1
d' = d * Dual 1 0
d'' = d' * Dual 2 0
--}
