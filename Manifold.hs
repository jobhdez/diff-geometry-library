module Manifold where

import AutoDiff ( Dual(Dual))
data Manifold =
  Manifold Int String String String [String]

data VectorField =
  VectorField [MathExpr]

data Vector =
  Vector [Int]
  deriving Show
type Coordinates = [String]
type Point = [Int]

type ManifoldName = String

data Chart =
  Chart Manifold [String] [Int]
  
data ScalarField =
  ScalarField Manifold Chart MathExpr
  
-- f(x,y) = 2x + y
data MathExpr =
  Var String
  | Num Int
  | Plus MathExpr MathExpr
  | Mul MathExpr MathExpr

  
class TopologicalManifold a where
{--

Suppose you have a set X.

Then a topology T on X is a collection of subsets of X called open subsets
that satisfy:

(1) X and the empty set are open.
(2) The union of open subsets is open.
(3) The intersection of finite open subsets is open.

Note: Here `open` means that if a subset U is open in X then U is in the
topology of X.

A Topological Manifold is a topological space with a given
structure that encodes the meaning that a manifold looks locally like
R^n.

Suppose M is a topological space.

Then M consists of the following structure that makes it behave like
Euclidian space R^n.

(1) M is a Hausdorff space.
(2) M is second countable
(3) M is locally euclidian of dimension n.

References:

[1] Introduction to Smooth Manifolds by John Lee
[2] Sage Manifolds

--}

  scalarField :: a -> Chart -> MathExpr -> ScalarField
  --constantScalarField :: a -> ConstantScalarField
  openSubset :: a -> ManifoldName -> Coordinates -> a
  chart :: a -> Coordinates -> Point -> Chart

-- given a chart on m, each point p in m is in the coordinate domain
-- given a chart, the local map and set of functions (x1,x2,..xn)
-- of the map map(p) = (x1(p), x2(p), ..xn(p))


class TopologicalManifold a => SmoothManifold a where
{--
A Smooth Manifold is a Toplogical Manifold with a given structure that allow us
to make sense of derivatives on real value functions, curve between manifolds.

Let U and V be open subsets of a manifold M.

Then f: U -> V is smooth if each component function has continuous partial derivatives of
all order.

References:
[1] Introduction to Smooth Manifolds by Jonh Lee
[2] Sage Manifolds
--}

  tangentVector :: a -> VectorField -> Point -> Vector
{--
class TangentSpace a where
  tangetVectors :: [a] -> Point -> ([a], Point)

instance TangentSpace TangentVectors where
  tangentVectors vectors point = tangentVectors' vectors point
--}

instance TopologicalManifold Manifold where
  scalarField manifold chart fn = topologicalField manifold chart fn
  openSubset manifold name coordinate = topologicalOpenSubset manifold name coordinate
  chart manifold coords map' = topologicalChart manifold coords map'

instance SmoothManifold Manifold where
  --scalarField manifold chart fn = topologicalField manifold chart fn
  --openSubset manifold name coordinate = topologicalOpenSubset manifold name coordinate
  --chart manifold coords map' = topologicalChart manifold coords map'
  --tangentSpace manifold vectorFields point = tangentSpace' manifold vectorFields point
  tangentVector fields manifold point = makeTangentVector fields manifold point

topologicalChart :: Manifold -> Coordinates -> Point -> Chart
topologicalChart manifold coordinates points =
{--

A chart on a Topological Manifold M is a pair (U, p') consisting of an
open subset U of M and a homeomorphism p: U -> U' where U' is defined
as p(U') being a subset of R^n.

Here a homeomorphism means that p: U -> U' and p^-1: U'-> U are both continuous.

Given a chart on M, each point p in M is in the coordinate domain.

Given a chart, the local map and set of functions (x1,x2,..xn)
of the map map(p') = (x1(p), x2(p), ..xn(p))

--}

  Chart manifold coordinates points

topologicalField :: Manifold -> Chart -> MathExpr -> ScalarField
topologicalField manifold chart fn =
  ScalarField manifold chart fn

topologicalOpenSubset :: Manifold -> ManifoldName -> Coordinates -> Manifold
topologicalOpenSubset (Manifold dimension name structure field _) name' coordinateRestriction =
{--
If an U is an open subset of a manifold M then
every element of U is also an element of M and U is in the
topology of M.
--}
  
  Manifold dimension name' structure field coordinateRestriction

  
class TangentVector a where
  makeTangentVector :: Manifold -> a -> Point -> Vector
  --directionalDiff :: a -> MathExpr -> Int

instance TangentVector VectorField where
  makeTangentVector manifold vectorField point = makeTangentVector' manifold vectorField point
  --directionalDiff mathExpr vector = directionalDiff' mathExpr vector

manifoldDimension :: Manifold -> Int
manifoldDimension (Manifold dimension _ _ _ _) =
  dimension
makeTangentVector' :: Manifold -> VectorField -> Point -> Vector
makeTangentVector' manifold (VectorField fields)  point =
  -- f(x,y) = 2x + y
  -- f(2,1) = 5
  let n = evaluate (head fields) point in
    Vector [n, n]

evaluate :: MathExpr -> Point -> Int
evaluate (Plus (Mul (Num n) var) (Var var')) point =
  let d1 = Dual n 1
      d2 = Dual 1 0
      d3 = Dual (head point) 0
      d4 = (d1 + d2) + d3 in
    primalPart d4


primalPart :: Dual a -> a
primalPart (Dual primal _) =
  primal

dualPart :: Dual a -> a
dualPart (Dual _ dual) =
  dual

diff :: MathExpr -> String -> Int
diff (Plus (Mul (Num n) x) (Var y)) "x"  =
  -- with respect to x
   let d1 = Dual n 1
       d2 = Dual 1 0
       d3 = Dual 2 0
       d4 = (d1 + d2) + d3 in
     dualPart d4
  
  
diff (Plus (Mul n x) (Var y)) "y"  =
  -- with respect to y
  let d = Dual 1 1
      d' =  Dual 1 0
      d'' = d' * d
      d''' = d'' + Dual 2 0 in
    dualPart d'''
      
  
  
directionalDiff' :: MathExpr -> Vector ->  Int
directionalDiff' expr (Vector vec) =
  let diffx = diff expr "x"
      diffy = diff expr "y"
      e1 = head vec
      e2 = head (tail vec) in
    (e1 * diffx) + (e2 * diffy)
