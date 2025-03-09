module Manifold where

data Manifold =
  Manifold Int String String String [String]
  

data Chart =
  Chart Manifold [String] [Int]
  
data ScalarField =
  ScalarField Manifold Chart Function
  
data Function =
  Function String
  
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

  scalarField :: a -> Chart -> Function -> ScalarField
  --constantScalarField :: a -> ConstantScalarField
  openSubset :: a -> String -> [String] -> a
  chart :: a -> [String] -> [Int] -> Chart

-- given a chart on m, each point p in m is in the coordinate domain
-- given a chart, the local map and set of functions (x1,x2,..xn)
-- of the map map(p) = (x1(p), x2(p), ..xn(p))

instance TopologicalManifold Manifold where
  scalarField manifold chart fn = topologicalField manifold chart fn
  openSubset manifold name coordinate = topologicalOpenSubset manifold name coordinate
  chart manifold coords map' = topologicalChart manifold coords map'

topologicalChart :: Manifold -> [String] -> [Int] -> Chart
topologicalChart manifold coordinates points =
{--

A chart on a Topological Manifold M is a pair (U, p') consisting of an
open subset U of M and a homeomorphism p: U -> U' where U' is defined
as p(U') being a subset of R^n.

Here a homeomorphism means that p: U -> U' and p^-1: U -> U' are both continuous.

Given a chart on M, each point p in M is in the coordinate domain.

Given a chart, the local map and set of functions (x1,x2,..xn)
of the map map(p') = (x1(p), x2(p), ..xn(p))

--}

  Chart manifold coordinates points

topologicalField :: Manifold -> Chart -> Function -> ScalarField
topologicalField manifold chart fn =
  ScalarField manifold chart fn

topologicalOpenSubset :: Manifold -> String -> [String] -> Manifold
topologicalOpenSubset (Manifold dimension name structure field _) name' coordinateRestriction =
{--
If an U is an open subset of a manifold M then
every element of U is also an element of M and U is in the
topology of M.
--}
  
  Manifold dimension name' structure field coordinateRestriction
