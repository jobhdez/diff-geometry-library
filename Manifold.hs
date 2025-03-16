module Manifold where

import qualified Data.Map as Map
import AutoDiff (Dual(Dual), sin', cos', tan', asin', acos', atan')

{--
based on my current code whhat are the decisons unresolved?
    from just experiementing a bit its a hassle to to first construct
the scalar field by first first construction the chart and then manifold.
to much work. i need a better and easier way to mess with
        scalar fields
        vector fields and so on ie not friendly interface
the sage manifolds library has optimizations, parallel programming.
because some computations are expensive.
so for the tensor stuff need to use mutable arrays.
take a look at the ocaml paper. now the ocaml book binds the ocaml
library to c. i can maybe target c code and maybe this is a good way
to apply optimizations. dont know. but needs to fast.

in order to get something up and running quickly i covered trivial
cases. you can see my code and the sage code. the sage code is
worthy of a library.
    so you must generalize the any dimension
    you need a proper way to organize your library

you need to apply low level to haskell.

when you constructing data what are you allocating?
need to think in terms pf bytes etc. by using this you will
improve performance.

another decision i made was to just go from mathexp to dual number. but
i didnt add a way to display the expression and i need to go from
expression to expression as well.

    
also, the structure of the subset of diff geometry doesnt reflect
with how its described.
        so you would need to build some vector space library
--}
import AutoDiff ( Dual(Dual) )

data Tensor =
  Scalar Float
  | Vector [Float]
  | Matrix [[Float]]
  | Tensor Tensor
  
type Dimension = Int
type Name = String

data Manifold =
  Manifold Dimension Name
  deriving Show

data VectorField =
  VectorField [MathExpr]
  
type Variable = String
{--data Vector =
  Vector [Float]
  deriving Show--}

--data Subsets =
 -- Subsets (Map.Map String Manifold)
  
type Coordinates = [MathExpr]
type Point = [Float]

data Chart' =
  Chart' Manifold Coordinates Point
  deriving Show

getPoint :: Chart' -> [Float]
getPoint (Chart' manifold coord point) =
  point
  
getCoordinates :: Chart' -> [MathExpr]
getCoordinates (Chart' manifold coord point) =
  coord
data ScalarField =
  ScalarField Manifold Chart' MathExpr

getChart :: ScalarField -> Chart'
getChart (ScalarField manifold chart _) =
  chart
  
-- f(x,y) = 2x + y
data MathExpr =
  Var String
  | Num Float
  | Plus MathExpr MathExpr
  | Mul MathExpr MathExpr
  | Sin MathExpr
  | Cos MathExpr
  | Tan MathExpr
  | Asin MathExpr
  | Acos MathExpr
  | Atan MathExpr
  | Exp MathExpr
  | Ln MathExpr
  | Power MathExpr MathExpr
  | Log MathExpr MathExpr
  | Eq MathExpr MathExpr
  deriving Show

  
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

  scalarField :: a -> Chart' -> MathExpr -> ScalarField
  --constantScalarField :: a -> ConstantScalarField
  openSubset :: a -> Manifold -> Map.Map String Manifold -> Map.Map String Manifold
  chart :: a -> Coordinates -> Point -> Chart'
  --subsets :: a -> [Manifold]

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

  tangentVector :: a -> VectorField -> Point -> Tensor
{--
class TangentSpace a where
  tangetVectors :: [a] -> Point -> ([a], Point)

instance TangentSpace TangentVectors where
  tangentVectors vectors point = tangentVectors' vectors point
--}
{--
class SmoothMap a where
  differential :: a -> Point -> Morphism
  jacobianMatrix :: a -> Chart -> Chart -> Matrix
  pullBack :: a -> Tensor -> Tensor
  pushForward :: a -> Tensor -> Tensor
--}

class Chart a where
  frame :: a -> [String]
  --restrict :: a -> Manifold -> a
  transitionMap :: a -> a -> [MathExpr] -> Coordinates
  --function :: a -> Chart -> MathExpr
  jacobianMatrix :: a -> Tensor
  --jacobianDet :: a -> Coordinates -> MatrixFunction

{--
class VectorField a where
  crossProduct :: a -> Metric -> VectorField
  dotProduct :: a -> Metric -> DiffScalarField
  norm :: a -> Metric -> DiffScalarField
--}

instance Chart Chart' where
  transitionMap chart chart' = transitionMap' chart chart
  jacobianMatrix chart = jacobianMatrix' chart
  frame chart = frame' chart
  
instance TopologicalManifold Manifold where
  scalarField manifold chart fn = topologicalField manifold chart fn
  openSubset manifold manifold' = topologicalOpenSubset manifold manifold'
  chart manifold coords map' = topologicalChart manifold coords map'

instance SmoothManifold Manifold where
  tangentVector fields manifold point = makeTangentVector fields manifold point

topologicalChart :: Manifold -> Coordinates -> Point -> Chart'
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

  Chart' manifold coordinates points

topologicalField :: Manifold -> Chart' -> MathExpr -> ScalarField
topologicalField manifold chart fn =
  ScalarField manifold chart fn

topologicalOpenSubset :: Manifold -> Manifold -> Map.Map String Manifold -> Map.Map String Manifold
topologicalOpenSubset manifold manifold' subsets =
{--
If an U is an open subset of a manifold M then
every element of U is also an element of M and U is in the
topology of M.

This means that if A and B are open subsets in M then
subsets(M) = [A, B]

if C is an open subset of A then

subsets(A) = [A, C]
--}
  let subsets' = subsetsMap manifold manifold' subsets
      subsets'' = subsetsMap manifold' manifold' subsets' in
    subsets''

getManifoldName :: Manifold -> String
getManifoldName (Manifold dim name) =
  name
{--
class PseudoRiemannMetric a where
  christoffel_symbols :: a -> Chart -> MathExpr
  connection :: a -> Connection
  determinant :: Frame -> DiffScalarField
  inverse :: a -> Tensor
  restrict :: a  ->  a
  ricci :: a -> Tensor
  ricciScalar :: a -> DiffScalarField
  riemann :: a -> Tensor
  restrict :: a -> Restriction
--}

class TangentVector a where
  makeTangentVector :: Manifold -> a -> Point -> Tensor

instance TangentVector VectorField where
  makeTangentVector manifold vectorField point = makeTangentVector' manifold vectorField point

manifoldDimension :: Manifold -> Int
manifoldDimension (Manifold dimension _) =
  dimension
  
makeTangentVector' :: Manifold -> VectorField -> Point -> Tensor
makeTangentVector' manifold (VectorField fields)  point =
  -- f(x,y) = 2x + y
  -- f(2,1) = 5
  let n = evaluate (head fields) point in
    Vector [n, n]

-- f(x, y) = x + y
evaluate :: MathExpr -> Point -> Float
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
      
subsetsMap :: Manifold -> Manifold -> Map.Map String Manifold ->  Map.Map String Manifold
subsetsMap manifold manifoldSubset subsetsMap =
  let name = getManifoldName manifold
  in
    Map.insert name manifoldSubset subsetsMap
  


transitionMap' :: Chart' -> Chart' -> [MathExpr] -> Coordinates
transitionMap' chart chart' mathexprs =
  let coordinates = getCoordinates chart
      coordinates' = getCoordinates chart' in
    map (\(x,y) -> Eq x y) $ zip   coordinates' mathexprs


jacobianMatrix' :: Chart' -> Tensor
jacobianMatrix' chart  =
  let coordinates = getCoordinates chart
      point = getPoint chart
      jacobian = diff' coordinates point in
    Matrix jacobian

diff' :: Coordinates -> Point ->  [[Float]]
diff' [] _ = []
diff' (x:xs) point =
  [dualPart (interp'' x point "x"), dualPart (interp'' x point "y")] :  diff' xs point

frame' :: Chart' -> [String]
frame' (Chart' manifold [] point) = []
frame' (Chart' manifold (Var x:xs) point) =
  ["partial-" ++ x] ++ frame' (Chart' manifold xs point)

interp' :: ScalarField -> Point -> Variable -> Dual Float
interp' (ScalarField manifold chart mathexpr) point var =
  interp'' mathexpr point var
  
interp'' :: MathExpr -> Point -> Variable -> Dual Float
interp'' (Num n) point var =
  (Dual n 0)

interp'' (Var v) (x:xs) var'' =
  if v == var''
  then
    let d1 = (Dual x 1) * Dual 1 0
        in
      d1
   else
    Dual 1 0
    
interp'' (Plus e e2) (x:xs) var'' =
  interp'' e (x:xs) var'' + interp'' e2 (x:xs) var''

interp'' (Mul e e2) (x:xs) var'' =
  interp'' e [x] var'' * interp'' e2 xs var''
           
interp'' (Sin (Var v)) (x:xs) var'' =
  if v == var''
  then
    Dual (sin x) (cos x)
  else
    sin' (Dual x 0)

interp'' (Sin (Num n)) (x:xs) var'' =
  sin' (Dual n 0)

interp'' (Sin e) (x:xs) var'' =
  -- f(x) = sin(2 + x)
  let e' = interp'' e (x:xs) var'' in
    sin' e'

interp'' (Cos (Var v)) (x:xs) var'' =
  if v == var''
  then
    Dual (cos x) (negate (sin x))
  else
    cos' (Dual x 0)

interp'' (Cos (Num n)) (x:xs) var'' =
  cos' (Dual n 0)

interp'' (Cos e) (x:xs) var'' =
  -- f(x) = sin(2 + x)
  let e' = interp'' e (x:xs) var'' in
    cos' e'


interp'' (Tan (Var v)) (x:xs) var'' =
  if v == var''
  then
    Dual (tan x) ((1 / (cos x)) ** 2)
  else
    tan' (Dual x 0)
    
interp'' (Tan e) (x:xs) var'' =
  let e' = interp'' e (x:xs) var'' in
    tan' e'
    
interp'' (Asin (Num n)) (x:xs) var'' =
  asin' (Dual n 0)

interp'' (Asin (Var v)) (x:xs) var'' =
  if v == var''
  then
    Dual (asin x) (acos x)
  else
    asin' (Dual x 0)
  
interp'' (Asin e) (x:xs) var'' =
  -- f(x) = sin(2 + x)
  let e' = interp'' e (x:xs) var'' in
    asin' e'

interp'' (Acos (Num n)) (x:xs) var'' =
  acos' (Dual n 0)

interp'' (Acos (Var v)) (x:xs) var'' =
  if v == var''
  then
    Dual (acos x) (negate (asin x))
  else
    acos' (Dual x 0)

interp'' (Atan (Num n)) (x:xs) var'' =
  atan' (Dual n 0)

interp'' (Atan (Var v)) (x:xs) var'' =
  if v == var''
  then
    (Dual (atan x) ((acos (1 / x)) ** 2))
   else
    atan' (Dual x 0)
  

