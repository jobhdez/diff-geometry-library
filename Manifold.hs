module Manifold where

import qualified Data.Map as Map
import AutoDiff (Dual(Dual), sin', cos', tan', asin', acos', atan', pow')

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
  | Tensor [Tensor]
  deriving Show
  
type Dimension = Int
type Name = String

data Manifold =
  Manifold Dimension Name Chart'
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
  Chart' Coordinates Point
  deriving Show

getPoint :: Chart' -> [Float]
getPoint (Chart' coord point) =
  point
  
getCoordinates :: Chart' -> [MathExpr]
getCoordinates (Chart' coord point) =
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
  | Minus MathExpr MathExpr
  | Mul MathExpr MathExpr
  | Division MathExpr MathExpr
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
  differential :: a -> a -> [MathExpr] -> Point -> Tensor
  christoffelSymbols :: a -> [[MathExpr]] -> Coordinates ->  [[[MathExpr]]]
  riemannTensor :: a -> [[MathExpr]] -> [[[[MathExpr]]]]
  ricciTensor :: a -> [[[[MathExpr]]]] -> [[MathExpr]]
  ricciScalar :: a -> [[MathExpr]] -> [[MathExpr]] -> MathExpr
  
{--
class TangentSpace a where
  tangetVectors :: [a] -> Point -> ([a], Point)

instance TangentSpace TangentVectors where
  tangentVectors vectors point = tangentVectors' vectors point
--}
{--
class SmoothMap a where
  differential :: a -> a -> MathExpr -> Point -> Tensor
  --jacobianMatrix :: a -> Chart -> Chart -> Matrix
  --pullBack :: a -> Tensor -> Tensor
  --pushForward :: a -> Tensor -> Tensor
--}
{--
instance SmoothMap Manifold where
  differential m1 m2 expr p1 = differential'' m1 m2 expr p1
--}

class Chart a where
  frame :: a -> [String]
  --restrict :: a -> Manifold -> a
  transitionMap :: a -> a -> [MathExpr] -> Coordinates
  function :: a -> Point -> MathExpr ->  Float
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
  function chart point mathexpr = function' chart point mathexpr
  
instance TopologicalManifold Manifold where
  scalarField manifold chart fn = topologicalField manifold chart fn
  openSubset manifold manifold' = topologicalOpenSubset manifold manifold'
  --chart manifold coords map' = topologicalChart manifold coords map'

instance SmoothManifold Manifold where
  tangentVector fields manifold point = makeTangentVector fields manifold point
  differential m1 m2 mathexpr p1 = differential' m1 m2 mathexpr p1
  -- pseudu riemman
  christoffelSymbols m1 metric coords = christoffelSymbols'' m1 metric coords
  riemannTensor m1 metric = riemannTensor' m1 metric
  ricciTensor m1 metric = ricciTensor' m1 metric
  ricciScalar m1 tensor metric = ricciScalar' m1 tensor metric

ricciScalar' :: Manifold -> [[MathExpr]] -> [[MathExpr]] -> MathExpr
ricciScalar' m1 ricciTensor metric =
  ricciScalar'' ricciTensor metric 0 0

ricciScalar'' :: [[MathExpr]] -> [[MathExpr]] -> Int -> Int -> MathExpr
ricciScalar'' tensor metric i j =
  if i  < length tensor
  then
    Plus (Mul (g metric i j) (g tensor i j)) (ricciScalar'' tensor metric (i + 1) (j + 1))
  else
    Num 0
    
ricciTensor' :: Manifold -> [[[[MathExpr]]]] -> [[MathExpr]]
ricciTensor' m1 riemannTensor  =
  ricciTensor'' riemannTensor 0 0 0 

ricciTensor'' :: [[[[MathExpr]]]] -> Int -> Int -> Int -> [[MathExpr]]
ricciTensor'' curvatureTensor 3 _ _ = []
ricciTensor'' curvatureTensor a b c =
  [[curvatureTensor !! a !! b !! c !! a]] ++ ricciTensor'' curvatureTensor (a + 1) (b + 1) (c + 1)
  
riemannTensor' :: Manifold -> [[MathExpr]] -> [[[[MathExpr]]]]
riemannTensor' m1 metric =
  let coords = getCoordinates (getManifoldChart m1)
      christoffelSymbolss = christoffelSymbols'' m1 metric coords
  in
    riemannTensor'' metric christoffelSymbolss coords

riemannTensor'' :: [[MathExpr]] -> [[[MathExpr]]] -> Coordinates -> [[[[MathExpr]]]]
riemannTensor'' metric cfsymbols coords =
  let
    r00 = riemannTensor''' metric cfsymbols coords 0 0 0 0 0
    r01 = riemannTensor''' metric cfsymbols coords 0 0 1 0 0
    r02 = riemannTensor''' metric cfsymbols coords 0 0 2 0 0
    r03 = riemannTensor''' metric cfsymbols coords 0 0 3 0 0
    r10 = riemannTensor''' metric cfsymbols coords 0 1 0 0 0
    r11 = riemannTensor''' metric cfsymbols coords 0 1 1 0 0
    r12 = riemannTensor''' metric cfsymbols coords 0 1 2 0 0
    r13 = riemannTensor''' metric cfsymbols coords 0 1 3 0 0
    r20 = riemannTensor''' metric cfsymbols coords 0 2 0 0 0
    r21 = riemannTensor''' metric cfsymbols coords 0 2 1 0 0
    r22 = riemannTensor''' metric cfsymbols coords 0 2 2 0 0
    r23 = riemannTensor''' metric cfsymbols coords 0 2 3 0 0
    r30 = riemannTensor''' metric cfsymbols coords 0 3 0 0 0
    r31 = riemannTensor''' metric cfsymbols coords 0 3 1 0 0
    r32 = riemannTensor''' metric cfsymbols coords 0 3 2 0 0
    r33 = riemannTensor''' metric cfsymbols coords 0 3 3 0 0
  in
    [[r00, r01, r02, r03]
    ,[r10, r11, r12, r13]
    ,[r20, r21, r22, r23]
    ,[r30, r31, r32, r33]
    ]
riemannTensor''' :: [[MathExpr]] -> [[[MathExpr]]] -> Coordinates -> Int -> Int -> Int -> Int -> Int -> [[MathExpr]]
riemannTensor''' metric cfs coords a b c d r =
  if d < length coords
  then
    let d1 = deriv (cf cfs b d a) (xv coords c)
        d2 = deriv (cf cfs b c a) (xv coords d)
        d3 = Mul (cf cfs a d r) (cf cfs r b c)
        d4 = Mul (cf cfs a c r) (cf cfs r b d)
        in
      [[d1,d2,d3,d4]] ++ riemannTensor''' metric cfs coords a b c (d + 1) (r + 1)
  else
    []

christoffelSymbols'' :: Manifold -> [[MathExpr]] -> Coordinates -> [[[MathExpr]]]
christoffelSymbols'' m1 exps coords =
  let inverse = invertMatrix exps in
    [[christoffelSymbols' m1 exps coords 0 0 0] ++ 
    [christoffelSymbols' m1 exps coords 0 1 0] ++ 
    [christoffelSymbols' m1 exps coords 0 2 0]] ++ 
    [[christoffelSymbols' m1 inverse coords 0 0 0] ++ 
    [christoffelSymbols' m1 inverse coords 0 1 0] ++ 
    [christoffelSymbols' m1 inverse coords 0 2 0]]

{--
christoffelSymbols' :: Manifold -> [[MathExpr]] -> Coordinates -> Int -> Int -> Int -> [Float]
christoffelSymbols' m [] [] a b c = []
christoffelSymbols' manifold exps coords a b c =
  if c < length coords then
    let d1 = dualPart (interp'' (g exps a a) [0.0,0.0,0.0] (xv coords c))
        d2 = dualPart (interp'' (g exps a c) [0.0,0.0,0.0] (xv coords b))
        d3 = dualPart (interp'' (g exps b c) [0.0,0.0,0.0] (xv coords a))
    in
      [1/2 * (d1 + d2 - d3)] ++ christoffelSymbols' manifold exps coords a b (c + 1)
  else
    []
 --}

christoffelSymbols' :: Manifold -> [[MathExpr]] -> Coordinates -> Int -> Int -> Int -> [MathExpr]
christoffelSymbols' m [] [] a b c = []
christoffelSymbols' manifold exps coords a b c =
  if c < length coords then
    let d1 = deriv (g exps a a) (xv coords c)
        d2 = deriv (g exps a c) (xv coords b)
        d3 = deriv (g exps b c) (xv coords a)
    in
      [Mul (Division (Num 1) (Num 2)) (Minus (Plus d1 d2) d3)] ++ christoffelSymbols' manifold exps coords a b (c + 1)
  else
    []
invertMatrix :: [[MathExpr]] -> [[MathExpr]]
invertMatrix [] = []
invertMatrix (x:xs) =
  [invertmatrix' x] ++ invertMatrix xs

invertmatrix' :: [MathExpr] -> [MathExpr]
invertmatrix' [] = []
invertmatrix' (x:xs) =
  case x of
    Num 0 -> [Num 0] ++ invertmatrix' xs
    Num 1 -> [Num 1] ++ invertmatrix' xs
    _ -> [Division (Num 1) x] ++ invertmatrix' xs
    
g :: [[MathExpr]] -> Int -> Int -> MathExpr
g exps row col =
  (exps !! row) !! col

cf :: [[[MathExpr]]] -> Int -> Int -> Int -> MathExpr
cf exps a b c =
  (((exps !! a) !! b) !! c)
  
xv :: Coordinates -> Int -> MathExpr
xv coords row =
  let row' = coords !! row in
    row'

getCoord :: MathExpr -> String
getCoord (Var coord) =
  coord
{--
christoffelSymbolHelper :: [MathExpr] -> Coordinates -> [Float]
christoffelSymbolHelper exp (y:ys) =
  --}
  
differential' :: Manifold -> Manifold -> [MathExpr] -> Point -> Tensor
differential' m1 m2 expr p1 =
  -- a differential of a smooth map is just a map from tangent spaces.
  -- so, given a tangent space to M at point p, a differential is a  map
  -- between this tanget space to another tanget space to N at point f(p)
  let chart = getManifoldChart m1
      coordinates = getCoordinates chart
      diff' = differential'' expr p1  coordinates
      in
    Matrix diff'

differential'' :: [MathExpr] -> Point -> Coordinates -> [[Float]]
differential'' [] p1 coords = []
differential'' (e:es) p1 coords =
 [differential''' e p1 coords] ++ differential'' es p1 coords
 
differential''' :: MathExpr -> Point -> Coordinates -> [Float]
differential''' e p1 [] = []
differential''' e p1 (Var x:xs) =
  [dualPart (interp'' e p1 x)] ++ differential''' e p1 xs
    
getManifoldChart :: Manifold -> Chart'
getManifoldChart (Manifold dim name chart) =
  chart 
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

  Chart' coordinates points

topologicalField :: Manifold -> Chart' -> MathExpr -> ScalarField
topologicalField manifold chart fn =
  ScalarField manifold chart fn

topologicalOpenSubset :: Manifold -> Manifold -> Map.Map String Manifold -> Map.Map String Manifold
topologicalOpenSubset manifold manifold' subsets =
{--
If U is an open subset of a manifold M then
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
getManifoldName (Manifold dim name chart) =
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
manifoldDimension (Manifold dimension _ _) =
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
frame' (Chart' [] point) = []
frame' (Chart' (Var x:xs) point) =
  ["partial-" ++ x] ++ frame' (Chart' xs point)

function' :: Chart' ->  Point -> MathExpr -> Float
function' chart (x:xs) mathexpr =
  let coordinates = getCoordinates chart in
    function'' mathexpr (x:xs) coordinates

function'' :: MathExpr -> Point -> Coordinates -> Float
function'' expr point []  = 0.0
function'' expr (y:ys) (Var x:xs) =
  primalPart (interp'' expr [y]  x) + function'' expr ys xs

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
    Dual 0 0
    
interp'' (Plus e e2) (x:xs) var'' =
  interp'' e (x:xs) var'' + interp'' e2 (x:xs) var''

interp'' (Minus e e2) (x:xs) var'' =
  interp'' e (x:xs) var'' - interp'' e2 (x:xs) var''

interp'' (Mul e e2) (x:xs) var'' =
  let d1 = interp'' e (x:xs) var''
      d2 = interp'' e2 (x:xs) var''
      in
    d1 * d2
    
interp'' (Power e e2) (x:xs) var'' =
  pow' (interp'' e (x:xs) var'') (interp'' e2 (x:xs) var'')
  
interp'' (Division e e2) (x:xs) var'' =
  let d1 = interp'' e (x:xs) var''
      d2 = interp'' e2 (x:xs) var''
      in
    d1 / d2
  
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
  
deriv :: MathExpr -> MathExpr -> MathExpr
deriv (Num n) var = Num 0
deriv (Var v) (Var var) = if v==var then Num 1 else Num 0
deriv (Plus e e2) var = Plus (deriv e var) (deriv e2 var)
deriv (Minus e e2) var = Minus (deriv e var) (deriv e2 var)
deriv (Mul e e2) var =
  Plus (Mul (deriv e var) e2) (Mul e (deriv e2 var))
deriv (Division e e2) var =
  Division (Minus (Mul (deriv e var) e2) (Mul e (deriv e2 var))) (Power e2 (Num 2))

deriv (Power (Var e) (Num n)) var =
  (Mul (Num n) (Power (Var e) (Num (n-1))))
  
deriv (Power (Sin (Var n)) (Num n')) var =
  Sin (Mul (Var n) (Num n'))
  
deriv (Sin e) var =
  let e' = deriv e var in
    Cos e'
