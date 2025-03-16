module SmoothFunction where

import AutoDiff (Dual(Dual), sin', cos', tan', asin', acos', atan')

import Manifold(
  Point,
  ScalarField(ScalarField),
  VectorField(VectorField),
  getPoint,
  getChart,
  getCoords,
  Coordinates,
  MathExpr(Plus, Var, Num, Mul, Sin, Cos, Tan, Asin, Acos, Atan),
  primalPart,
  dualPart,
  interp',
  Tensor(...)
  )
  

class SmoothFunction a where
  eval :: a -> Point -> Float
  differential :: a ->  Point -> Float
  gradient :: a -> Tensor -> [Float]
  lieDiff :: a -> VectorField -> Float
  -- laplacian: this involves second order partial derivatives.
  -- you need hyperduals.
  
instance SmoothFunction ScalarField where
  eval field point = eval' field point 
  differential field point = directionalDiff' field point
  gradient field metric = gradient' field metric
  lieDiff scalarField vectorField = lieDiff' scalarField vectorField
  

eval' :: ScalarField -> Point -> Float
eval' field point =
  let dual = interp' field point "x" in
    primalPart dual
    
gradient' :: ScalarField -> Tensor -> [Float]
gradient'  (ScalarField _ chart expr) metric =
  let point = getPoint chart
      coords = getCoords chart
      gradient = gradient'' expr point coords
      in
    gradient

gradient'' :: MathExpr -> Point -> Coordinates -> [Float]
gradient'' exp (x:xs) [] = []
gradient'' exp (x:xs) (y:ys) =
  -- generialize to pseudo riemanr metric
  -- also need an evaluator from MathExpr -> MathExpr
  let dual = interp'' exp (x:xs) y
  in
    dualPart dual : gradient'' exp (x:xs) ys
    
lieDiff' :: ScalarField -> VectorField -> Float
lieDiff' (ScalarField _ chart expr) (VectorField field) =
  let point = getPoint chart in
    lieDiff'' expr field point

lieDiff'' :: MathExpr -> [MathExpr] -> Point -> Float
lieDiff'' expr field point =
  --  f(x,y) = x * sin(y)
  -- vectorfield = v = y ∂/∂x + x ∂/∂y
  
  let diffx = dualPart (interp'' expr point "x")
      
      diffy = dualPart (interp'' expr point "y")
      t1 = (head point) * diffy
      t2 = (head (tail point)) * diffx in
    t1 + t2

directionalDiff' :: ScalarField -> Point -> Float
directionalDiff' (ScalarField manifold chart expr) point =
  -- you need generalize this to any higher dimension
  -- a vector field maps a vector to each point on a manifold.
  -- This is done by taking the directional derivative.
  let n = interp'' expr point "x"
      n' = interp'' expr point "y"
      diffx = dualPart n
      diffy = dualPart n'
      e1 = head point
      e2 = head (tail point) in
    (e1 * diffx) + (e2 * diffy)

getY :: Point -> Float
getY point =
  head point
  
getX :: Point -> Float
getX point =
  head (tail point)
  
