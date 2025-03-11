module SmoothFunction where

import AutoDiff (Dual(Dual), sin', cos', tan', asin', acos', atan')

import Manifold(
  Point,
  ScalarField(ScalarField),
  VectorField(VectorField),
  getPoint,
  getChart,
  MathExpr(Plus, Var, Num, Mul, Sin, Cos, Tan, Asin, Acos, Atan),
  primalPart,
  dualPart
  )

type Variable = String

class SmoothFunction a where
  eval :: a -> Point -> Float
  differential :: a -> Variable -> Float
  --gradient :: a -> Metric -> VectorField
  lieDiff :: a -> VectorField -> Float

instance SmoothFunction ScalarField where
  eval field point = eval' field point 
  differential field var = differential' field var
  --gradient field metric = gradient' field metric
  lieDiff scalarField vectorField = lieDiff' scalarField vectorField

eval' :: ScalarField -> Point -> Float
eval' field point =
  let dual = interp' field point "x" in
    primalPart dual

differential' :: ScalarField -> Variable -> Float
differential' field var =
  let point = getPoint (getChart field)
      dual = interp' field point var in
    dualPart dual

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
    let sindiff = interp'' (Sin (Var "x")) (x:xs) var''
        cosdiff = interp'' (Cos (Var "x")) (x:xs) var''
        f' = dualPart sindiff
        f = primalPart sindiff
        g' = dualPart cosdiff
        g = primalPart cosdiff
        tandiff = ((g * f') - (f * g')) / g ** 2 in
      Dual (asin x) tandiff
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
     let sindiff = interp'' (Sin (Var "x")) (x:xs) var''
         cosdiff = interp'' (Cos (Var "x")) (x:xs) var''
         f' = dualPart sindiff
         f = primalPart sindiff
         g' = dualPart cosdiff
         g = primalPart cosdiff
         atandiff = ((g' * f) - (g * f')) / f ** 2  in
       (Dual (atan x) atandiff)
   else
    atan' (Dual x 0)
  


