```
[2 of 2] Compiling Manifold         ( Manifold.hs, interpreted )

Manifold.hs:262:34: error:
    • Couldn't match expected type ‘MathExpr’ with actual type ‘Float’
    • In the first argument of ‘interp''’, namely ‘(cf cfs b d a)’
      In the first argument of ‘dualPart’, namely
        ‘(interp'' (cf cfs b d a) [0.0, 0.0, 0.0] (xv coords c))’
      In the expression:
        dualPart (interp'' (cf cfs b d a) [0.0, 0.0, 0.0] (xv coords c))
    |
262 |     let d1 = dualPart (interp'' (cf cfs b d a) [0.0,0.0,0.0] (xv coords c))
    |                                  ^^^^^^^^^^^^

Manifold.hs:263:34: error:
    • Couldn't match expected type ‘MathExpr’ with actual type ‘Float’
    • In the first argument of ‘interp''’, namely ‘(cf cfs b c a)’
      In the first argument of ‘dualPart’, namely
        ‘(interp'' (cf cfs b c a) [0.0, 0.0, 0.0] (xv coords d))’
      In the expression:
        dualPart (interp'' (cf cfs b c a) [0.0, 0.0, 0.0] (xv coords d))
    |
263 |         d2 = dualPart (interp'' (cf cfs b c a) [0.0,0.0,0.0] (xv coords d))
    |                                  ^^^^^^^^^^^^
Failed, one module loaded.
ghci> 
```
