module HAD.Y2014.M04.D10.Exercise where

{- | onBothSide
   Apply the same argument on both side of a binary numeric function.

   The only interesting solution is point-free.

   It IS the easiest exercise so far, with the shortest solution.

   Examples:

   prop> onBothSide (+) x = x + x

   prop> onBothSide (*) x = x * x

-}
onBothSide :: Num a => (a -> a -> b) -> a -> b
onBothSide = undefined
