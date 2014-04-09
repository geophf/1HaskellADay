module HAD.Y2014.M04.D08.Solution where

import Data.Monoid

{- | replicateF
   replicate and chain an endofunction

   Examples:

   prop> x + 10 == replicateF 10 (+1) x

   prop> 10 * x == replicateF 10 (+x) 0

   prop> replicate 10 x == replicateF 10 (x:) []
-}
replicateF :: Int -> (a -> a) -> a -> a
replicateF = ((appEndo . mconcat . map Endo) .) . replicate
