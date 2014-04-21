module HAD.Y2014.M04.D14.Exercise where

{- | stagedComputation

   Given a list of endofunctions and an initial value, stagedComputation returns
   the list of the staged computation of these functions
   (might be unclear, check the examples)

   Examples:

   >>> stagedComputation [(+1), (*2), subtract 3] 4
   [5,10,7]

   >>> stagedComputation [(++) "el", (:) 'h'] "lo"
   ["ello","hello"]

-}
stagedComputation :: [a->a] -> a -> [a]
stagedComputation = undefined
