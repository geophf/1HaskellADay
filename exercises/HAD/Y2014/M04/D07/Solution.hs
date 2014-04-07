module HAD.Y2014.M04.D07.Solution where

{- | braid
   Braid two lists

   Examples:

   >>> braid [0,2] [1,3]
   [0,1,2,3]

   >>> braid [0,2] [1,3 ..] 
   [0,1,2,3]

   >>> braid [0,2 ..] [1,3]
   [0,1,2,3]
-}
braid :: [a] -> [a] -> [a]
braid = (concat .) . zipWith (flip (.) return . (:))

braid' :: [a] -> [a] -> [a]
braid' = let
  pairElem x y = [x,y]
  in (concat .) . zipWith pairElem
