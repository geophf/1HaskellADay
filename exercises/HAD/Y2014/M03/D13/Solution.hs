module HAD.Y2014.M03.D13.Solution where

import Control.Applicative

-- | pairToList Trnsform a pair of same type elements in a list of two
-- elements.
-- 
-- Of course, the major challenge is to find a point free function
-- (without lambda)
--
-- prop> replicate 2 (x :: Int) == pairToList (x,x)
--
-- prop> (\(f,s) -> [f,s]) x == pairToList x 
--
pairToList :: (a,a) -> [a]
pairToList = (:) <$> fst <*> ((:[]) . snd)
