module HAD.Y2014.M03.D19.Solution where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (group, maximumBy)

-- $setup
-- >>> import Control.Applicative ((<*>))
-- >>> import Data.List (isInfixOf)
-- >>> import Test.QuickCheck

-- | mostRepeatedElem
-- Returns the element with the longest (consecutive) repetition and the
-- repetition number
-- If there are tie, the last most repeated element is returned
-- It returns error on empty string
-- 
-- Examples:
--
-- >>> mostRepeatedElem "hello world!"
-- ('l',2)
--
-- >>> mostRepeatedElem [1,1,2,2]
-- (2,2)
--
-- prop> (flip isInfixOf <*> uncurry (flip replicate) . mostRepeatedElem) . getNonEmpty

mostRepeatedElem :: Eq a => [a] -> (a,Int)
mostRepeatedElem = maximumBy (compare `on` snd) . map (head &&& length) . group
