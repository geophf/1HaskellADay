module HAD.Y2014.M03.D19.Exercise where

-- $setup
-- >>> import Control.Applicative ((<*>))
-- >>> import Data.List (isInfixOf)
-- >>> import Test.QuickCheck

-- Level: Easy
-- Pointfree: yes


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
mostRepeatedElem = undefined
