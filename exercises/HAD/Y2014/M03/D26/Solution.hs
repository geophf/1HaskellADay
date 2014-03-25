module HAD.Y2014.M03.D26.Solution
  ( Board
  , board
  , getList
  , Direction (..)
  , viewFrom
  ) where

import Data.List (groupBy, transpose)
import Control.Applicative ((<*>))
import Control.Monad (liftM, replicateM)

import Test.QuickCheck

-- Preamble

-- $setup
-- >>> import Control.Applicative ((<$>), (<*>))
-- >>> import Data.List (sort)
-- >>> :{
--   let checkReverse d1 d2 =
--     (==) <$>
--        sort . map sort . getList . viewFrom d1 <*>
--        sort . map (sort . reverse) . getList . viewFrom d2 
-- :}

newtype Board a = Board {getList :: [[a]]}
  deriving (Eq, Show)

data Direction = North | South | East | West
  deriving (Eq, Read, Show)

-- Exercise

-- | viewFrom given a direction, produce an involution such that the
-- inner lists elements are ordered as if they were seen from that direction.
-- 
--
-- Examples: 
--
-- Defaut view is from West
-- prop> xs == viewFrom West xs
--
-- The function is an involution
-- prop> \(d,xxs) -> (==) <*> (viewFrom d . viewFrom d) $ (xxs :: Board Int)
--
-- Ordering properties from opposite side views (for inner lists elements
-- prop> checkReverse West  East  (xxs :: Board Int)
-- prop> checkReverse East  West  (xxs :: Board Int)
-- prop> checkReverse North South (xxs :: Board Int)
-- prop> checkReverse South North (xxs :: Board Int)
--
viewFrom :: Direction -> Board a -> Board a
viewFrom d = let
  go West  = id
  go East  = reverse . map reverse
  go North = transpose
  go South = reverse . map reverse . transpose
  in Board . go d . getList


-- Constructor

-- | board Yesterday's squareOf, build a square board with initial values
board :: Int -> a -> [a] -> [[a]] 
board n x = take n . map (take n) . iterate (drop n) . (++ repeat x)

-- Arbitrary instances

instance Arbitrary a => Arbitrary (Board a) where 
  arbitrary = liftM Board (arbitrary >>= replicateM <*> vector)

instance Arbitrary Direction where
  arbitrary = elements [North, South , East , West]
