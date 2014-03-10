module HAD.Y2014.M03.D10.Solution where

import Data.Maybe (listToMaybe)
import Numeric (readDec)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative

-- | maybeReadPositiveInt Try to parse a positive Int
-- Can be done point-free (and it's probably funnier this way).
--
--
-- Examples:
--
-- prop> (==) <$> Just <*> maybeReadPositiveInt . show $ getNonNegative x
--
-- prop> Nothing == (maybeReadPositiveInt . show . negate . getPositive $ x)
--
-- >>> maybeReadPositiveInt "foo"
-- Nothing
--
-- >>> maybeReadPositiveInt "12 "
-- Nothing

maybeReadPositiveInt :: String -> Maybe Int
maybeReadPositiveInt =
  fmap fst . listToMaybe . filter (null . snd) . readDec
