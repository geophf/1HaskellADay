module HAD.Date
  ( current
  , date
  ) where

import Data.Time

-- Helper that get current Day
current :: IO (Int, Int, Int)
current = do
  (y, m, d) <- fmap (toGregorian . utctDay) getCurrentTime
  return (fromInteger y, m, d)

date :: Int -> Int -> Int -> IO (Int, Int, Int)
date y m d = return (y,m,d)
