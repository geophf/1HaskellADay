{-# LANGUAGE QuasiQuotes #-}

module Y2017.M12.D11.Exercise where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import System.Random

-- below imports available via 1HaskellADay git repository

import Store.SQL.Util.Indexed

{--
An exponential probability distribution looks like the attached image (see 
Y2017/M12/D11/exponential-histogram.png), and can be computed, thus:

-ln(1 - rndx)

where x is some number, possibly 'randomly' generated (with the quote, or if
you are getting true random numbers, like from random.org, then: without the
quote. Hey, I'm easy!)
--}

-- 1. write the exponential distribution, that, given a number, computes it

negLn :: Double -> Double
negLn x = undefined

-- 2. Generate 10,000 random numbers from the exponential distribution
--    in the Integer range from 5000 - 25000

rndExpRange :: RandomGen g => (Integer, Integer) -> g -> (Integer, g)
rndExpRange rng gen = undefined

{-- BONUS -----------------------------------------------------------------

For each of the ids in the article table of the NYT article archive, generate
a random integer in the range from 5000 - 25000 on the exponential distribution,
and update the view_count column with that value.
--}

fetchIdsStmt :: Query
fetchIdsStmt = [sql|SELECT id from article|]

fetcher :: Connection -> IO [Index]
fetcher conn = undefined

updatesStmt :: Query
updatesStmt = [sql|UPDATE article SET view_count=? WHERE id=?|]

updater :: Connection -> [IxValue Integer] -> IO ()
updater conn artsViews = undefined

-- make sure you do this in bulk, or it make take a while.

-- so, of course, you need a artid -> rndexp -> IxVal mapping function

viewCountsById :: Index -> Integer -> IxValue Integer
viewCountsById artid viewcnt = undefined

-- and there you go!

-- ... do we want to create an application of this? This ... IS supposed to be
-- a one-time thing, so we'll only do this like ten times or so, smh.
