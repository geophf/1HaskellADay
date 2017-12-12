{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Y2017.M12.D11.Solution where

import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import System.Random

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
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
negLn x = -log (1 - x)

-- 2. Generate 10,000 random numbers from the exponential distribution
--    in the Integer range from 5000 - 25000

rndExpRange :: RandomGen g => (Integer, Integer) -> State g Integer
rndExpRange (f,c) =
   get >>= \gen -> 
   let (comp, g) = first negLn (random gen) in
   put g >>
   return ((f +) . round $ fromIntegral (c - f) * comp)

{--
>>> evalState (rndExpRange (5000,25000)) <$> getStdGen 
13628
>>> take 10 . evalState (mapM (const (rndExpRange (5000,25000)))
                              (replicate 10000 ())) <$> getStdGen 
[13628,10506,28190,39568,19095,8851,11727,16811,6949,31534]

YAY!
--}

{-- BONUS -----------------------------------------------------------------

For each of the ids in the article table of the NYT article archive, generate
a random integer in the range from 5000 - 25000 on the exponential distribution,
and update the view_count column with that value.
--}

fetchIdsStmt :: Query
fetchIdsStmt = [sql|SELECT id from article|]

fetcher :: Connection -> IO [Index]
fetcher = flip query_ fetchIdsStmt

-- must do this to avoid long hang-time on mass updates:

updatesStmt :: IxValue Integer -> ByteString
updatesStmt (IxV i vc) =
   B.pack ("UPDATE article SET view_count=" ++ show vc
               ++ " WHERE id=" ++ show i ++ "; ")

updater :: Connection -> [IxValue Integer] -> IO ()
updater conn = void . execute_ conn . Query . B.unlines . map updatesStmt

-- executeMany has a 'feature' around formating multiple ? in a query. So we
-- have this work-around of bulk-update-as-many-updates-in-one-transaction.

-- make sure you do this in bulk, or it make take a while.

-- so, of course, you need a artid -> rndexp -> IxVal mapping function

viewCountsById :: Index -> Integer -> IxValue Integer
viewCountsById (idx -> artid) = IxV artid

-- and there you go!

{--
>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> rnds <- evalState (mapM (const (rndExpRange (5000,25000))) (replicate 10000 ())) <$> getStdGen
>>> ids <- fetcher conn
>>> updater conn (zipWith viewCountsById ids rnds)
--}

-- ... do we want to create an application of this? This ... IS supposed to be
-- a one-time thing, so we'll only do this like ten times or so, smh.
