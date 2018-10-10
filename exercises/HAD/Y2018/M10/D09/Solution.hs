module Y2018.M10.D09.Solution where

{--
Yesterday we picked some ids from our 'database' of ids for screening. Today,
we are given a set of ids to add to our database or to update information. So,
we need to determine from the below list, what are new ids and what are pre-
existing ones, given our database from yesterday.
--}

import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Logic.Frege ((-|))

import Y2018.M10.D08.Exercise (ID, exDir, idsToday)

ex1Dir, newIds :: FilePath
ex1Dir = "Y2018/M10/D09/"
newIds = "updates.txt"

data Status = NEW | UPDATE
   deriving (Eq, Show)

instance Monoid Status where
   mempty = NEW
   mappend = undefined

-- so Status is the dual of a Semigroupoid

queryStatus :: FilePath -> FilePath -> IO (Map ID Status)
queryStatus idDB updates =

-- 1. read in the existing set of ids as a set

   Set.fromList . lines <$> readFile idDB >>= \db ->

-- 2. fetch the updates as a list

   lines <$> readFile updates >>=

-- 3. map the set-membership over the updates

   return . Map.fromList . map (id &&& isInSet db) 
   
-- and a little Frege logic (with monoids!) to determine status

isInSet :: Set ID -> ID -> Status
isInSet set elt = elt `Set.member` set -| UPDATE

{--
Given the 'database' of ids, and the set of new ids that came in to be added
or updated, determine which ids are new and which ids are already in the
database.

>>> queryStatus (exDir ++ idsToday) (ex1Dir ++ newIds) >>= 
    mapM_ print . Map.toList 

("3Xab5gFdGx8gaLHB6WBtMV35",UPDATE)
("4dbqvyh2Gp2dEzeJZFWMCkjQ",NEW)
("6rZmXn446MF4KCQLDj3FM2Ej",NEW)
("8shPDEDWpHxCtUTCt7FFPJjE",UPDATE)
("AYZ48cFAK4Qt7DpuWdTGgWEC",NEW)
("BdEBbMTz3MmbZrbGzfRGSAw7",NEW)
("EGSXADx2BMyMq5kmJ6CZpewC",UPDATE)
("GnEGreLZGCq4UQbx7atVwwwJ",UPDATE)
("SxKZcfb5GbkdW34J35GpLtLw",UPDATE)
("UVWUUuvRQJ4FMLCv2Rgv8hTC",NEW)
("Yn4Ug6kpptyfBuxLkW45PcEf",UPDATE)
("ZbxZWHpr649MSTxJqC2sAj4Q",UPDATE)
("bVpTY2EKCshGbXhRvzY8aeD3",NEW)
("dGHmka7qxBnf4SnJhXuxprpg",NEW)
("n2pwDg2tC7PB99fVsggSQdpF",NEW)
("vFcTuZES3tcBGEyuusSCC868",NEW)
("xNfPsTB9MUjEAvdz6RRj9dKt",NEW)
--}
