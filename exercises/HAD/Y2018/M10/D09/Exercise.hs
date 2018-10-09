module Y2018.M10.D09.Exercise where

{--
Yesterday we picked some ids from our 'database' of ids for screening. Today,
we are given a set of ids to add to our database or to update information. So,
we need to determine from the below list, what are new ids and what are pre-
existing ones, given our database from yesterday.
--}

import Data.Map (Map)
import Y2018.M10.D08.Exercise (ID, exDir, idsToday)

ex1Dir, newIds :: FilePath
ex1Dir = "Y2018/M10/D09/"
newIds = "updates.txt"

data Status = NEW | UPDATE

queryStatus :: FilePath -> FilePath -> IO (Map ID Status)
queryStatus idDB updates = undefined

{--
Given the 'database' of ids, and the set of new ids that came in to be added
or updated, determine which ids are new and which ids are already in the
database.
--}
