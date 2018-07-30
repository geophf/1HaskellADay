module Y2018.M07.D30.Solution where

import Data.Set (Set)
import qualified Data.Set as Set

-- We have two databases

pilot, entities, exDir :: FilePath
exDir = "Y2018/M07/D30/"
pilot = "pilot_db.csv"
entities = "entities_db.csv"

-- We want to merge those databases into one SUPER-database

-- Read in the tables from each database (mind the header information) and do
-- a set difference. What are the names of the tables that both databases share?
-- How many tables are common between both databases?

type Table = String
type Database = Set Table

readDatabase :: FilePath -> IO Database
readDatabase = fmap (Set.fromList . drop 2 . lines) . readFile

{--
>>> p <- readDatabase (exDir ++ pilot)
>>> length p
29
>>> take 3 (Set.toList p)
["public.action_lk","public.active_lk","public.article"]

>>> e <- readDatabase (exDir ++ entities)
>>> length e
22
>>> take 3 (Set.toList e)
["public.action_lk","public.active_lk","public.article_entity"]
--}

sharedTables :: Database -> Database -> Set Table
sharedTables = Set.intersection

{--
>>> sharedTables p e
fromList ["public.action_lk","public.active_lk","public.audit","public.log","public.severity_lk"]
--}
