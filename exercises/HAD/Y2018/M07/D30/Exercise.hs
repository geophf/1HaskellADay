module Y2018.M07.D30.Exercise where

import Data.Set (Set)

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
readDatabase file = undefined

sharedTables :: Database -> Database -> Set Table
sharedTables db1 db2 = undefined
