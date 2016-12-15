module Y2016.M12.D15.Exercise where

import Data.Map (Map)

-- below import available from 1HaskellADay git repository

import Y2016.M12.D13.Exercise

{--
Back to the Census bureau data.

The SAIPE, Small Area Income and Poverty Estimates, is a breakdown of the 
numbers of people in poverty in the USA by Country, State, and county.

Okay. That's not an insignificant amount of data. I've gzipped the CSV into this
directory.

So, TODAY's task, is simply to read in these data then to organize these county
data rows by USState
--}

type County = String

data SAIPERow = SomeStructureYouDefine
type SAIPEData = Map USState (Map County SAIPERow)

readSAIPEData :: FilePath -> IO SAIPEData
readSAIPEData zipFile = undefined

-- Question: Which USState has the most counties? Which USState, the least?
-- What are these Counties?

countiesFor :: SAIPEData -> USState -> [County]
countiesFor saipe state = undefined

-- How would you define functions to answer the above questions?

-- Simpsons question: How many USStates have Springfield as a county?

-- What is the expression that will get you the Simpsons answer?

