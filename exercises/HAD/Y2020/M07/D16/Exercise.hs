module Y2020.M07.D16.Exercise where

{--
And now for something completely different.

You have this dataset in daterz.csv (in this directory)

(remember strings are always bad, and here's why)
(but that's what you're getting, so deal)

Parse daterz.csv, then tell me: which level gives the most r's per we?

Use whatever parsing tools or techniques you prefer.
--}

import Data.Map (Map)
import qualified Data.Map as Map

type Level = String
type WorldEnergy = Int
type Recruits = Int
type KnowledgeBase = Map Level (WorldEnergy, Recruits)

daterzFile :: FilePath
daterzFile = "Y2020/M07/D16/daterz.csv"

dataset :: FilePath -> IO KnowledgeBase
dataset file = undefined

type RWERatio = Int

levelsRanked :: KnowledgeBase -> [(Level, RWERatio)]
levelsRanked kb = undefined

{--
levelsRanked returns the level with the highest R / WE as the first element.
I suppose you could use a heap, instead, to automate ranking, if you wish.

These were my results:

>>> dataset daterzFile >>= return . levelsRanked 
[("s1:8-4",233.33%),("s1:7-2",200.00%),("s1:7-4",200.00%),("s2:5-2N",183.33%),
 ("s2:21-10N",175.00%),("s3:7-10N",166.66%),("s3:7-4N",166.66%),
 ("s2:27=1N",133.33%),("s3:11-2N",128.57%)]

... what do you come up with?
--}
