module Y2021.M03.D04.Solution where

{--
Today's haske...

... BUT WHAT IS THIS? NO OVERLOADEDSTRINGS IN THE PROCESSING INSTRUCTIONS???

... *ahem* yes. We're moving on to a set of different, unitary (that is to say,
non-episodic) Haskell problems.

By the way: Did Haskell B. Curry have a nickname? Like, King George VI's
nickname was 'Bertie.' Did Dr. Curry go by 'Haskie' to his friends? Did he
have friends? Do you ever wonder that? Ever? Or do you just use the language
and glory in its awesomeness, never thinking of poor Haskie, pouring over his
Maths textbooks, sad, and lonely.

Well, now you do.

You're welcome.

OKAY! *Whew* I'm glad that didn't escalate!

Anyway.

Let's say you have a time-series of values.
--}

import Control.Arrow (first)

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ord

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time

type TimeSeries a = Map Day a

-- What is the most recent date? (and, subsequently, value)

latest :: TimeSeries a -> Maybe Day
latest ts = fst <$> Set.maxView (Map.keysSet ts)

{--
>>> latest sampTS 
Just 2021-03-04
--}

-- Given a Day, d, what is the previous entry to that day? ... and all
-- preceding entries to that 'yesterday'?

yesterday :: TimeSeries a -> Day -> Maybe (Day, Set Day)
yesterday ts d = yest' d (Map.keysSet ts)

yest' :: Day -> Set Day -> Maybe (Day, Set Day)
yest' d ts = Set.maxView ts >>= y' d 

y' :: Day -> (Day, Set Day) -> Maybe (Day, Set Day)
y' tday ans@(yday, days) | yday < tday = return ans
                         | Set.size days == 0 = Nothing
                         | otherwise = yest' tday days

-- I mean: how do you even get what day it is, today, in UTC-time?

{--
>>> latest sampTS >>= yesterday sampTS 
Just (2021-03-02,fromList [2021-02-26,2021-03-01])
--}

today :: IO Day
today = utctDay <$> getCurrentTime

{--
>>> today
2021-03-04
--}

-- Using the below TimeSeries, answer the above questions

sampTS :: TimeSeries String
sampTS = Map.fromList $ map (first read) [("2021-03-04", "foo"),
                                          ("2021-03-02", "bar"),
                                          ("2021-03-01", "baz"),
                                          ("2021-02-26", "quux")]

-- Finally, given a time-series: return the key/value-pairs in sorted, 
-- descending order.

snort :: TimeSeries a -> [(Day, a)]
snort = sortOn (Down . fst) . Map.toList

{--
>>> snort sampTS 
[(2021-03-04,"foo"),(2021-03-02,"bar"),(2021-03-01,"baz"),(2021-02-26,"quux")]

Adding these functions to Date.Time.TimeSeries
--}
