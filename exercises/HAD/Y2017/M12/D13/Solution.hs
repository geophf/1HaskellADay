{-# LANGUAGE ViewPatterns #-}

module Y2017.M12.D13.Solution where

{--
A little simple thing for today's Haskell exercise.

Yesterday's date was 2017-12-12. I noticed something after I had already 
published yesterday's SQL problem.

Yesterday's date has the following property

Both the month and the day are composed of digits from the year. That is to 
say that the following holds:
--}

import Data.List (unfoldr)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Calendar

composableDate :: Day -> Bool
composableDate (toGregorian -> (yr, mos, day)) =
   mos `isComposedOfDigitsIn` yr && day `isComposedOfDigitsIn` yr

isComposedOfDigitsIn :: Int -> Integer -> Bool
isComposedOfDigitsIn (show -> x) (Set.fromList . show -> yr) =
   all (`Set.member` yr) x

{--
>>> composableDate (read "2017-12-12")
True
>>> composableDate (read "2017-12-13")
False
--}

-- Great!

-- Now: what are all the dates in 2017 that have the composeableDate property?

allComposeableDatesIn :: Integer -> [Day]
allComposeableDatesIn yr =
   filter composableDate .  validDatesIn yr $ fromGregorian yr 1 1

{--
>>> take 5 $ allComposeableDatesIn 2107
[2017-01-01,2107-01-02,2107-01-07,2107-01-10,2107-01-11]
>>> length (allComposeableDatesIn 2017)
66
--}

validDatesIn :: Integer -> Day -> [Day]
-- validDatesIn yr day = day:unfoldr (nextDay yr) day
validDatesIn yr = (:) <*> unfoldr (nextDay yr)

nextDay :: Integer -> Day -> Maybe (Day, Day)
nextDay yr today =
   let tomorrow = addDays 1 today
       (yr1, _, _) = toGregorian tomorrow in
   if yr1 > yr then Nothing else Just (tomorrow, tomorrow)
-- I'm sure there's some kind of Mayan spin to all this, but you tell me.
