module Y2017.M12.D14.Exercise where

{--
The problem with yesterday's problem/solution isn't so much a 'problem,' per se,
as it is a sense of dissatisfaction on my part. I mean, okay, there are 66
composable dates in 2017, but weekends, too? Who works on the weekend? I mean,
besides me and the other poor schmucks in startups. Who solves haskell problems
during the weekends? I mean, besides those poor schmucks addicted to solving
Haskell problems on the weekends ... yours truly, again.

So, today's Haskell problem. Find the composable dates, as before, but not the
weekends, because some people actually do have lives, which means, apparently:
vacuously party hard with their 'friends.'

But I'm not bitter. Really.
--}

import Data.Time

-- below import available via 1HaskellADay git repository

import Y2017.M12.D13.Exercise (allComposeableDatesIn)

vacuous :: Day -> Bool
vacuous weekend = undefined

-- vacuous returns true if it's a weekend, false if it's a weekday, because
-- people working for a big corporation during the week is 'not vacuous' ...
-- for some reason.

-- With the definition of vacuous, above, answer the following question:

-- How many composable dates are there that are not weekends?

allComposeableDatesNotWeekendsIn :: Integer -> [Day]
allComposeableDatesNotWeekendsIn year = undefined

-- What is the diff? That is to say? How many composable dates are there that
-- are weekends?
