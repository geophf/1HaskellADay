module Y2020.M07.D13.Exercise where

{--
So, 'yesterday' we paired our devs together. The day before, we created
a simple historical context, keyed by days.

Today, after we settle on a pairing, let's store that decision (those pairings)
into our historical context.

Why?

Because we don't want today's pairing to be the same as the previous x days
of pairings, that is to say: we want people to pair with different people
each day.

But that's not today's problem (unique, or fresh, pairings), today's problem
is to store today's existing pairings.

Let's do that.
--}

import Data.Time

import Y2020.M07.D08.Exercise   -- for pairs and mobs and people and such
-- import Y2020.M07.D07.Exercise   -- we don't want this now.

import Data.Map (Map)
import qualified Data.Map as Map

-- why don't we want to import D07? Because we're rebuilding our History-type

type Pairings = (Day, [Team])
type History = Map Day [Team]

addPairings :: Pairings -> History -> History
addPairings (date, teams) hist = undefined

history :: Int -> History -> History
history days hist = undefined

-- history gives you a slice of your history: only n days of history.

-- why?

-- because if you bring in all history, then everyone will have paired with
-- everyone, eventually. You only want to go back (about) team-size / 2 days.

-- Okay, do it to it! 
