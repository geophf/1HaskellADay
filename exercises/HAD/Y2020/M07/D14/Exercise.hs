module Y2020.M07.D14.Exercise where

{--
Today, we'll look at data transformation with an eye toward building
information that we can use in decision-making. This we call decisionable
or actionable data.

From yesterday's exercise, we have a slice of our history going back n days,
and that history is given to us as sets of pairings (which is a mapping from
a Day to a list of teams).

Today we wish to filter down that data to just (or 'all') the pairs from the
recent history so we can ask questions like: "Who has x paired with?" and
"Can I pair X with Y?"
--}

import Data.Time

import Y2020.M07.D08.Exercise
import Y2020.M07.D13.Exercise

pairs :: History -> [Team]
pairs hist = undefined

-- given the recent history (obtained from history function from yesterday),
-- pairs returns the aggregate of teams in that history

paired :: Member -> [Team] -> [Member]
paired x teams = undefined

-- who has x teamed with? The paired function answers this question.

available :: Member -> [Team] -> [Member]
available x teams = undefined

-- With whom, given the pairing context, can we pair x?

-- LET'S ROOOOOOOOCK!
