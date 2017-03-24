module Y2017.M03.D24.Exercise where

{--
Okay, we're going to try some probability problems where the probabilities,
themselves are uncertain, unknown, and not material to the solution, anyway.

We're going to be dealing with 'kinda'-numbers.

50 Challening Problems in Probability, Frederick Mosteller

...

Problem 2: Successive Wins

To encourage Elmer's promising tennis career, his father offers him a prize if
he wins (at least) two tennis sets in a row in a three-set series to be played
with his father and the club champion alternatively: father-champion-father or
champion-father-champion, according to Elmer's choice. The champion is a better
player than Elmer's father. Which series should Elmer choose.

...

So, how do we go about this? Well, Elmer has to win two in a row to be rewarded,
so if we have a probability of the winning outcomes for Elmer, then:

P(LLL) has no payout
P(LLW) same
etc

What is the winning outcomes diagrams that payout?
--}

data Outcome = Win | Lose deriving (Eq, Ord, Show)

payouts :: [[Outcome]]
payouts = undefined

-- How would you write such a function (payouts) that returns the winning/losing
-- outcomes that pays Elmer?

-- Now that we have outcomes that pay, we need the two options for Elmer's
-- opponents in the tennis sets which are:

data Opponent = Father | ClubChamp deriving (Eq, Ord, Show)

series :: [[Opponent]]
series = undefined

-- what are the two series that are Elmer's options?

-- Now we have the outcomes that payout and the series-options, now we need
-- to pose probabilities for Elmer to win against his respective opponents.

-- What is the winning strategy for Elmer? That is, what is the strategy that
-- gives him the highest probability of getting a payout on the outcome of his
-- match series?

type Probability = Rational

analyze :: [Opponent] -> [Outcome] -> Probability
analyze series result = undefined

-- that's for one possible series with one possible outcome-set, so the strategy
-- chooses the best. What is the best approach for Elmer?

strategy :: [[Opponent]] -> [[Outcome]] -> ([Opponent], Probability)
strategy series outcomes = undefined
