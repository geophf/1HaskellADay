module Y2017.M03.D29.Exercise where

-- below import available via 1HaskellADay git repository

import Data.Probability

{--
What, with March Madness, and all ...

From Fifty Challenging Problems in Probability by Frederick Mosteller

Problem 16: March Madness for ... TENNIS!

Will Second-Best Be Runner Up?

A tennis tournament has 8 players. The number a player draws from a hat decides
his first-round run in the tournament ladder. See diagram.

Diagram:

1 -\
    >--- a -\
2 -/         \
              >--- α -\
3 -\         /         \
    >--- b -/           \
4 -/                     \
                          >--- WINNER!
5 -\                     /
    >--- c -\           /
6 -/         \         /
              >--- β -/
7 -\         /
    >--- d -/
8 -/

(I'm very pleased with my ASCII-artesque diagramming skills, by the way)

Suppose that the best player always defeats the next best and that the latter
always defeats all the rest. The loser of the finals gets the runner-up cup.
What is the chance that the second-best player wins the runner-up cup?
--}

draw :: Prob Int
draw = undefined

-- The function draw gives the probability distribution of a player drawing
-- a number for the first-round match.

playerTwoRunnerUp :: Rational
playerTwoRunnerUp = undefined

-- What is the probability of Player 2 making it to the semi-finals ... to go
-- down in the flames of defeat, yes, but still claim that paltry second-place
-- prize to feed his dear family? BABY NEEDS A NEW PAIR OF SHOES!
