module Y2020.M07.D08.Solution where

{--
Okay, we have a context from history (yesterday's exercise). Now, today, let's
do a simple pairing, free of the context (free) of the context-free context.

Whatevs.

Today's exercise, we're going to break up our Group of devs into teams of Pairs
or, if we have an odd number of developers, a ... okay, I didn't invent this
term, and I "don't" think it's stupid: a Mob of three people.

That's a Triple, but what the hell do I know?

Nothing, apparently.

AHEM!

Where were we?

Yes, 'pairing' and ... 'mobbing.' [ick.]
--}

import Data.List (delete)

data Member = Len | Howie | Ray | Nicole | Morgan | Jose
           | Victor | Tony | Apoorv | Ken | Roxi
   deriving (Eq, Ord, Show, Enum)

data Team = Pair Member Member
          | Mob [Member]
   deriving (Eq, Show)

-- no team is 'better' than another, so Ord does not apply

group :: [Member]
group = [Len .. Roxi]

teams :: [Member] -> [[Team]]
teams [] = [[]]
teams us = choose us >>= \(a, rest) ->
           choose rest >>= \(b, rest') ->
           finishUp a b rest' >>= \(team, rest'') ->
           teams rest'' >>= \yeah ->
           return (team:yeah)

finishUp :: Member -> Member ->[Member] -> [(Team, [Member])]
finishUp a b [] = return (Pair a b, [])
finishUp a b [c] = return (Mob [a,b,c], [])
finishUp a b lst@(h:t) = return (Pair a b, lst)

choose :: Eq a => [a] -> [(a, [a])]
choose [] = []
choose lst@(h:t) = lst >>= \x -> return (x, delete x lst)
