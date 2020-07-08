module Y2020.M07.D08.Exercise where

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

data Member = Len | Howie | Ray | Nicole | Morgan | Jose
           | Victor | Tony | Apoorv | Ken | Roxi
   deriving (Eq, Ord, Show, Enum)

data Team = Pair Member Member
          | Mob [Member]
   deriving (Eq, Show)

-- no team is 'better' than another, so Ord does not apply

group :: [Member]
group = undefined

teams :: [Member] -> [Team]
teams us = undefined

{--
So we should get:

>>> length $ teams group
... goes on forever, because there are 39916800 results, so:

>>> head $ teams group
[Pair Len Howie,
 Pair Ray Nicole,
 Pair Morgan Jose,
 Pair Victor Tony,
 Mob [Apoorv,Ken,Roxi]]
--}
