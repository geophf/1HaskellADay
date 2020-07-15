module Y2020.M07.D15.Exercise where

{--
Okay, we've got historical context, we have a way to query that historical
context, we have active members of our group, and we have already paired them,
but it was a simple pairing algorithm, ignoring the context of who was paired
with whom before.

Today, we're going to create a pairing algorithm that will

1. get only the relevant history for pairings today; and,
2. pair our team-members today, given that relevant history.

YEET!
--}

import Data.Time

import Y2020.M07.D08.Exercise
import Y2020.M07.D13.Exercise
import Y2020.M07.D14.Exercise

{--
Side note:

You see how these exercises build on each other? That's how I do development.
So, if you're just joining this conversation, how do you get started?

This is what I would recommend (so I will): look at the imports. For the
exercises imported that you have not completed, do those first. You'll find
exercises imported in those exercises, do those first-first. When you have
all your imports resolved, you've build up your modules to the state where
you can now use them in this exercise. THEN do this exercise.

So, this 1HaskellADay exercise could turn into several exercises to do.

That's a fine learning-experience.

/Side note
--}

depth :: [Member] -> Int
depth activeMembers = undefined

{--
depth

Given the active members (data Member), compute how far back in depth you want
to go in the history so you don't have pairings now that become impossible.

How do you know that?

Eh. floor (length of all active members / 2) should be a pretty good eye-ball.
--}

pairings :: Int -> History -> [Member] -> [Team]
pairings depth history activeMembers = undefined

{--
The function pairings pairs all the members who haven't been paired together 
before, and mobs any remaining people who would fall into a triple (you know, 
for groups that don't have even numbers of people.
--}
