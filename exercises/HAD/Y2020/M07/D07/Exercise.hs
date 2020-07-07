module Y2020.M07.D07.Exercise where

{--
So, I built this pairing-algorithm for our Agile team, so that people paired
before (phasing out history as we go), would not be paired again, and what 
happens if there are an uneven number of people in the team?

Let's break down this pairing-algorithm-problem bit-by-bit. I solved it using
a graph database and Prolog, because that's how I roll, but we're going to
solve it using Haskell, because that's how WE rollz, yo!

First problem. Let's establish what we mean by 'history' to establish
a context for pairing today.

'History' is a tricky thing, come to think about it, because 'yesterday'
isn't always today - 1 days ago: there are weekends and holidays to consider
to compute what yesterday (or working days prior to this date) are.

How I solve this problem, when I'm faced with the problem of 'give me the
previous x days of data,' is to create a linked list of working, or data, 
days. Each new working day is added to the top of this stack-o-days, and I
have a pointer (ToS, or 'top o stack') to the top-most stored day.

Today's Haskell problem is to create this structure as well as to create the
'give me x previous days'-function.
--}

import Data.Time

type History = [Day]

addDay :: Day -> History -> History
addDay day history = undefined         -- is this ... monadic?

history :: Int -> History -> [Day]
history forXDays history = undefined

-- Looks rather ... simple, yes? But the History-type will be enriched
-- as we develop this app over the next few days.

-- geddit? '... over the next few ... DAYs'? GEDDIT?
