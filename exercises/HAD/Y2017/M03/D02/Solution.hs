module Y2017.M03.D02.Solution where

import Control.Arrow (first, (&&&))
import Data.Function (on)
import Data.List (unfoldr)

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Rosalind.Types

{--
My problem is that I'm stupid.

No, really. I keep creating these generate-and-test algorithms that work fine
in a microcosm, but as soon as I open up the throttle everything goes to
combinatorial-hell. I've done this over and over and over again.

I've even done permutations of doing generate-and-test: over and over again!

SO!

For today's haskell problem, we're just going to toss the idea of finding
subsequences efficiently, because that's way too generate-and-test-y (and, yes,
I'm feeling quite testy right now), and, instead, since we know we progress
through each list, anyway, we'll just use those progressions simultaneously to
arrive at our solution at O(n+m)-time in the worst case scenario.

YAY! Champaign for everyone!

... I mean: of course we have to solve the problem, but still, I have a good
feeling about this approach (because it's not generate-and-test-y)

So, today's Haskell problem:

Given two lists, as and bs, advance through each list to the next same element
in both lists covering the least distance possible in each list at each step.

Oh, and accumulate the matching elements as you go, of course!
--}

minDistanceTraversals :: Eq a => [a] -> [a] -> [[a]]
minDistanceTraversals = pure <<- curry (unfoldr (uncurry minimumest))

{--
Of course, that's the top-down view of our approach. What do we need to do to
arrive at the solution? Well, we need an automaton to crawl forward in each 
list, recording the distance it went, then we need to find a way to minimize
the combined distance the automata travel through these lists ...

... is this starting to sound a lot like graph theory?
--}

distance :: Eq a => a -> [a] -> Maybe (Int, [a])
distance = distance' 0

distance' :: Eq a => Int -> a -> [a] -> Maybe (Int, [a])
distance' _      _   []                = Nothing
distance' offset elt (h:t) | elt == h  = Just (offset, t)
                           | otherwise =
   distance' offset elt t >>= return . first succ

-- it's actually distance' we want, as we are most often dealing with offsets

{--
>>> distance 1 [1..10]
Just (0, [2..10])

>>> distance 11 [1..10]
Nothing

>>> distance 5 [1..10]
Just (4,[6,7,8,9,10])

So we know, now the distance from head as to the same element in bs:

distance (head as) bs

And we know the same for head bs to the same element in as:

distance (head bs) as

So we just take the min of those distances and proceed?

Not so fast there, cowboy! What if we have:

as = [5, 1, 12, 17, ..]
bs = [7, 1, 13, 15, ..]

Then those distances are deceiving, because we have an even shorter path:

head (tail as) == head (tail bs)

How do we cover this possibility (that a distance in depth is shorter than
a 'surface' distance)?

We need to find the min of the distances in depth going only as far into the
lists as the min of the surface distances, right?

What's the algorithm for that?

Given: index-depth == 0
       min-surface-distance = d

1. add 2 to the index-depth as we take the tails of both lists
2. compute the distances there (factoring in the index-depth)
3. decrement the min-surface-distance by 2
4. lather. rinse. repeat until min-surface-distance < 1

Then find the 'minimumest' distance of all those distances.

minimumest means not just minimum, but minimum of the minimums, or WAY minimum.
--}

data Path a = P { dist :: Int, val :: a, path :: ([a], [a]) }
   deriving (Eq, Show)

instance Eq a => Ord (Path a) where
   compare = compare `on` dist

minimumest :: Eq a => [a] -> [a] -> Maybe (a, ([a], [a]))
minimumest [] _ = Nothing
minimumest _ [] = Nothing
minimumest (a:as) (b:bs) =

-- given two lists as and bs, find the next element that is shared by both
-- lists using the least amount of steps to get to that next shared element.

-- first, we find the respective initial distances, to get a feel of the
-- challenge

   race 0 (a:as) (b:bs) >>= project 2 as bs . dist <*> id

-- find the two distances from each respective list and return the winner

{--
race :: Eq a => Int -> [a] -> [a] -> Maybe (Path a)
race offset (a:as) (b:bs) =
   distance' offset a (b:bs) >>= \(bdist, newbs) ->
   distance' offset b (a:as) >>= \(adist, newas) ->
   return (min (P bdist a (as, newbs)) (P adist b (newas, bs)))

-- of course, we need to cover the case if one of the returns a value
-- and one of them doesn't! ... and under Min-domain

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
   mempty = Min Nothing
   Min Nothing `mappend` x           = x
   x           `mappend` Min Nothing = x
   Min a       `mappend` Min b       = Min (min a b)

-- now we can write a race-function

race :: Eq a => Int -> [a] -> [a] -> Maybe (Path a)
race offset (a:as) (b:bs) =
   getMin (Min (distance' offset a (b:bs))
        <> Min (distance' offset b (a:as))) >>= \(dist, news)

... nope, we need both. We can't afford information-loss here. So we need a
race' fn that takes Maybe-arguments and deals with the fallout that way:
--}

race :: Eq a => Int -> [a] -> [a] -> Maybe (Path a)
race offset (a:as) (b:bs) =
   race' (a:as) (b:bs) (distance' offset a (b:bs)) (distance' offset b (a:as))

race' :: Eq a => [a] -> [a] -> Maybe (Int, [a]) -> Maybe (Int, [a])
      -> Maybe (Path a)
race' _ _ Nothing Nothing = Nothing
race' (a:as) _ (Just (bdist, newbs)) Nothing = Just (P bdist a (as, newbs))
race' _ (b:bs) Nothing (Just (adist, newas)) = Just (P adist b (newas, bs))
race' (a:as) (b:bs) (Just (bdist, newbs)) (Just (adist, newas)) =
   return (min (P bdist a (as, newbs)) (P adist b (newas, bs)))

-- here is the engine of minimumest:

project :: Eq a => Int -> [a] -> [a] -> Int -> Path a -> Maybe (a, ([a], [a]))
project _      [] _  _       _                   = Nothing
project _      _  [] _       _                   = Nothing
project offset as bs mindist minp | mindist <= offset = Just ((val &&& path) minp)
                                  | otherwise         =
   race offset as bs >>=
   (project (offset + 2) (tail as) (tail bs) . dist <*> id) . min minp

-- so, for the lists:

rosalind23, rosalind64 :: DNAStrand
rosalind23 = "AACCTTGG"
rosalind64 = "ACACTGTGA"

-- What is the 'first' minimumest? The next minimumest (from the resulting
-- returned sublists)? The next-next minimumest ... How many minimumest values
-- can you get from the above two rosalind lists until you get Nothing?

{--
>>> minimumest rosalind23 rosalind64
Just ('A',("ACCTTGG","CACTGTGA"))

>>> it >>= uncurry minimumest . snd
Just ('A',("CCTTGG","CTGTGA"))

>>> it >>= uncurry minimumest . snd
Just ('C',("CTTGG","TGTGA"))

>>> it >>= uncurry minimumest . snd
Just ('T',("TGG","GTGA"))

>>> it >>= uncurry minimumest . snd
Just ('T',("GG","GA"))

>>> it >>= uncurry minimumest . snd
Just ('G',("G","A"))

>>> it >>= uncurry minimumest . snd
Nothing

... and you see how we now have our solution as an unfold-structure? Recall
the type of Data.List.unfoldr:

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

We'll tie this together tomorrow, vis:

>>> unfoldr (uncurry minimumest) (rosalind23, rosalind64)
"AACTTG"

BUT!

>>> unfoldr (uncurry minimumest) (rosalind64, rosalind23)
"ACCTG"

Uh, oh! We'll have to explore this further!
--}
