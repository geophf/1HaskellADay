module Y2016.M11.D16.Exercise where

{--
Today we focus on the important things: coffee and chocolate.

So, we have two chemicals: caffeine and theobromine (chocolate), if we look at
their chemical compositions, see caffeine.png and chocolate.jpeg here in this 
directory or at the URLs:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D16/caffeine.png

and

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D16/chocolate.jpeg

Looking at these diagrams we see the commonality of them, the importance of the
carbon-bond.

Today's Haskell problem. We are NOT going to model caffeine nor chocholate, ...
today. Nor are we going to see how caffeine breaks down into three compounds
in the liver, one of them being chocolate ... today.

So, today we are going to be looking at chemical bonding.

If we have the atom, C, that 'wants' four chemical bonds, and

H, 1
N, 3
and O, 2

We get neat things like ... water, where H wants 1 electron each, and O wants
to give away 2 of them, so what do you have?

Water.

Or water-reactants.png and water-properties.png

And there we have it: O has 2δ- and each H as 1δ+ and together they stabilize

... if you look at this planet, there is a WHOLE LOT of stabilization going on!

Let's model this chemical valence or stability.

Given:
--}

data Atom = C | H | O | N
   deriving (Eq, Show)

-- define the valence for each of these atoms (look it up. I know you can)
-- or, better, their δ or oxidation state:

oxidationState :: Atom -> Int
oxidationState atom = undefined

-- Now, given that water and chocolate have the chemical compositions:

type Count = Int

theobromine, water :: [(Atom, Count)]
water = [(H, 2), (O, 1)]
theobromine = [(C, 7), (H, 8), (N, 4), (O, 2)]

-- theobromine, chocolate, is written: C7H8N4O2

-- Are these compounds stable? Show their net oxidation states.

stable :: [(Atom, Count)] -> Bool
stable compound = undefined

-- Of course, listing the atoms of a compound is one thing. We'll be looking
-- at chemical bonds in another exercise.
