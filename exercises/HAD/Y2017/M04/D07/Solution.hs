module Y2017.M04.D07.Solution where

import Control.Monad (guard)

-- below module available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

{--
From the Mensa Genius Quiz-a-Day Book by Dr. Abbie F. Salny

Problem for April 7th:

"You are decorating for Spring," the problem begins, and I'm like: I am?

Dr. Salny responds: "CHUT UP, FOO'! AND READ THE PROBLEM!"

Okay, then!

You are decorating for Spring, and you've found a bargain: a huge box of 
beautiful decorated tiles, ...

... [see what she did with that? 'decorating for Spring' and 'decorated tiles'?]

... enough to provide a border in two rooms. You really can't figure out how to
arrange them, however. If you set a border of two tiles all around, there's one
left over; if you set three tiles all around, or four, or five, or six, there's
still one tile left over.* Finally, you try a block of seven tiles for each 
corner, and you come out even.

What is the smallest number of tiles you could have to get this result?

* OCD much? Is it me, or would I just throw away the one extra tile, seeing that
I had bought it at a bargain, anyway, and what's one tile between friends, 
seeing that I have a huge box of them, anyway?**

** two 'seeing's and two 'anyway's. OCD much, geophf?

Why, yes. Why do you ask?

ANYWAY!

MODULAR FORMS! Did you know that education in basic mathematics in Japan besides
addition, subtraction, multiplication and division includes the modulus as a
basic operator? So this problem should be a breeze for our Japanese Haskellers.

So, solve this problem.

First we need to know what the equations are.
--}

twoTiles, threeTiles, fourTiles, fiveTiles, sixTiles, sevenTiles :: Int -> Int
twoTiles   = ntiles 2
threeTiles = ntiles 3
fourTiles  = ntiles 4
fiveTiles  = ntiles 5
sixTiles   = ntiles 6
sevenTiles = ntiles 7

-- where twoTiles, etc, give the total number of tiles used if each corner has
-- a block of tiles two, or three, or ... tiles deep.

-- You ... COULD generalize the above equations if you want...

ntiles :: Int -> Int -> Int
ntiles = (8 *) <<- (*)     -- there are 4 corners in each of the two rooms.
                           -- I mean, I am assuming that there are, but that
                           -- makes every possible solution even, even the seven
                           -- tiles per corner solution, so this could get ugly!

-- Now we need the guards, or test functions

oneTileRemainingFor, comesOutEvenFor :: Int -> Int -> Bool
oneTileRemainingFor = moduloIs 1
comesOutEvenFor     = moduloIs 0

-- Of course, we want to test that it comes out even for a depth of seven,
-- but GENERALIZATION IS GOOD! Just in case you need a comesOutEvenFor function
-- for your everyday Haskelling tasks.

-- You're welcome.

-- You ... COULD even generalize those 'generalized' guards, but I didn't want
-- to get carried away by generality. By the Admiralty, sure, but not by
-- generality.

-- You'll get that driving home today, I'm sure.

-- and, yes, I do want to generalize the guards:

moduloIs :: Int -> Int -> Int -> Bool
moduloIs m = (== m) <<- mod

-- Now we need to tie these functions together.

-- What is the total number of tiles, t, that equals twoTiles n2, threeTiles n3,
-- ... etc all of which are modulo 1 to their respective widths n2, n3, etc,
-- and are modulo 0 to depth of seven, n7?

{--
But let's think about this problem for a second! 

1. we need modulo7 == 0 in all cases
2. we have 8 corners??? really??? 

Eh, let's just say we're decorating n corners, instead with 2,3,...7 tiles
and go from there.

That means we have 7,14,21, etc tiles.

Let's do this. Let's work from a total number of tiles and work backwards.

.. Also we know even numbers are out (mod 2 == 0) so we can just skip those:
--}

totalTiles :: [Int]
totalTiles = map (*7) [1,3 ..] >>= \tot ->
   guard (tot `mod` 3 == 1 && tot `mod` 4 == 1 && tot `mod` 5 == 1
       && tot `mod` 6 == 1) >> return tot

-- totalTiles gives us the total number of tiles that satisfies the above
-- constraints. So you've just written a constraint solver to help you decorate
-- your house for Spring. lolneat.

{--
>>> head totalTiles 
301
--}
