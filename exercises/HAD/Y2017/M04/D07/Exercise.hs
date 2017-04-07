module Y2017.M04.D07.Exercise where

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
twoTiles width = undefined
threeTiles width = undefined
fourTiles width = undefined
fiveTiles width = undefined
sixTiles width = undefined
sevenTiles width = undefined

-- where twoTiles, etc, give the total number of tiles used if each corner has
-- a block of tiles two, or three, or ... tiles deep.

-- You ... COULD generalize the above equations if you want...

-- Now we need the guards, or test functions

oneTileRemainingFor, comesOutEvenFor :: Int -> Int -> Bool
oneTileRemainingFor ntiles depth = undefined
comesOutEvenFor ntiles depth = undefined

-- Of course, we want to test that it comes out even for a depth of seven,
-- but GENERALIZATION IS GOOD! Just in case you need a comesOutEvenFor function
-- for your everyday Haskelling tasks.

-- You're welcome.

-- You ... COULD even generalize those 'generalized' guards, but I didn't want
-- to get carried away by generality. By the Admiralty, sure, but not by
-- generality.

-- You'll get that driving home today, I'm sure.

-- Now we need to tie these functions together.

-- What is the total number of tiles, t, that equals twoTiles n2, threeTiles n3,
-- ... etc all of which are modulo 1 to their respective widths n2, n3, etc,
-- and are modulo 0 to depth of seven, n7?

totalTiles :: [Int]
totalTiles = undefined

-- totalTiles gives us the total number of tiles that satisfies the above
-- constraints. So you've just written a constraint solver to help you decorate
-- your house for Spring. lolneat.
