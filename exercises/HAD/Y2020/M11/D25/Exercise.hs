module Y2020.M11.D25.Exercise where

{--
So, we're TOTALLY going to publish all airbases and all alliances and correct
that the USA is not in NATO, even though the USA is in the graph database...

... but that's not what we're doing today.

TODAY! How do airbases communicate?

WITH A DICK TRACY SUPER-SECRET CODE!

WHAT SUPER-SECRET CODE? Well, obviously, in a super-secret code nobody knows
and nobody uses.

Morse code.

(My Uncle Roy, a licensed HAM radio owner/operator will now find me and kill me)

Now, morse code is this:

https://en.wikipedia.org/wiki/Morse_code

(I'm not going to type out the morse code. You can follow the link.)

Now, you can hand-represent each letter with each da-dit, 'by hand,' but
earlier this week there was a graph representation of the same. I've added
that graph to the repository here as morse-code.png.

Either way you wanna do it: have a plain-text-to-morse-code translator.
--}

data Morse = DA | DIT | SPACE  -- ... the final frontier, or nah?
   deriving (Eq, Show)

data MorseTable = Table Char [Morse]
   deriving (Eq, Show)         -- this can be a Map or a Graph or whatever

morse :: Char -> [Morse]
morse = undefined

-- what is the morse code of:

ciceroOnPain :: String
ciceroOnPain = "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit..."

morseCodify :: String -> [[Morse]]
morseCodify = undefined
