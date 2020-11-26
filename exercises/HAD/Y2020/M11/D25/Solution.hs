{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Y2020.M11.D25.Solution where

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

import Control.Arrow (second)

import Data.Char (toUpper)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

data Morse = DA | DIT | SPACE  -- ... the final frontier, or nah?
   deriving Eq

type MorseTable = Map Char [Morse]

instance Show Morse where
   show DA = "-"
   show DIT = "."
   show SPACE = " "

instance Show [Morse] where
   show [] = ""
   show (h:t) = show h ++ concat (map show t)

instance Read Morse where
   readsPrec _ (c:har) = return (c' c, har)
      where c' '-' = DA
            c' '.' = DIT
            c' ' ' = SPACE

morseTable :: MorseTable
morseTable = Map.fromList $ map (second (map (read . return)))
   [('A', ".-"), ('B', "-..."), ('C', "-.-."),('D', "-.."), ('E', "."),
    ('F', "..-."),('G', "--."),('H', "...."),('I',".."),('J',".---"),
    ('K', "-.-"),('L', ".-.."),('M',"--"),('N',"-."),('O',"---"),('P',".--."),
    ('Q', "--.-"),('R', ".-."),('S',"..."),('T',"-"),('U',"..-"),('V',"...-"),
    ('W', ".--"),('X', "-..-"),('Y',"-.--"),('Z',"--.."),(' '," ")]
   

morse :: MorseTable -> Char -> Maybe [Morse]
morse = flip Map.lookup

-- what is the morse code of:

ciceroOnPain :: String
ciceroOnPain = "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit..."

morseCodify :: MorseTable -> String -> [[Morse]]
morseCodify mt = mapMaybe (morse mt) . map toUpper

{--
>>> morseCodify morseTable ciceroOnPain 
[-.,.,--.-,..-,., ,.--.,---,.-.,.-.,---, ,--.-,..-,..,...,--.-,..-,.-,--, ,
 .,...,-, ,--.-,..-,.., ,-..,---,.-..,---,.-.,.,--, ,..,.--.,...,..-,--, ,--.-,
 ..-,..,.-, ,-..,---,.-..,---,.-., ,...,..,-, ,.-,--,.,-, ,-.-.,---,-.,...,.,
 -.-.,-,.,-,..-,.-., ,.-,-..,..,.--.,..,...,-.-.,.., ,...-,.,.-..,..,-]
--}
