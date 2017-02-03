{-# LANGUAGE ViewPatterns #-}

module Y2017.M02.D03.Solution where 

import Control.Monad (guard)
import Prelude hiding (Word)
import Data.Char (toUpper)
import Data.Set (Set, delete, fromList, union, splitRoot, toList)

{--
Let's turn to the Mensa Genuis Quiz-a-Day book again for today's problem.

Then maybe some more Rosalind stuff next week? Or interesting NASA data-science?

We shall see.

So, today's problem is from August 4:

Can you go from PINK to ROSE in four steps, changing one letter at a time and
making a new English word at each step?

P I N K
_ _ _ _
_ _ _ _
_ _ _ _
R O S E

Use a dictionary of English words: mine is at /usr/share/dict/words, yours may
be elsewhere.
--}

type Word = String
type Dictionary = Set Word

englishWords :: FilePath -> IO Dictionary
englishWords dict =
   fromList . filter ((&&) . isFour <*> startsWith "PR") . words . map toUpper
   <$> readFile dict

isFour :: Word -> Bool
isFour = (== 4) . length

startsWith :: [Char] -> Word -> Bool
startsWith [] = const False
startsWith (h:t) = (||) . (== h) . head <*> startsWith t

{--
*Y2017.M02.D03.Solution> englishWords "/usr/share/dict/words" ~> dict
*Y2017.M02.D03.Solution> length dict ~> 527

Some of these 'English' words, though... really?
--}

firstWord, lastWord :: Word
firstWord = "PINK"
lastWord  = "ROSE"

-- unnecessary: we are working with only English words anyway: no check needed
-- verifyEnglishWord :: Dictionary -> Word -> Bool
-- verifyEnglishWord = flip member

pickOne :: Dictionary -> [(Word, Dictionary)]
pickOne = p1 . splitRoot

p1 :: [Dictionary] -> [(Word, Dictionary)]
p1 [] = []
p1 [s1, elt, s2] =
   let subset = union s1 s2
       ans = (head (toList elt), subset) in
   ans : pickOne subset

oneLetterDifference :: Word -> Word -> Int -> Bool
oneLetterDifference [] [] n = n == 1
oneLetterDifference (a:b) (c:d) n =
   if n > 1 then False
   else oneLetterDifference b d ((if a == c then id else succ) n)

{-- So, to get the second word:

*Y2017.M02.D03.Solution> let uno = pickOne dict           >>=
  \(w, rest) -> guard (oneLetterDifference w firstWord 0) >>
  return (w, rest)
*Y2017.M02.D03.Solution> length uno ~> 13
*Y2017.M02.D03.Solution> map fst uno
["PISK","PINE","PING","PINT","PINY","PUNK","PINA","PIND","PINO","RINK","PENK",
 "PICK","PANK"]
--}

interimWords :: Dictionary -> Word -> Word -> Int -> [Word] -> [[Word]]
interimWords _ _ endword 0 (w:ords) =
   guard (oneLetterDifference w endword 0) >> return (reverse (w:ords))
interimWords dict startWord endWord n acc = 
   pickOne (delete startWord dict) >>= \(w, rest) -> 
   guard (oneLetterDifference w startWord 0) >>
   interimWords rest w endWord (pred n) (w:acc)

{--
*Y2017.M02.D03.Solution> interimWords dict firstWord lastWord 3 []
[["PISK","PISE","POSE"],["PISK","PISE","RISE"],["PISK","RISK","RISE"],
 ["PINE","PISE","POSE"],["PINE","PISE","RISE"],["PINE","PONE","RONE"],
 ["RINK","RISK","RISE"]]

Okay, but can we eliminate some ridiculous solutions?
--}

removeRidiculousWords :: Dictionary -> Dictionary
removeRidiculousWords = flip (foldr delete) ["PISK","PISE","PONE","RONE"]

{--
*Y2017.M02.D03.Solution> let realdict = removeRidiculousWords dict
*Y2017.M02.D03.Solution> length realdict ~> 523
*Y2017.M02.D03.Solution> interimWords realdict firstWord lastWord 3 []
[["RINK","RISK","RISE"]]

The give Mensa book answer was: "One solution is PINE PONE POSE" ... so there's
no accounting for taste. "PONE"?!?

pone, Virginian Algonquin: corn bread.

Yeah. Right. Whatever.

RINK RISK RISE also works.
--}
