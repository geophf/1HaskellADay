module Y2018.M12.D04.Solution where

{--
Today we're going to find more than our kayak, like we did yesterday. Today
we're going to find 5-letter palindromes.

Locate your computer's dictionary. For me, mine is at

/usr/share/dict/words

Now, from that dictionary, find words of five letters and capitalize them.
--}

import Control.Arrow ((&&&))

import Data.Char (toUpper)
import Data.Set (Set)
import qualified Data.Set as Set

-- below module available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)

wordDict :: FilePath
wordDict = "/usr/share/dict/words"

fiveLetterWords :: FilePath -> IO (Set String)
fiveLetterWords =
   fmap (Set.map (map toUpper) . Set.fromList . filter ((== 5) . length) . words)
      . readFile

{-- how many words are in this set?

>>> fivers <- fiveLetterWords wordDict 
>>> length fivers
9972

>>> take 10 (Set.toList fivers)
["AALII","AARON","ABACA","ABACK","ABAFF","ABAFT","ABAMA","ABASE","ABASH","ABASK"]

--}

-- Now find the palindromes. How many palindromes do you have? Is KAYAK in
-- tnhis set?

palindrome :: String -> Bool
palindrome = uncurry (==) . adjoin (take 3) . (id &&& reverse)

{--
>>> palies = Set.filter palindrome fivers
>>> length palies 
25
>>> Set.member "KAYAK" palies
True
>>> palies
{"AJAJA","ALALA","ALULA","ANANA","ARARA","CIVIC","KAYAK","KAZAK","KELEK",
 "LEMEL","LEVEL","MADAM","MESEM","MINIM","RADAR","REFER","REVER","ROTOR",
 "SAMAS","SERES","SIRIS","TEBET","TENET","ULULU","YARAY"}
--}
