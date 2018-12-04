module Y2018.M12.D04.Exercise where

{--
Today we're going to find more than our kayak, like we did yesterday. Today
we're going to find 5-letter palindromes.

Locate your computer's dictionary. For me, mine is at

/usr/share/dict/words

Now, from that dictionary, find words of five letters and capitalize them.
--}

import Data.Set (Set)

wordDict :: FilePath
wordDict = undefined

fiveLetterWords :: FilePath -> IO (Set String)
fiveLetterWords file = undefined

-- how many words are in this set?

-- Now find the palindromes. How many palindromes do you have? Is KAYAK in
-- tnhis set?

palindrome :: String -> Bool
palindrome str = undefined
