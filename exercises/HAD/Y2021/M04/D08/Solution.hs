module Y2021.M04.D08.Solution where

{--
readMaybe.

You have a String that you'd like to parse into a typed value, ...

... because this is Haskell, and all, ICYMI, ...

But you're not sure the string is well-formed.

What you want to do is accept the string if it is well-formed, and reject
it if it isn't, and move onto the next task, that is to say.

>>> (read "5") :: Int
5

works, but 

>>> (read "5s") :: Int
*** Exception: Prelude.read: no parse

... we don't want to interrupt our program, nor our accumulated value-set,
we want to move onto the next values and work with what we have.
--}

import Data.Maybe (listToMaybe, mapMaybe)

sumnums :: String
sumnums = "1 3 4 3 5 6 4 6 4 g 4  6 3 2 5  2 2 d 3 6"

-- what is the sum of the numbers in the above input string?

readMaybe :: Read a => String -> Maybe a
readMaybe s = listToMaybe (fst <$> reads s)

-- and how would you parse the above string into individual substrings for
-- parsing?

{--
>>> sum $ mapMaybe readMaybe (words sumnums)
69
--}
