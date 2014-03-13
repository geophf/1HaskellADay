module HAD.Y2014.M03.D11.Solution where

import Data.Tuple (swap)

-- | lcAlphabetFrom
-- Display the alaphabet in lower cas, starting from the letter given in
-- parameter.
-- If the parameter is not a lowercase letter, displays the alphabet from 'a'
--
-- Examples:
--
-- >>> lcAlphabetFrom 'a'
-- "abcdefghijklmnopqrstuvwxyz"
--
-- >>> lcAlphabetFrom 'e'
-- "efghijklmnopqrstuvwxyzabcd"
--
-- >>> lcAlphabetFrom '`'
-- "abcdefghijklmnopqrstuvwxyz"
--
-- >>> lcAlphabetFrom '{'
-- "abcdefghijklmnopqrstuvwxyz"

lcAlphabetFrom :: Char -> String
lcAlphabetFrom = uncurry (++) . swap . flip span ['a'..'z'] . (>)

-- @sangeet_kar's solution proposed on twitter
lcAlphabetFrom' :: Char -> String
lcAlphabetFrom' = uncurry (flip (++)) . (`break` ['a'..'z']) . (==)
