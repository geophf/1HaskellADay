module Y2021.M02.D24.Solution where

import Data.Time

{--
What's today's date?

Today's Haskell problem is to get the current date as a YYYY-mm-dd-formatted
string from ... wherever you get the current date.
--}

today :: IO String
today = take 10 . show <$> getCurrentTime

{--
>>> today
"2021-02-24"
--}
