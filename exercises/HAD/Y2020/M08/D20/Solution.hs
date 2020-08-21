module Y2020.M08.D20.Solution where

import Y2020.M08.D19.Solution

{--
Yesterday we pulled in words and propernouns from out local (or, I'm not
prejudiced, here: on the cloud, if you coded it that way) dictionary(ies).

Great!

But did you notice something?
 
"new zealand" is a ... 'word.' ... or word-phrase, if you prefer.

How many word phrases are there?

That's why, although declarative languages are good and all that, 
implementation matters, too. If you use the function `words` to grab the words 
from propernouns, you won't see "new zealand" (like you would with `lines`),
you'll have a redundant "new" that set-operations will eliminate, and you'll
be left with "zealand."

... and nobody wants that.

So, today's mission, should you choose to accept it, is to populate an
histogram of word-phrases.

What is the highest number of words in a word-phase? How many 1-word-phrases
are there? How many 2-word-phrases? Provide an histogram of word-frequency in
word-phrases for today's Haskell problem.
--}

import Control.Monad (join, (>=>))

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

allWords :: [FilePath] -> IO (Set String)
allWords = mapM readFile >=>
           return . Set.fromList . map (map toLower) . lines . join

{--
>>> length <$> allWords dicts
234371
--}

wordHistogram :: Set String -> Map Int (Set String)
wordHistogram = foldl (alterer f) Map.empty
   where f Nothing = Set.singleton
         f (Just set) = flip Set.insert set
         alterer f map val = 
            let len = length (words val)
                base = Map.lookup len map
            in  Map.insert len (f base val) map

{--
>>> Map.map length . wordHistogram <$> allWords dicts 
fromList [(1,234371)]

... m'kay! No New Zealand for moiself! :,(
--}
