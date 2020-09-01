module Y2020.M08.D31.Solution where

{--

"Yesterday,"* ...

* where "yesterday" is "our last working day where we, the workers, get
weekends off"-yesterday.

... "yesterday" we discovered that Charles Dickens is Charles Darwin is
Charlie Kaufman, because they all wrote the word: "the."

Yay! Everything is related to everything, and we can all go home, now.

Good to know, but also not helpful.

So, there are many common or connective words that don't relate to the topic*

* but then, if you're writing a research paper on the English use of the 
definite article "the" and the Polish avoidance of the definite article when
speaking in English, then "the" is very much the topic, and what are you going
to do about that, huh? Nothing? Is that your answer?

and those are know as STOPWORDS, and, unix systems also have in

/usr/share/dict/

a set of words called connectives.

And guess what the very first word is in connectives.

Just.
guess.

So!

Today's Haskell problem.

Yesterday* ...
--}

import Y2020.M08.D28.Solution

{--
... we created a word-frequency analysis of the cleaned-text of Charles
Dickens' "A Christmas Carol." And we learned "the" is the most-frequent word.

TODAY,* ...

*Today, n.: actually today, unlike 'yesterday's' meaning varies

... let us compute what the most-frequent word is, having removed the STOPWORDS.
--}

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

import Y2020.M08.D25.Solution (workingDir, gutenbergTop100Index)

stopwords :: FilePath
stopwords = "/usr/share/dict/connectives"

loadStopwords :: FilePath -> IO (Set String)
loadStopwords connectives = Set.fromList . lines <$> readFile connectives

removeStopwords :: Set String -> Map String Int -> Map String Int
removeStopwords stopwords =
   Map.filterWithKey (const . not . flip Set.member stopwords)

{--
What is Charles Dickens most-frequent word in "A Christmas Carol," having
removed all stopwords?

>>> let conns = loadStopwords stopwords
>>> let weirdos = Set.fromList "!\"#$%'()*,-./0123456789:;?@[]\182\187\191"
>>> let bookus = study (workingDir ++ gutenbergTop100Index) 
>>> let bookwords = cleanDoc weirdos <$> bookus
>>> let wordus = wordFreq <$> bookwords
>>> length <$> wordus
4852

>>> let keywords = removeStopwords <$> conns <*> wordus
>>> length <$> keywords
4702

>>> take 5 . sortOn (Down . snd) . Map.toList <$> keywords 
[("scrooge",314),("upon",120),("ghost",93),("christmas",92),("project",87)]

Okay, NOW we're talking!
--}

go :: IO [(String, Int)]
go = let weirdos = Set.fromList "!\"#$%'()*,-./0123456789:;?@[]\182\187\191"
     in  study (workingDir ++ gutenbergTop100Index) >>=
         return . wordFreq . cleanDoc weirdos       >>= \wordus ->
         loadStopwords stopwords                    >>=
         return . take 5 . sortOn (Down . snd)
                . Map.toList . flip removeStopwords wordus
