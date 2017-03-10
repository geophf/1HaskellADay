module Y2017.M03.D10.Exercise where

import Data.Map (Map)

{--
A little bit of linguistics-love for Today's Haskell problem.

So, this being Lent 2017, I've taken it upon myself to meditate daily on 
Psalm 51, along with listening to the Hillard's Ensemble rendition of Arvo
Pärt's Miserere:

https://www.youtube.com/watch?v=aNy7pCOq7oI

Now, the thing is, there are so many ways to deconstruct language and thought.

One is, well, Zipf's law

https://en.wikipedia.org/wiki/Zipf%27s_law

(urg! That took forever to find) And that is an interesting study to follow
... but not today.

Let's look at the opening verse of PS 51 in three different languages:
--}

latin, english, french :: String
latin = "Miserere mei Deus secundum magnam misericordiam tuam et secundum "
     ++ "multitudinem miserationum tuarum dele iniquitatem meam"

english = "Have mercy on me O God according to thy great mercy And "
       ++ "according to the multitude of thy tender mercies blot out my "
       ++ "iniquity"

french = "O Dieu aie pitié de moi dans ta bonté Selon ta grande miséricorde "
      ++ "efface mes transgressions"

{--
We're not going to do Zipf distribution analysis, instead, we're going to 
identify languages by their word endings.

For each of the three samples above, assign the (probability) distribution
of word-endings to their respective language:
--}

data Tongue = LATIN | ENGLISH | FRENCH deriving (Eq, Ord, Show)

type Distribution = Map Char Rational

distribution :: String -> Distribution
distribution verse = undefined

{--
The distribution-function categorizes a verse by its distribution of word-
endings. Now, take those distributions and relate them to their languages.
--}

languages :: [Tongue] -> [Distribution] -> Map Tongue Distribution
languages langs dists = undefined

{-- BONUS -----------------------------------------------------------------

Classify (in so whichever means you choose) the following snippets, assigning
the language to the snippet. How did you fare?
--}

ovid, austen, flaubert :: FilePath
ovid = "metamorphoses6.txt"
austen = "pride-and-prejudice.txt"
flaubert = "madame-bovary.txt"

-- at this directory, of course, or you can use the raw text at the URLs on
-- the git repository. Either way

dir :: FilePath
dir = "Y2017/M03/D10/"

classify :: Map Tongue Distribution -> FilePath -> Tongue
classify ontology file = undefined

-- So, ... how did we do? ;)
