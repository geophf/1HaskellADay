module Y2020.M08.D25.Exercise where

import Data.Map (Map)

{--
Let's continue, shall we? #BladeRunner

To get to the point of cosine similarities, we need (to generate) a set of
vector-sets to align cosinely similarly.

So there.

Today's #haskell exercise.

From project gutenberg:
--}

baseUrl :: FilePath
baseUrl = "http://www.gutenberg.org/"

{--
We have oodles of books to ingest and scalar-vectorify (that's a word, now).
We're NOT going to be doing the ingest and scalar-vectorification today, 
however, to get to that point, we need to know which books are where (bear
with me here, Haskellers).

We DO have indices, and one of those indices, I've captured (in part) here.

Today, we're going to ingest and codify this index.
--}

workingDir :: FilePath
workingDir = "Y2020/M08/D25/"

gutenbergTop100Index :: FilePath
gutenbergTop100Index = "gutenberg-top100.html"

-- yes, it's in HTML. yes: deal with it.

type BookIndex = Map String FilePath

gutenbergIndex :: FilePath -> IO BookIndex
gutenbergIndex html = undefined

{--
Now.

You can import a prebuilt HTML parser ... like tagsoup, for example, or, as 
this HTML is rather simple: a small subset and regular, you can roll your
own. How'msoeverest you want to do it, JUST SHOW ME ANSWERS!


DO IT, ... TO IT!
--}
