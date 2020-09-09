module Y2020.M09.D09.Exercise where

{--
Yesterday (which, oddly enough, is really yesterday, in this particular case),
--}

import Y2020.M09.D08.Exercise
import Y2020.M09.D01.Exercise (Ontology)

{--
we did a modicum of data analyses and reduced a word-... set? word-... net?
from ~250,000 words to ~2,000 words.

We can work with either set, but 2,000 words, approximating standard daily
usage is an avenue I'd like to explore.

But here's a problem, ... an administrative problem.

To get to the 2,000 words, we need to load in all the documents, load in all
the words, good and bad, then reduce to those 2,000 words.

For today's #haskell problem, let's compile those 2,000 words into a Haskell
program, so we have those words right away, and get rid of that pesky monad: IO.
--}

compile :: Ontology -> FilePath -> IO ()
compile bookwordnet outputfile = undefined

{--
The function `compile` compiles `bookwordnet` to a Haskell module, named in
`outputfile` and placed that Ontology value and the corresponding 
WordOccurrences value into that file, so, when imported, is available for
immediate use.

DO IT TO IT!
--}
