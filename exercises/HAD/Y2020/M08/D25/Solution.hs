module Y2020.M08.D25.Solution where

import Control.Arrow (first)
import Control.Monad ((>=>), mzero, MonadPlus)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tuple (swap)

import Text.HTML.TagSoup

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
gutenbergIndex html = Map.fromList . map swap . scanLiRows <$> tagsFromFile html

{--
Now.

You can import a prebuilt HTML parser ... like tagsoup, for example, or, as 
this HTML is rather simple: a small subset and regular, you can roll your
own. How'msoeverest you want to do it, JUST SHOW ME ANSWERS!


DO IT, ... TO IT!

>>> gutenbergIndex (workingDir ++ gutenbergTop100Index)
{...,
 ("A Dictionary of Cebuano Visayan by John U. Wolff (256)",
  "http://www.gutenberg.org//ebooks/40074"),
 ("A Journal of the Plague Year by Daniel Defoe (278)",
  "http://www.gutenberg.org//ebooks/376"),
 ("A Modest Proposal by Jonathan Swift (736)",
  "http://www.gutenberg.org//ebooks/1080"),...}
--}

-- first off, we want to get a set of tags:

tagsFromFile :: FilePath -> IO [Tag String]
tagsFromFile = readFile >=> return . parseTags

{--
>>> tagsFromFile (workingDir ++ gutenbergTop100Index)
[TagOpen "html" [],TagText "\n ",TagOpen "body" [],TagText "\n",...]
--}

-- Now, let's get to what we care about: the LI-rows

scanLiRows :: [Tag String] -> [(FilePath, String)]
scanLiRows = slr' []

slr' :: [(FilePath, String)] -> [Tag String] -> [(FilePath, String)]
slr' ans [] = ans
slr' acc (tag:tags) | tag == TagOpen "li" [] =
   uncurry slr' (maybe (acc, tags) (first (:acc)) (snarfRow tags))
                    | otherwise           = slr' acc tags

snarfRow :: MonadPlus m => [Tag String] -> m ((FilePath, String), [Tag String])
snarfRow tags = fetchA tags >>= \(a, r0) ->
                fetchtxt r0 >>= \(t, r1) ->
                return ((baseUrl ++ a,t), r1)

fetchA :: MonadPlus m => [Tag String] -> m (FilePath, [Tag String])
fetchA (TagOpen "a" [("href", url)]:tags) = return (url, tags)
fetcha _ = mzero

fetchtxt :: MonadPlus m => [Tag String] -> m (String, [Tag String])
fetchtxt (TagText txt:tags) = return (txt, tags)
fetchtxt _ = mzero
