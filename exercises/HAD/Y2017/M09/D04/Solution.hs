module Y2017.M09.D04.Solution where

import qualified Codec.Compression.GZip as GZ
import Control.Applicative (liftA2)
import Control.Arrow (second, (***))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate, sortOn)
import Data.Map ((\\))
import qualified Data.Map as Map
import Data.Monoid    -- for getSum
import Data.Ord       -- for Down
import qualified Network.HTTP.Conduit as Net
import System.Directory

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Data.Bag

{--
Today's Haskell problem will be looking at the beginning of a document processor

We have a set of articles and grant solicitions from anon under articles:
--}

type URL = FilePath

exDir, bDir, nDir :: FilePath
exDir = "Y2017/M09/D04/articles/"
bDir = exDir ++ "b/"
nDir = exDir ++ "n/"

sourceDocumentRoot :: URL
sourceDocumentRoot = "https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/" ++ exDir

{--
These documents are under the b/ and n/ directories and have file extension .gz

(although I suppose it's possible to scan for this?)
--}

-- 1. What are the names of the files in b/
-- 2. What are the names of the files in n/

type Directory = String
type FileName = String

-- of course, URLs are not local inodes, come to find. I'd have to write a
-- service or proxy to give these values, so for now ...

fileNames :: URL -> Directory -> IO [FileName]
fileNames url dir = listDirectory $ glue [url, dir]

glue :: [String] -> String
glue = intercalate "/"

{--
>>> fileNames exDir "b"
["AP880218-0096.txt.gz","AP880219-0067.txt.gz","AP880220-0086.txt.gz",
 "AP880221-0063.txt.gz","AP880222-0001.txt.gz","AP880222-0104.txt.gz",
 "AP880222-0291.txt.gz","AP880223-0242.txt.gz","AP880224-0068.txt.gz",
 "AP880224-0082.txt.gz"]

>>> fmap (take 2) $ fileNames exDir "n"
["career-physiological-genetics-of-the-dwarf-surf-clam-mulinia-lateralis.txt.gz",
 "career-probing-protein-surfaces-using-multiple-solvent-crystal-structures.txt.gz"]
--}

-- read the files into memory

loadFile :: URL -> Directory -> FileName -> IO ByteString
loadFile url dir file =
   fmap GZ.decompress $ Net.simpleHttp (glue [url, dir, file] ++ "?raw=true")

-- 3. Which file has the most number of words?

wordCount :: ByteString -> Int
wordCount = length . BL.words

{--
>>> fileNames exDir "b"                              >>= \filenames ->
    mapM (loadFile sourceDocumentRoot "b") filenames >>=
    return . zip filenames . map wordCount 
[("AP880218-0096.txt.gz",448),("AP880219-0067.txt.gz",351),
 ("AP880220-0086.txt.gz",251),("AP880221-0063.txt.gz",285),
 ("AP880222-0001.txt.gz",352),("AP880222-0104.txt.gz",927),
 ("AP880222-0291.txt.gz",67),("AP880223-0242.txt.gz",241),
 ("AP880224-0068.txt.gz",778),("AP880224-0082.txt.gz",377)]
--}

-- so the file names and contents for a directory is:

fnfc :: URL -> Directory -> [FileName] -> IO [(FileName, ByteString)]
fnfc url dir files = fmap (zip files) $ mapM (loadFile url dir) files

mostWords :: [(FileName, ByteString)] -> FileName
mostWords = fst . head . sortOn (Down . snd) . map (second wordCount)

{-- 
>>> fmap mostWords (liftA2 (++)
        (fileNames exDir "b" >>= fnfc sourceDocumentRoot "b")
        (fileNames exDir "n" >>= fnfc sourceDocumentRoot "n"))
"AP880222-0104.txt.gz"
--}

-- 4. What is the word frequency across all the b/files?
-- 5. What is the word frequency across all the n/files?

-- for this I'm going to use a Bag

wordFreq :: URL -> Directory -> [FileName] -> IO (Bag ByteString)
wordFreq _   _   []          = pure emptyBag
wordFreq url dir files@(_:_) = wf url dir files emptyBag

wf :: URL -> Directory -> [FileName] -> Bag ByteString -> IO (Bag ByteString)
wf _ _ []       ans = pure ans
wf u d (f:iles) acc = wordFreq' acc u d f >>= wf u d iles

wordFreq' :: Bag ByteString -> URL -> Directory -> FileName -> IO (Bag ByteString)
wordFreq' bag url dir = fmap (foldr add bag . BL.words) . loadFile url dir

{--
e.g.:
>>> fmap (take 2 . Map.toList) $ wordFreq' emptyBag sourceDocumentRoot "b" "AP880222-0104.txt.gz"
[("18",Sum {getSum = 1}),("180",Sum {getSum = 1})]

>>> fmap (take 5 . sortOn (Down . snd) . bagToList) $
         fileNames exDir "b" >>= wordFreq sourceDocumentRoot "b" 
[("the",223),("a",106),("of",102),("in",101),("to",85)]

>>> fmap (take 5 . sortOn (Down . snd) . bagToList) $
         fileNames exDir "n" >>= wordFreq sourceDocumentRoot "n" 
[("the",141),("of",131),("and",118),("to",91),("in",59)]

Similar word frequencies: "the" dominates in both article sets.
--}

-- 6. What is the most frequent word used in b/, ... n/? 
-- 7. What is the most frequent word across all?

mostFreq :: [(a, Int)] -> (a, Int)
mostFreq = head . sortOn (Down . snd)

mergeBags :: Ord a => Bag a -> Bag a -> Bag a
mergeBags =

-- well, let's think about this merge a and b is

-- get all the elements in a that are not in b and vice versa:
-- now we need to sum the sums of the a /\ b

-- this is Map.mergeWithKey, yes?
   Map.mergeWithKey (const (pure <<- (<>))) id id

{--
    Map.unions [(a \\ b), (b \\ a), mergeIntersectedBags a b]


mergeIntersectedBags :: Ord a => Bag a -> Bag a -> Bag a
mergeIntersectedBags a b =
   Map.fromList (zipWith (\(a,b) (_,d) -> (a, b <> d))
                         (listInter a b) (listInter b a))
      where listInter = Map.toList <<- Map.intersection
-- whoa. O(n^3 log something or other?)
--}

bagToList :: Bag a -> [(a, Int)]
bagToList = Map.toList . Map.map getSum

{--
>>> fmap (mostFreq . bagToList)
         (liftA2 mergeBags
                 (fileNames exDir "b" >>= wordFreq sourceDocumentRoot "b") 
                 (fileNames exDir "n" >>= wordFreq sourceDocumentRoot "n"))
("the",364)

moving the enhancements for bags to Data.Bag
--}
