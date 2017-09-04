module Y2017.M09.D04.Exercise where

import Codec.Compression.GZip
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.HTTP.Conduit

{--
Today's Haskell problem will be looking at the beginning of a document processor

We have a set of articles and grant solicitions from anon under articles:
--}

type URL = FilePath

sourceDocumentRoot :: URL
sourceDocumentRoot = "https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2017/M09/D04/articles/"

{--
These documents are under the b/ and n/ directories and have file extension .gz

(although I suppose it's possible to scan for this?)
--}

-- 1. What are the names of the files in b/
-- 2. What are the names of the files in n/

type Directory = String
type FileName = String

fileNames :: URL -> Directory -> IO [FileName]
fileNames url dir = undefined

-- read the files into memory

loadFile :: URL -> Directory -> FileName -> IO ByteString
loadFile url dir file = undefined

-- 3. Which file has the most number of words?

wordCount :: ByteString -> Int
wordCount contents = undefined

mostWords :: [(FileName, ByteString)] -> FileName
mostWords idfiles = undefined

-- 4. What is the word frequency across all the b/files?
-- 5. What is the word frequency across all the n/files?

wordFreq :: URL -> Directory -> IO [(String, Int)]
wordFreq  url dir = undefined

-- 6. What is the most frequent word used in b/, ... n/? 
-- 7. What is the most frequent word across all?

mostFreq :: [(String, Int)] -> (String, Int)
mostFreq wordFreqs = undefined
