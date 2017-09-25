{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D25.Solution where

import qualified Codec.Compression.GZip as GZ
import Control.Arrow ((&&&))
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Control.DList
import Control.Logic.Frege (adjoin)

import Y2017.M09.D22.Solution (scanArticles, articleTextById, dir, arts, rawText)

{--
So, 'yesterday' (Friday) we were able to scan a compressed NYT article archive,
separate out each article, then return those articles as Haskell structures
(that could be output as JSON if desired). Great!

Today, we will start rending that structure into smaller pieces.

From your scanning exercise yesterday, you noticed that the articles follow this
form:

_______________________________

Document x of 1000

[title]

Author: [author]

Publication info: [publisher]

[article url]

Abstract: [article synopsis]

Links: [url separated by newlines]

Full text: [the full, well, text]

[more metadata in key: value format]

_______________________________

Okay, so, first of all, that's the first blush for the first article in the
compressed archive. Is this assessment correct, or am I trippin' on some really
bad 'shrooms?

Second of all, parse the document set into this enriched Article format.
--}

data Article =
   Art { artId              :: Int,
         title, author      :: String,
         url                :: FilePath,
         abstract, fullText :: ByteString,
         metadata           :: Map String String }
      deriving (Eq, Show)

-- n.b. I ignore some attributes in the raw text.

{--
So. Toady –  ... no, wait: TODAY, because 'toady' is just wrong – take the 
articles separated out in yesterday's exercise then parse them into the 
structure given above.
--}

-- notice something: line breaks in the raw text indicate semantic breaks ..
-- the full text of the articles don't have empty lines.

-- so what we need here is a 'smart'-lines function:

sections :: ByteString -> [ByteString]
sections = map BL.concat . section emptyDL . BL.lines

section :: DList ByteString -> [ByteString] -> [[ByteString]]
section dl [] =
   let ans = dlToList dl in if null ans then [] else [ans]
section dl (h:t) =
   if BL.length h == 0 then dlToList dl : section emptyDL t
                       else section (dl <| h) t

{--
So, how many sections in yesterday's article-as-block?

>>> articles <- scanArticles . GZ.decompress <$> BL.readFile (dir ++ arts)
>>> sects = sections (rawText $ head articles)
>>> length sects
32
>>> take 5 sects
[[],
 ["Document 1 of 1000"],
 ["Mario M. Cuomo, 1932-2015"],
 ["Author: Nagourney, Adam"],
 ["Publication info: New York Times , Late Edition (East Coast); New York, N.Y. [New York, N.Y]02 Jan 2015: A.1."]]

Okay, then!

So, let's tease these sections apart and put data to data, as it were.
--}

-- welp, one thing we need fer shur is a key: value parser

parseKV :: -- maybe I should study up on parser technology? idk
           -- [ByteString] -> Maybe ((ByteString, ByteString), [ByteString])
           ByteString -> (ByteString, ByteString)
parseKV = (head &&& BL.tail . BL.concat . tail) . BL.split ':'

-- and with that, we have a scheme to parse an article

parseArticle :: MonadPlus m => Int -> ByteString -> m Article
parseArticle artId txt =
   let (_:_:title:author:_pub:url:abstract:_link1:_link2:text:meta)
            = sections txt
       (auth1, auth) = parseKV author
       (abs1, abstr) = parseKV abstract
       (txt1, txt2)  = parseKV text
       mets          = Map.fromList (map (adjoin byteStr . parseKV) meta)
   in  check "Author" auth1 >>
       check "Abstract" abs1 >>
       check "Full text" txt1 >>
       return (Art artId (byteStr title) (byteStr auth) (byteStr url)
                   abstr txt2 mets)

byteStr :: ByteString -> String
byteStr = BL.unpack

check :: MonadPlus m => ByteString -> ByteString -> m ()
check should is = if should == is then return () else
   error ("Key should be " ++ show should ++ " but is actually " ++ show is)

{--
>>> Just art3 = parseArticle 3 (rawText $ head articles)
>>> BL.putStrLn (BL.take 80 (fullText art3))
*** Exception: Key should be "Full Text" but is actually "Full text"
CallStack (from HasCallStack):
  error, called at Y2017/M09/D25/Solution.hs:135:4 in main:Y2017.M09.D25.Solution

That was when my key was "Full Text" NOT "Full text" ... fixed that.

>>> BL.putStrLn (BL.take 80 (fullText art3))
   CORRECTION APPENDEDMario M. Cuomo, the three-term governor of New York who c

and

>>> Map.keys (metadata art3)
["CODEN","Copyright","Country of publication","Database","Document URL",
 "Document type","ISSN","Language of publication","Last updated","Pages",
 "People","Place of publication","ProQuest document ID","Publication date",
 "Publication subject","Publication title","Publication year","Publisher",
 "Section","Source type","Subject","Title"]

WOOT!
--}

{-- BONUS -----------------------------------------------------------------

Output the above structure as JSON. Yeah, I went there. Fight me.
--}

instance ToJSON Article where
   toJSON art =
      object ["articleId" .= artId art, "title" .= title art,
              "author" .= author art, "url" .= url art,
              "abstract" .= byteStr (abstract art),
              "fullText" .= byteStr (fullText art),
              "metadata" .= metadata art]

{--
>>> BL.putStrLn $ encodePretty art3 
{
    "url": "https://search.proquest.com/docview/1641860292?accountid=14696",
    "fullText": " Â  CORRECTION APPENDEDMario M. Cuomo, the three-term governor
    ...
    "author": " Nagourney, Adam",
    "abstract": " Delivering the keynote address at the 1984 Democratic ..."
    "title": "Mario M. Cuomo, 1932-2015",
    "articleId": 3
}

Note that the attributes have a space preceeding them, due to splitting on ':'
... let's fix that.

... added in fix of BL.tail to the parseKV definition.

... and it's fixed. YAY!
--}
