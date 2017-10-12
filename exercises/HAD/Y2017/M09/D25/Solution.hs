{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2017.M09.D25.Solution where

import qualified Codec.Compression.GZip as GZ
import Control.Arrow ((&&&))
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Control.DList
import Control.Logic.Frege (adjoin, (-|))
import Store.SQL.Util.Inserts (byteStr)

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
   Art { srcId                 :: Integer,
         title                 :: String,
         author                :: Maybe String,
         url                   :: FilePath,
         abstract, fullText    :: ByteString,
         metadata              :: Map String String }
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
           Integer -> ByteString -> (ByteString, ByteString)
parseKV artId str =
   let kv = BL.split ':' str in
   if length kv < 2
   then error ("No key-value in article " ++ show artId ++ ", line: '"
            ++ byteStr str ++ "'")
   else let k = head kv
            v = BL.concat (tail kv) in
   if BL.length v == 0
   then error ("Value empty for " ++ byteStr k ++ " in article " ++ show artId)
   else (k, BL.tail v)

-- (head &&& BL.tail . BL.concat . tail) . BL.split ':'

-- Lots of guards around this parsing out of key-value pairs

-- and with that, we have a scheme to parse an article

parseArticle :: MonadPlus m => Integer -> ByteString -> m Article
parseArticle artId = parseArt' artId . sections

parseArt' :: MonadPlus m => Integer -> [ByteString] -> m Article

-- n.b. author by-line is optional for some articles.

parseArt' artId (_:doc:title:rest@(_:_:_)) =
   parseHeader (title:rest) (byteStr doc) >>= \idx ->
   let (auth, parseFn)               = parseAuthor idx (head rest)
       (url:abstract:text:meta)      = parseTail idx parseFn rest
       (abs1, abstr)                 = parseKV idx abstract
       (txt1, txt2)                  = parseKV idx text
       mets         = Map.fromList (map (adjoin byteStr . parseKV artId) meta)
   in  check idx "Abstract" abs1  >>
       check idx "Full text" txt1 >>
       return (Art artId (byteStr title) auth (byteStr url) abstr txt2 mets)
parseArt' artId huh = mzero -- for the trailing garbage at EOF

egheader :: String
egheader = "Document 102 of 999"

-- parse that line and return x in "Document x of y"

parseHeader :: MonadPlus m => [ByteString] -> String -> m Integer

{--
... but hold your horses there, pahdnah. What happens when everything goes
South, as it did for me on document 221?

Do we fail hard and cause the ETL to crash?

Well, yes. Yes, in fact, we do just that, looking at Y2017.M09.D25.Exercise.

Ah, good, then. smh.
--}

parseHeader bytes = parseHeader' bytes . words

parseHeader' :: MonadPlus m => [ByteString] -> [String] -> m Integer
parseHeader' _bytes [doc,n,off,m] =
   guard (doc == "Document" && off == "of") >>
   guard (all isDigit (n ++ m)) >>
   return (read n)

parseHeader' bytes garbage =

-- there's a special case in some archives: they have a bibliography at the end

   if garbage == ["Bibliography"] then fail "Bibliography" else

-- then there's the 'huh? what happened?' case:

   error ("Expected 'Document x of y' but got '" ++ unwords garbage ++ "';"
       ++ "context: " ++ show bytes)

-- Now parse header. What is your result?
{--
>>> parseHeader egheader
102
--}

parseAuthor :: Integer -> ByteString -> (Maybe String, [a] -> [a])
parseAuthor artId (parseKV artId -> (k,v)) =
   fromMaybe (Nothing, id) (k == "Author" -| Just (Just (byteStr v), tail))
   
parseTail :: Integer -> ([ByteString] -> [ByteString]) -> [ByteString] -> [ByteString]
parseTail artId f (tail . f -> (url:abstract:link1:link2:textish:rest)) =
   let (text,meta) = textIs textish rest in (url:abstract:text:meta)
parseTail artId _ huh =
   error ("Cannot parse rest of article " ++ show artId ++ ": " ++ show huh)

-- Now. Here. There are some random articles that have random line-breaks
-- randomly insertered, pel-mel. We must account for this or it messes up
-- parsing the metadata.

textIs :: ByteString -> [ByteString] -> (ByteString, [ByteString])
textIs txt [] = (txt, [])
textIs txt rest@(r:est) =
   if BL.length r == 0 then textIs txt est else
   case tail (BL.split ':' r) of
      [] -> textIs (BL.append txt r) est
      (_:_) -> (txt, rest)

check :: MonadPlus m => Integer -> ByteString -> ByteString -> m ()
check idx should is = if should == is then return () else
   error ("For article " ++ show idx ++ ": Key should be " ++ show should
       ++ " but is actually " ++ show is)

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
      object ["sourceId" .= srcId art, "title" .= title art,
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

Note that the attributes have a space preceding them, due to splitting on ':'
... let's fix that.

... added in fix of BL.tail to the parseKV definition.

... and it's fixed. YAY!
--}
