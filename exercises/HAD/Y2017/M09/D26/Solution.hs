{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D26.Solution where

import qualified Codec.Compression.GZip as GZ
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D20.Solution (inserter)
import Y2017.M09.D22.Solution (scanArticles, dir, arts, rawText, articleTextById)
import Y2017.M09.D25.Solution (parseArticle, metadata, Article, artId)

{--
So, I wrote a whole exercise for today, which you will see as tomorrow's 
exercise, instead, because then I realized that it was too much for one day 
without some introduction.

So, today's Haskell problem: a name, by any other name, would smell as sweet.

A datum from Friday's exercise is "Person":

>>> articles <- scanArticles . GZ.decompress <$> BL.readFile (dir ++ arts)
>>> Just art3 = parseArticle 3 (rawText $ head articles)
>>> Map.lookup "People" (metadata art3)
Just "Cuomo, Mario M"

This is an example where the article is about just one person. As you scan the
articles you will see some that are about more than one person and the names
will be in various formats.

Today we will not worry about formats of name(s) in the Person field.

Because today we're simply going to store the names.

Say you have a staging table in PostgreSQL called name_stg with the following
structure:
--}

data RawNames = Raw { fromArticle :: Integer, text :: String }
   deriving (Eq, Ord, Show)

-- with our handy insert statement:

insertRawNamesStmt :: Query
insertRawNamesStmt = [sql|INSERT INTO name_stg (article_id,names) VALUES (?,?)|]

-- from the above, derive the below:

instance ToRow RawNames where
   toRow rn = [toField (fromArticle rn),toField (text rn)]

-- this looks like an applicative functor definition, but it's weird because
-- the toField 'function' is actually two different function value types.

{-- 
Okay, great, and you can use the inserter from before to construct the
procedure that inserts RawNames values into PostgreSQL database.

Before we do that, we have to convert articles scanned to a list of raw names 
values. And before we do that, let's create a function that pipelines the whole
process of extracting articles from the archive and reifying those articles
to the Y2017.M09.D25.Article type.
--}

type Compressed = ByteString
-- reminder to me that this is a compressed archive

extractArticles :: Compressed -> [Article]
extractArticles gz = Map.toList articles >>= uncurry parseArticle
   where articles = articleTextById . scanArticles $ GZ.decompress gz

{--
>>> articles <- extractArticles <$> BL.readFile (dir ++ arts)
--}

-- then let's grab the line that has the raw names listed from each article

art2RawNames :: Article -> Maybe RawNames
art2RawNames = fmap . Raw . artId <*> Map.lookup "People" . metadata

-- really? mapping the functor OVER the Applicative functor? REALLY?

{--
>>> names = mapMaybe art2RawNames articles
>>> names
[Raw {fromArticle = 1, text = "Cuomo, Mario M"},
 Raw {fromArticle = 2, text = "Reagan, Ronald Wilson"},
 Raw {fromArticle = 3, text = "Obama, Barack Cameron, David"},
 Raw {fromArticle = 4, text = "Armstrong, Karen"},
 Raw {fromArticle = 5, text = "Cuomo, Mario M"},
 Raw {fromArticle = 7, text = "Rivlin, Reuven"},
 Raw {fromArticle = 8, text = "Francis (Pope)"},
 Raw {fromArticle = 10, text = "Yingluck Shinawatra"},
 Raw {fromArticle = 11, text = "Baraka, Amiri"}]

n.b.: articles 6 and 9 have no people associated with them.

Previous attempts with self-critiques:

* uncurry fmap . (Raw . artId &&& Map.lookup "Person" . metadata)

whenever I see the pattern 

uncurry f . (this &&& that)

I know I'm unnecessarily complicating things

I mean: why compose a tuple simply to uncurry it?

* fmap (Raw (artId art)) . Map.lookup "Person" $ metadata art

ugh: repeated names ('art'). That's gross.
--}

-- and with that transformation function, we can insert raw names from articles

insertAllRawNames :: Connection -> [RawNames] -> IO ()
insertAllRawNames conn = inserter conn insertRawNamesStmt

{-- 
For all the articles compressed in the archive, (dir ++ arts), insert the
names from the Person metadata into the names_stg table at the index artId.

>>> connectInfo 
ConnectInfo {connectHost = "..." ...}
>>> conn <- connect it
>>> insertAllRawNames conn names
>>> close conn

How many rows did you insert? [low key: your answer should be '11']

$ select count(1) from name_stg;
9
[actually, it was 9 rows, as two articles didn't have associated people]

$ select * from name_stg;

id	article_id	names
----------------------------------------------------
1	1		Cuomo, Mario M
2	2		Reagan, Ronald Wilson
3	3		Obama, Barack Cameron, David
4	4		Armstrong, Karen
5	5		Cuomo, Mario M
6	7		Rivlin, Reuven
7	8		Francis (Pope)
8	10		Yingluck Shinawatra
9	11		Baraka, Amiri

Now: how many names did you insert?

We will address that question tomorrow when we get into some simple name
parsers.
--}

{-- BONUS -----------------------------------------------------------------

Output your RawNames values as JSON.

--}

instance ToJSON RawNames where
   toJSON rn = object ["fromArticle" .= fromArticle rn, "names" .= text rn]

-- BONUS-BONUS ------------------------------------------------------------

-- prettily.

{--
>>> mapM_ (BL.putStrLn . encodePretty) names
{
    "names": "Cuomo, Mario M",
    "fromArticle": 1
}
{
    "names": "Reagan, Ronald Wilson",
    "fromArticle": 2
}
{
    "names": "Obama, Barack Cameron, David",
    "fromArticle": 3
}
{
    "names": "Armstrong, Karen",
    "fromArticle": 4
}
{
    "names": "Cuomo, Mario M",
    "fromArticle": 5
}
{
    "names": "Rivlin, Reuven",
    "fromArticle": 7
}
{
    "names": "Francis (Pope)",
    "fromArticle": 8
}
{
    "names": "Yingluck Shinawatra",
    "fromArticle": 10
}
{
    "names": "Baraka, Amiri",
    "fromArticle": 11
}
--}
