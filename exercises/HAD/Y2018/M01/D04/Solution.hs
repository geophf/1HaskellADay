{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Y2018.M01.D04.Solution where

{--
Yesterday we processed and uploaded keywords, leveraging the work we did before
with subjects.

Let's process authors today.
--}

import Data.Aeson

{--
What is the structure for authors? Well. Let's see:

"authors": [
                {
                    "screen_name": "victoria bourne",
                    "first_name": "Victoria",
                    "uuid": "5a919c4a-6225-11e5-8a96-10604b9ffe60",
                    "last_name": "Bourne",
                    "byline": null,
                    "avatar": "https://pilotonline.com/pilotonline.com/content/tncms/avatars/5/a9/19c/5a919c4a-6225-11e5-8a96-10604b9ffe60.3f16c8799c2e097f4b6d34d4d11423cc.png"
                }
            ],

There's one sample.

            "authors": [],

There's another.

... and I thought I saw two authors somewhere in the sample, so we shall see.
--}

import Control.Monad (zipWithM_, void)
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- Below imports available via 1HaskellADay git repository

import Control.List (weave)

import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

-- import Y2017.M12.D20.Solution -- for Block
import Y2017.M12.D27.Solution (DatedArticle)
import qualified Y2017.M12.D27.Solution as Art
import Y2017.M12.D29.Solution hiding (etl) -- for filtering out AP articles
import Y2018.M01.D02.Solution hiding (storeAncilliary)  -- etl process

-- declare an Author-type, parse Author values from the sample at

data Author = Author { uuid, screenName, firstName, lastName :: String }
   deriving Show

instance FromJSON Author where
   parseJSON (Object o) =
      Author <$> o .: "uuid" <*> o .: "screen_name"
             <*> o .: "first_name" <*> o .: "last_name"

data Authors = Authors { authorSet :: [Author] }

instance FromJSON Authors where
   parseJSON o = Authors <$> parseJSONList o

-- How many articles have no authors? 1? more?

{--
>>> pac <- readSample sample
>>> blx = rows pac
>>> (ans, log) = runWriter (elide apArt blx)
>>> mapM_ (print . fmap authors . snd) ans

There are many articles that have no authors, many that have one, and one
article has two authors:

Just [Author {uuid = "6cc07684-6225-11e5-8b68-10604b9ffe60",
              screenName = "margaret matray", firstName = "Margaret",
              lastName = "Matray"},
      Author {uuid = "596d7514-6225-11e5-8a89-10604b9ffe60",
              screenName = "bill bartel", firstName = "Bill",
              lastName = "Bartel"}]

parseAuthors :: Block -> Parser [Author]
parseAuthors (Object o) = o .: "authors"

blk2Auths :: Block -> [Author]
blk2Auths o = fromJust (HM.lookup "authors" o)

 case (fromJSON (parseAuthors o)) of
   Success vals -> vals
   Error err    -> error ("Cannot parse authors: " ++ err)
--}

-- write the storage functions for storing author data, given an article ID

-- for ALL the authors we want to store them as one concatenated line

instance ToField Authors where
   toField (Authors auth) = toField (weave (map screenName auth))

-- for EACH author we want to store them in their own table

instance ToRow Author where
   toRow auth = [toField] <*> ([uuid,screenName,lastName,firstName] <*> [auth])

insertAuthStmt :: Query
insertAuthStmt =
   [sql|INSERT INTO author (uuid,screen_name,family_name,given_name)
        VALUES (?,?,?,?) returning id|]

insertAuthors :: Connection -> [Author] -> IO [Index]
insertAuthors conn = returning conn insertAuthStmt

-- and with the returned author ID store the pivot table information

insertArtAuthPivotStmt :: Query
insertArtAuthPivotStmt =
   [sql|INSERT INTO article_author (article_id,author_id) VALUES (?,?)|]

-- hint: Store.SQL.Util.Pivots may help storing pivots

-- like yesterday's work, we need to manage authors on their uniqueness
-- UNLIKE yesterday's work, we do this management by uuid

instance Eq Author where
   x == y = uuid x == uuid y
   x /= y = uuid x /= uuid y

instance Ord Author where
   compare x = compare (uuid x) . uuid

-- so we need to fetch the authors from the database and 

fetchAuthorStmt :: Query
fetchAuthorStmt = 
   [sql|SELECT id,uuid,screen_name,given_name,family_name from author|]

fetchAuthors :: Connection -> IO [IxValue Author]
fetchAuthors conn = query_ conn fetchAuthorStmt

instance FromRow Author where
   fromRow = Author <$> field <*> field <*> field <*> field

-- store the authors (above) into a memoizing table

-- using the MemoizingTable functions and the functions here, extract
-- the authors from the articles, store unique authors, and pivot on the new
-- articles' authors.

-- we need to process the authors from raw blocks to do this:

pa :: Monad m => Integer -> Result (DatedArticle Authors) -> Logger m (Maybe (DatedArticle Authors))
pa idx (Success art) =
   say ("Parsed " ++ Art.uuid art) >> return (Just art)
pa idx (Error err) =
   say ("Could not parse article " ++ show idx ++ ", error: " ++ err) >>
   return Nothing

storeAuthors :: Connection -> [IxValue (DatedArticle Authors)] -> IO ()
storeAuthors conn ixarts =
   fetchAuthors conn >>= \preauths ->
   let memtable = MT.start (map ix2tup preauths)
       (ids,arts) = unzip (map ix2tup ixarts)
       stat = execState (zipWithM_ MT.triageM ids (map (authorSet . Art.authors) arts))
                                   (memtable,Map.empty)
       substate = Set.toList (MT.newValues (fst stat))
   in  insertAuthors conn substate >>= \ixaut ->
   let table = MT.update (zip (map idx ixaut) substate) (fst stat)
   in  void (executeMany conn insertArtAuthPivotStmt
                 (evalState buildPivots (table, snd stat)))

{-- BONUS -----------------------------------------------------------------

Store the articles as per Y2017.M12.D29.Exercise and, add to the ETL process
storing ancilliary information, which in this case is storing the author info.
--}

storeAncilliary :: Connection -> [IxValue (DatedArticle Authors)] -> IO ()
storeAncilliary conn arts =
   storeSubjects conn arts >>
   storeAuthors conn arts

-- reminder, storeAncilliary will now store subjects and authors.

-- Go to! Go to!

{--
>>> withConnection (flip (etl pa storeAncilliary) sample)

$ select * from author LIMIT 10;

id	uuid					screen_name		family_name	given_name
1	109faa4e-627f-11e7-b017-8b9380b98cda	briana adhikusuma	Adhikusuma	Briana	
2	235ac1c6-d915-11e6-8909-cf3bb4084961	editorial board			
3	569ea0a2-8425-11e5-86d9-ef5ff1939894	Brock Vergakis		Vergakis	Brock	
4	59029802-6225-11e5-8a84-10604b9ffe60	clay barbour		Barbour		Clay	
5	595844e6-6225-11e5-8a88-10604b9ffe60	mary reid barrow	Barrow		Mary Reid	
6	596d7514-6225-11e5-8a89-10604b9ffe60	bill bartel		Bartel		Bill	
7	5a7acea2-6225-11e5-8a95-10604b9ffe60	larry bonko		Bonko		Larry	
8	5a919c4a-6225-11e5-8a96-10604b9ffe60	victoria bourne		Bourne		Victoria	
9	5c19f7ee-d104-11e5-9924-f306847abc86	mary beth gahan		Gahan		Mary Beth	
10	5d3c7528-6225-11e5-8ab5-10604b9ffe60	mike connors		Connors		Mike	

$ select * from article_author LIMIT 10;

id	article_id	author_id
1	688		8
2	689		37
3	694		13
4	695		38
5	696		20
6	697		11
7	698		22
8	699		14
9	700		14
10	701		37
--}
