{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Y2018.M01.D04.Exercise where

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

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- Below imports available via 1HaskellADay git repository

import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M12.D20.Exercise -- for Block
import Y2017.M12.D27.Exercise -- for DatedArticle
import Y2017.M12.D29.Exercise -- for filtering out AP articles

-- declare an Author-type, parse Author values from the sample at

sample :: FilePath
sample = "Y2017/M12/D20/sample.json"

data Author = Author { uuid, screenName, firstName, lastName :: String }
   deriving Show

instance FromJSON Author where
   parseJSON (Object o) = undefined

data Authors = Authors { authorSet :: [Author] }
   deriving Show

instance FromJSON Authors where
   parseJSON o = undefined

instance ToField Authors where
   toField authos = undefined

-- How many articles have no authors? 1? more?

-- write the storage functions for storing author data, given an article ID

instance ToRow Author where
   toRow auth = undefined

insertAuthStmt :: Query
insertAuthStmt =
   [sql|INSERT INTO author (uuid,screen_name,family_name,given_name)
        VALUES (?,?,?,?) returning id|]

insertAuthors :: Connection -> [Author] -> IO [Index]
insertAuthors conn authors = undefined

-- and with the returned author ID store the pivot table information

insertArtAuthPivotStmt :: Query
insertArtAuthPivotStmt =
   [sql|INSERT INTO article_author (article_id,author_id) VALUES (?,?)|]

-- hint: Store.SQL.Util.Pivots may help storing pivots

-- like yesterday's work, we need to manage authors on their uniqueness
-- UNLIKE yesterday's work, we do this management by uuid

instance Eq Author where
   (==) = undefined
   (/=) = undefined

instance Ord Author where
   compare = undefined

-- so we need to fetch the authors from the database and 

fetchAuthorStmt :: Query
fetchAuthorStmt =
   [sql|SELECT id,uuid,screen_name,given_name,family_name from author|]

fetchAuthors :: Connection -> IO [IxValue Author]
fetchAuthors conn = undefined

instance FromRow Author where
   fromRow = undefined

-- store the authors (above) into a memoizing table

-- using the MemoizingTable functions and the functions here, extract
-- the authors from the articles, store unique authors, and pivot on the new
-- articles' authors.

pa :: Monad m => Integer -> Result (DatedArticle Authors) -> Logger m (Maybe (DatedArticle Authors))
pa idx art = undefined

{-- BONUS -----------------------------------------------------------------

Store the articles as per Y2017.M12.D29.Exercise and, add to the ETL process
storing ancilliary information, which in this case is storing the author info.
--}

storeAncilliary :: Connection -> [IxValue (DatedArticle a)] -> IO ()
storeAncilliary conn artIds = undefined

-- reminder, storeAncilliary will now store subjects and authors.

-- Go to! Go to!
