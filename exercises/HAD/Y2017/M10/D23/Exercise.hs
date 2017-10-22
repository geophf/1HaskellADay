{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D23.Exercise where

{--
Here I am!
Rock me like a Hurricane!

The NYT archives has a plethora of articles classified by subject, as we saw
last week. This week, we're going to start teasing apart the archives by 
subject.

Select a subject, get its index, and from the index, extract the articles on
that subject.

As for me, I'm picking 'Hurricanes.'
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Exercise (Topic, fetchSubjects, Subject)

-- from fetchSubjects we get subject and their ids. Now get an id for a subject:

subjectId :: Topic -> [Subject] -> Maybe Integer
subjectId subj subjs = undefined

-- Now, from that subject id, get the associated article ids from the pivot table

articleIdsFromSubject :: Connection -> Integer -> IO [Integer]
articleIdsFromSubject conn = undefined

-- the SQL for that:

articleSubjectPivotStmt :: Query
articleSubjectPivotStmt =
   [sql|SELECT article_id FROM article_subject WHERE subject_id IN ?|]

-- save out your article Ids, prettily, to file for later analyses

artIds2File :: FilePath -> [Integer] -> IO ()
artIds2File file artIds = undefined

-- Now we have our article ids. Get the articles

articlesStmt :: Query
articlesStmt =
   [sql|SELECT src_id, title, author, publish_dt, abstract, url, section,
               full_text, people, locations
        FROM article
        WHERE id IN ?|]

-- Okay, this is neat. I have a structure for storing articles (Article), but
-- I don't have a structure for extracting them. So, guess what we're doing now?

data ArticleProxy =
   ArtProxy Integer String (Maybe String) Day String FilePath (Maybe String)
            String String String

instance FromRow ArticleProxy where 
   fromRow = undefined

-- oh, boy.

-- but the article proxy exists only for a moment, we hope...

articles :: Connection -> [Integer] -> IO [Article]
articles conn artIds = undefined

prxy2art :: Integer -> ArticleProxy -> Article
prxy2art idx proxy = undefined

-- ...because we immediately reify them as articles:

data Article =
   Art { artIdx, srcIdx    :: Integer,
         title             :: String,
         author            :: Maybe String,
         published         :: Day,
         abstract          :: Maybe String,
         url               :: Maybe FilePath,
         section           :: Maybe String,
         fullText          :: String,
         people, locations :: Maybe String }
   deriving (Eq, Show)

-- How many articles are in the subject you chose?
-- How many articles are relevant? ... hm.

-- Save out your articles to file as JSON for further study:

instance ToJSON Article where
   toJSON art = undefined

writeArticles :: FilePath -> [Article] -> IO ()
writeArticles file arts = undefined

-- we'll be studying articles by topic this week
