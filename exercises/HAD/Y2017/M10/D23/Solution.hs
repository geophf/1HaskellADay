{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D23.Solution where

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

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Solution (Topic, fetchSubjects, Subject)

-- from fetchSubjects we get subject and their ids. Now get an id for a subject:

subjectId :: Topic -> [Subject] -> Maybe Integer
subjectId subj = Map.lookup subj . Map.fromList . map (val &&& ix)

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> subjs <- fetchSubjects conn
>>> length subjs
3817
>>> Just subj = subjectId "Hurricanes" subjs
>>> subj
708
>>> Just rain = subjectId "Rain" subjs
--}

-- Now, from that subject id, get the associated article ids from the pivot table

articleIdsFromSubject :: Connection -> Integer -> IO [Integer]
articleIdsFromSubject conn subjId =
   map fromOnly <$> query conn articleSubjectPivotStmt [subjId]

-- the SQL for that:

articleSubjectPivotStmt :: Query
articleSubjectPivotStmt =
   [sql|SELECT article_id FROM article_subject WHERE subject_id=?|]

{--
>>> artIds <- articleIdsFromSubject conn subj
>>> length artIds
173
>>> take 5 artIds
[321,655,1211,2184,3171]
>>> raidIds <- articleIdsFromSubject conn rain
--}

-- save out your article Ids, prettily, to file for later analyses

artIds2File :: FilePath -> [Integer] -> IO ()
artIds2File file = writeFile file . show

{--
>>> artIds2File "Y2017/M10/D23/hurricanes.lst" artIds

... but let's also put "Floods" into our back pocket ...

>>> subjectId "Floods" subjs
Just 1727
>>> artIds1 <- articleIdsFromSubject conn 1727
>>> length artIds1
156
>>> artIds2File "Y2017/M10/D23/floods.lst" artIds1
>>> let floodHurricanes = Set.intersection (Set.fromList artIds) (Set.fromList artIds1)
>>> length floodHurricanes 
56

>>> floodRains = Set.intersection (Set.fromList fldIds) (Set.fromList raidIds)
>>> length floodRains 
52

>>> rainHurrs = Set.intersection (Set.fromList artIds) (Set.fromList raidIds)
>>> length rainHurrs 
37

Huh.
--}

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
   ArtProxy Integer String (Maybe String) Day (Maybe String) (Maybe FilePath)
            (Maybe String) String (Maybe String) (Maybe String)
      deriving Show

instance FromRow ArticleProxy where 
   fromRow = ArtProxy <$> field <*> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field <*> field

-- oh, boy.

-- but the article proxy exists only for a moment, we hope...

articles :: Connection -> [Integer] -> IO [Article]
articles conn artIds =
   zipWith prxy2art artIds <$> query conn articlesStmt (Only (In artIds))

prxy2art :: Integer -> ArticleProxy -> Article
prxy2art idx (ArtProxy a b c d e f g h i j) =
   Art idx a b c d e f g h i j

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

{--
>>> arts <- articles conn artIds
>>> (artIdx &&& title) (head arts)
(321,"In Sweltering South, Climate Change Is Now a Workplace Hazard")
--}

-- How many articles are in the subject you chose?
-- How many articles are relevant? ... hm.

-- Save out your articles to file as JSON for further study:

instance ToJSON Article where
   toJSON art = object ["artIdx" .= artIdx art, "srcIdx" .= srcIdx art,
                        "title" .= title art, "author" .= author art,
                        "published" .= published art, "abstract" .= abstract art,
                        "url" .= url art, "section" .= section art,
                        "fullText" .= fullText art, "people" .= people art,
                        "locations" .= locations art]

writeArticles :: FilePath -> [Article] -> IO ()
writeArticles file =
   BL.writeFile file . (BL.append "[") . (`BL.append` "]")
       . BL.intercalate "," . map encodePretty

-- we'll be studying articles by topic this week
