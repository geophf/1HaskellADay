{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D23.Solution where

{--
Okay, we have weird titles (in that they have embedded HTML Entities. Next up,
we have some HTML in the article bodies or in their excerpts that is playing
havock with how I use TagSoup to parse out the text. Let's see where I'm going
wrong here in my previous exercises and fix that, or, given that the exception
is because of TagSoup, let's see how I can catch the error before it crashes
the ETL process, ... because crashing an ETL process is not considered good
form by some.

Here's the exception I got when parsing an article's text:

*** Exception: (TagOpen "a" [("data-cke-saved-href","http://iono.fm/e/360859"),("href","http://iono.fm/e/360859"),("rel","nofollow")]) is not a TagText
CallStack (from HasCallStack):
  error, called at src/Text/HTML/TagSoup/Type.hs:97:17
     in tagsoup-0.14.2-7gdPxgob4rD6glk9oZ3DIj:Text.HTML.TagSoup.Type

From the logging I've done, I've discovered that it's in the fifth packet
I downloaded from the WPJ REST endpoint:

Stamped {stamped = Entry {sev = INFO, app = "wpjLoad", mod = "Y2018.M04.D19.Solution", msg = "Inserting packet 5 [Idx 5]"}, time = 2018-04-20 05:05:48.206319}

(you can even see when I downloaded that packet and everything).

And, checking the log data table I see that it was article ID 461.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M04.D02.Solution -- for Article' et al
import Y2018.M04.D18.Solution -- JSONString

{--
So, I went to the database and retrieved the raw JSON that I prescently stored.
That JSON is located here:
--}

trouble :: FilePath
trouble = "Y2018/M04/D23/wpj-art-461.json"

-- 1. load in this JSON into an Article' type:

readSemiParsed :: FilePath -> IO (Maybe Article')
readSemiParsed = fmap decode . BL.readFile

{--
>>> art <- readSemiParsed trouble 
>>> excerpt <$> art
Just {
    "protected": false,
    "rendered": "<p><iframe frameborder=\"0\" height=\"135\" 
        src=\"https://embed.iono.fm/epi/360859\" width=\"100%\">&lt;a 
        data-cke-saved-href=&quot;http://iono.fm/e/360859&quot; 
        href=&quot;http://iono.fm/e/360859&quot; 
        rel=&quot;nofollow&quot;&gt;Content hosted by iono.fm&lt;/a&gt;
      </iframe></p>\n"
}
--}

-- (alternatively you can load the JSON in question from the database)

loadSemiParsed :: Connection -> [Index] -> IO [Article]
loadSemiParsed conn = fmap (mapMaybe jsonStr2Article') . query conn fetchJSONStmt

fetchJSONStmt :: Query
fetchJSONStmt = [sql|SELECT json from article_json where id in (?)|]

jsonStr2Article' :: JSONString -> Maybe Article
jsonStr2Article' (S' str) = decode $ BL.pack str

instance FromRow JSONString where
   fromRow = S' <$> field

-- (you need a FromRow instance to do that) (but it's a bit tricky, because
-- we're storing the raw JSON, not the (semi-)parsed Article'-value, so: what's
-- a good FromRow representation for raw JSON?)

{--
>>> withConnection WPJ (\c -> loadSemiParsed c [Idx 461] >>= print)
[Art {art = Art' {idx = 10391, date = Just 2016-11-18 11:09:08 +0000, 
                  title = Object (fromList [("rendered",
             String "World Policy On Air, Ep. 94: Challenges and Opportunities
                     in the Arctic")]),

Okay, so we can load an article fine from the JSON stored in the database, so
perhaps the error is storing the article into the database. We'll look at that
tomorrow.
--}
