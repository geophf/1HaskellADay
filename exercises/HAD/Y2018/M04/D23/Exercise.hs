{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D23.Exercise where

{--
Okay, we have weird titles (in that they have embedded HTML Entities. Next up,
we have some HTML in the article bodies or in their excerpts that is playing
havock with how I use TagSoup to parse out the text. Let's see where I'm going
wrong here in my previous exercises and fix that, or, given that the exception
is because of TagSoup, let's see how I can catch the error before it crashes
the ETL process, ... because crashing an ETL process is not considered good
form by some.

Here's the exception I got when parsing an article's text:

*** Exception: (TagOpen "a" [("data-cke-saved-href","http://iono.fm/e/360859"),
       ("href","http://iono.fm/e/360859"),("rel","nofollow")]) is not a TagText
CallStack (from HasCallStack):
  error, called at src/Text/HTML/TagSoup/Type.hs:97:17 in 
        tagsoup-0.14.2-7gdPxgob4rD6glk9oZ3DIj:Text.HTML.TagSoup.Type

From the logging I've done, I've discovered that it's in the fifth packet
I downloaded from the WPJ REST endpoint:

Stamped {stamped = Entry {sev = INFO, app = "wpjLoad", 
         mod = "Y2018.M04.D19.Solution", msg = "Inserting packet 5 [Idx 5]"}, 
         time = 2018-04-20 05:05:48.206319}

(you can even see when I downloaded that packet and everything).

And, checking the log data table I see that it was article ID 461.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.HTML (demark)

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M04.D02.Solution -- for Article' et al

{--
So, I went to the database and retrieved the raw JSON that I prescently stored.
That JSON is located here:
--}

trouble :: FilePath
trouble = "Y2018/M04/D23/wpj-art-461.json"

-- 1. load in this JSON into an Article' type:

readSemiParsed :: FilePath -> IO (Maybe Article')
readSemiParsed json = undefined

{--
>>> (Just art) <- readSemiParsed trouble
>>> excerpt art
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

loadSemiParsed :: Connection -> [Index] -> IO [Article']
loadSemiParsed conn idxn = undefined

fetchJSONStmt :: Query
fetchJSONStmt = [sql|SELECT json from article_json where id in (?)|]

-- (you need a FromRow instance to do that) (but it's a bit tricky, because
-- we're storing the raw JSON, not the (semi-)parsed Article'-value, so: what's
-- a good FromRow representation for raw JSON?)

{--
Okay, now that we have the Article' value. What is causing the error? The
title? The excerpt? Or the body of the article? The top-level function that
results in the error is Y2018.M04.D02.Solution.fromRendered which calls my
Data.HTML.demark which calls Text.HTML.TagSoup.parseTags. Where do things
run afoul here?
--}

{--
Getting the content of article:

>>> (Success mapi) = (fromJSON (content art)) :: Result (Map String Value)
>>> String bod = mapi Map.! "rendered"
>>> huh = demark (T.unpack bod)
>>> huh
"<a data-cke-saved-href=\8221http://iono.fm/e/360859\8243 
    href=\8221http://iono.fm/e/360859\8243 rel=\8221nofollow\8221>Content 
    hosted by iono.fm</a> \n World Policy On Air \194\160is a podcast from"...

That worked. But it did render an interesting result: there is embedded HTML
in what is supposed to be plain text. Will a second pass remove the HTML? Let's
see.

>>> demark huh
"Content hosted by iono.fm  \n World Policy On Air \194\160is a podcast from 
the pages and website of\194\160 World Policy Journal. \n As the effects of 
climate...

Yup. That worked. How about a demark on plain text, then?

>>> demark it
"Content hosted by iono.fm  \n World Policy On Air \194\160is a podcast from 
the pages and website of\194\160 World Policy Journal. \n As the effects of 
climate...

Yup. demark of plain text turns into the id function. WOOT!

Okay, let's look at the excerpt:

>>> (Success mapi1) = (fromJSON (excerpt yo)) :: Result (Map String Value)
>>> String exc = mapi1 Map.! "rendered"
>>> wat = demark (T.unpack exc)
>>> wat
"<a data-cke-saved-href=\"http://iono.fm/e/360859\" 
    href=\"http://iono.fm/e/360859\" 
    rel=\"nofollow\">Content hosted by iono.fm</a> \n"

That worked, and the iterative demarkings ...

>>> demark wat
"Content hosted by iono.fm  \n"
>>> demark it
"Content hosted by iono.fm  \n"

Behold! The fixpoint!

Ditto for title:

>>> (Success mapt) = (fromJSON (title art)) :: Result (Map String Value)
>>> String tit = mapt Map.! "rendered"
>>> demark (T.unpack tit)
"World Policy On Air, Ep. 94: Challenges and Opportunities in the Arctic"
--}

-- So we have no errors. What the hey. We'll dig deeper tomorrow.
