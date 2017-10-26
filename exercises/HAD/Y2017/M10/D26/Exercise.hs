module Y2017.M10.D26.Exercise where

{--
Okay, so we've subdivided our hurricanes (or the topics you chose) into 
subtopics by doing a little bit of scanning into the documents. Are we done?

OF COURSE NOT!

Some articles are 'trash'; some articles are wildly misclassified from the
media source (you know, for teh viewzorx!) and some articles, yes, mention
the topic, e.g.: Harvey, to talk about "President Trump is Not Taking Climate
Change Seriously" Or "104 YO Man, Harvey, Reflects with his wife, Irma, on the
Last Century!"

No, really. There are articles about those topics in the archives.

So, we're going to do a little 'artificial-artificial intelligence' today 
(that's the name google gives: 'doing it by hand'). I've categorized and
subcategorized these articles by hand by going through the archive and reading
them.

Ugh. Yup. I was up all last night doing this.

Then I've assigned a theme to each article, like: gov't or Trump, or
climate change, or human interest, or preparedness/response or some other
themes.

Today's Haskell problem, read in my artificial-artificial intelligence
classifications-by-theme of these articles from hurricanes.csv on this
directory, then, group each article by theme, THEN save out each article into
its own <theme>.csv file
--}

import Data.Map (Map)

-- below import available via 1HaskellADay git repository

import Control.Scan.CSV

-- hint: csv treats quoted values as a column. That's a good thing

data ArticleClass = ARowFromYourCSVFile

readArts :: FilePath -> IO [ArticleClass]
readArts csvfile = undefined

-- Now that we have our article classifications, let's group them by theme

type Theme = String

groupArts :: [ArticleClass] -> Map Theme [ArticleClass]
groupArts arts = undefined

-- And now we have articles-by-theme, save out each themed-set to its own-named
-- CSV file

saveThemes :: Map Theme [ArticleClass] -> IO ()
saveThemes themes = undefined
