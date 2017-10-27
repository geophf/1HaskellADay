module Y2017.M10.D26.Solution where

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

import Control.Arrow ((&&&))
import Control.Monad
import Data.Char (isAlpha)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time

-- below import available via 1HaskellADay git repository

import Control.List (weave)
import Control.Presentation
import Control.Scan.CSV

import Y2017.M10.D04.Solution (Topic)

-- hint: csv treats quoted values as a column. That's a good thing

type Theme = String

data ArticleClass =
   ArtC { artCix :: Integer, pri, sec :: [Topic], date :: Day, title :: String,
          notes :: String, interesting :: Bool, theme :: Maybe Theme }
      deriving (Eq, Show)

readArts :: FilePath -> IO [ArticleClass]
readArts csvfile =
    -- zipWithM_ -- (\a b -> -- print (a, csv b)) [1..] --
               -- print (a, (\[idx, pris, secs, dt, tit, nots, int, _, them] ->
           -- ArtC (readIdx idx) (csv pris) (csv secs) (read dt) tit nots
                -- (readBool int) (emptyOr them)) $ csv b)) [1..]
              -- (\a b -> print (a,b)) [1..] 
   concatMap (parseLine . csv) . tail . tail . lines <$> readFile csvfile

parseLine :: MonadPlus m => [String] -> m ArticleClass
parseLine [idx,pris,secs,dt,tit,nots,int,_,them] =
   return (ArtC (readIdx idx) (csv' pris) (csv' secs) (read dt) tit nots
                (readBool int) (emptyOr them))
parseLine f = fail ("Could not parse " ++ show f)

csv' :: String -> [String]
csv' = map trim . csv

trim, mirt :: String -> String
trim [] = []
trim word@(h:t) | h == ' ' = trim t
                | otherwise = mirt (reverse word)
mirt [] = []
mirt word@(h:t) | h == ' ' = mirt t
                | otherwise = reverse word

artDir :: FilePath
artDir = "Y2017/M10/D26/"

readIdx :: String -> Integer
readIdx idx = read idx -- error ("reading " ++ idx)

readBool :: String -> Bool
readBool b@('y':_) = True
readBool _ = False

emptyOr :: String -> Maybe String
emptyOr "" = Nothing
emptyOr l@(_:_) = Just l

-- Now that we have our article classifications, let's group them by theme

{--
>>> arts <- readArts (artDir ++ "hurricanes.csv")
>>> take 3 arts
[ArtC {artCix = 7729, pri = ["social media"], sec = ["maria","puerto rico"], 
       date = 2017-09-23, title = "When Disaster Hits and Landlines Fail, Social Media Is a Lifeline", 
       notes = "", interesting = True, theme = Just "human interest"},
 ArtC {artCix = 7570, pri = ["aid"], sec = ["maria","puerto rico"], 
       date = 2017-09-22, title = "How to Help Puerto Rico and Other Islands After Hurricane Maria", 
       notes = "", interesting = False, theme = Just "human interest"},
 ArtC {artCix = 7454, pri = ["insurance"], sec = ["cars"], 
       date = 2017-09-21, title = "How to Avoid Buying a Car Flooded by Hurricanes", 
       notes = "", interesting = False, theme = Just "howto"}]
--}

groupArts :: [ArticleClass] -> Map Theme [ArticleClass]
groupArts = 
   Map.fromList . map (fromJust . theme . head &&& id)
      . groupBy ((==) `on` theme) . sortOn theme . filter (toBool . theme)

toBool :: Maybe a -> Bool
toBool Nothing = False
toBool (Just _) = True

{--
>>> grps = groupArts arts
>>> head (Map.toList grps)
("caribbean",[ArtC {artCix = 6943, pri = ["disaster"], 
                    sec = ["hurricanes","caribbean"], date = 2017-09-18, 
                    title = "Hurricane Maria Makes Landfall in Dominica as Other Islands Brace for Potential Disaster", 
                    notes = "", interesting = True, theme = Just "caribbean"},
              ArtC {artCix = 5562, pri = ["caribbean"], sec = ["Irma"], 
                    date = 2017-09-07, 
                    title = "Caribbean Devastated as Irma Heads Toward Florida", 
                    notes = "", interesting = True, theme = Just "caribbean"},
              ArtC {artCix = 5199, pri = ["Puerto Rico"], sec = ["Irma"], 
                    date = 2017-09-06, 
                    title = "The Storm Reaches Puerto Rico: \226\128\152There Is Nothing Like This\226\128\153", 
                    notes = "", interesting = True, theme = Just "caribbean"}])
--}

-- And now we have articles-by-theme, save out each themed-set to its own-named
-- CSV file

saveThemes :: Map Theme [ArticleClass] -> IO ()
saveThemes =
   mapM_ (\(theme, arts) -> writeFile (artDir ++ sanitize theme ++ ".csv")
                                      (unlines $ map uncsv arts)) . Map.toList
      where sanitize = map endash
            endash c = if isAlpha c then c else '-'

-- we'll need to make ArticleClass an Univ-type to do this

instance Univ ArticleClass where
   explode art = [show . artCix, weave . pri, weave . sec, show . date, title,
                  notes, showBool . interesting, orEmpty . theme] <*> [art]
      where showBool True = "y"
            showBool False = ""
            orEmpty Nothing = ""
            orEmpty (Just x) = x

{--
>>> saveThemes grps 
*** Exception: Y2017/M10/D26/preparedness/response.csv:
               openFile: does not exist (No such file or directory)

D'OH! By putting a slash in my theme 'preparedness/response' I JUST OWNED MYSELF!

... so it goes.

... okay, added in sanitize that fixed that problem!

WHEW! These real-world curveballs, messin' up my beautiful, perfect problems!
--}
