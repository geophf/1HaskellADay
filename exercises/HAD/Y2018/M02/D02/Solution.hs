module Y2018.M02.D02.Solution where

{--
We have been able to download sets of articles and then we've triaged them,
but before we do triage, we have to do a bifurcation from before: we don't
want Associated Press articles showing up in our NEW / UPDATED / REDUNDANT
stacks.

Today's Haskell problem is to remove the AP articles from the downloaded
packets.

Hint: you may have seen this before in a different context.
--}

import Control.Monad ((>=>))
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

import Database.PostgreSQL.Simple (Connection, connect, close)

-- below imports available via 1HaskellADay git repository

import Control.DList (dlToList)
import Control.Logic.Frege (assert)

import Data.Logger (Logger, LogEntry)
import Data.Time.Stamped (Stamped)

import Store.SQL.Connection (withConnection, Database(PILOT))

import Y2017.M12.D27.Solution (DatedArticle)
import Y2017.M12.D29.Solution (apArt)

import Y2018.M01.D26.Solution (ow)
import Y2018.M01.D29.Solution (oneWeekAgo)
import Y2018.M01.D30.Solution hiding (main') -- for triaging from the triage

deAPify :: DatedArticle a -> Maybe (DatedArticle a)
deAPify = assert apArt

-- given a parsed article, remove it (return Nothing) if it's an AP article

-- How many articles did you download?

-- How many articles were left after you delinted the AP articles?

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> owa <- oneWeekAgo conn
>>> owa
2018-01-23
>>> (packs,log) <- runWriterT (ow owa 0 [])
>>> length packs
5
>>> arts = articles packs
>>> length arts
459
>>> pilotArts = filter (apArt . snd . snd) (articles packs)
>>> length pilotArts 
444
--}

{-- BONUS -----------------------------------------------------------------

Recreate the triage report app that downloads the articles and triages them,
but this time, first removes the AP articles
--}

triageSansAP :: Connection
             -> IO (([ArticleMetaData], Map Triage [ArticleTriageInformation]), [Stamped LogEntry])
triageSansAP conn = do
   owa <- oneWeekAgo conn
   (packs,log) <- runWriterT (ow owa 0 [])
   putStrLn ("Fetched " ++ show (length packs) ++ " packets")
   let term = addDays (-1) owa
   let arts = articles packs
   let pilotArts = filter (apArt . snd . snd) arts
   putStrLn ("Excluded " ++ show (length arts - length pilotArts)
          ++ " AP articles")
   amd <- fetchArticleMetaData conn term
   return ((amd, triageArticles amd pilotArts), dlToList log)

main' :: [String] -> IO ()
main' [] = errmsg
main' ["go"] = withConnection PILOT (triageSansAP >=> \((_,tri), log) ->
                 mapM_ print log            >>
                 print (Map.map length tri))
main' _ = errmsg
