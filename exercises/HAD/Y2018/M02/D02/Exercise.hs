module Y2018.M02.D02.Exercise where

{--
We have been able to download sets of articles and then we've triaged them,
but before we do triage, we have to do a bifurcation from before: we don't
want Associated Press articles showing up in our NEW / UPDATED / REDUNDANT
stacks.

Today's Haskell problem is to remove the AP articles from the downloaded
packets.

Hint: you may have seen this before in a different context.
--}

import Data.Map (Map)

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Data.Logger (Logger)

import Y2017.M12.D27.Exercise (DatedArticle)
import Y2018.M01.D26.Exercise -- for downloading from the endpoint
import Y2018.M01.D29.Exercise -- for determining how far back to go
import Y2018.M01.D30.Exercise -- for triaging from the triage

deAPify :: DatedArticle a -> Maybe (DatedArticle a)
deAPify art = undefined

-- given a parsed article, remove it (return Nothing) if it's an AP article

-- How many articles did you download?

-- How many articles were left after you delinted the AP articles?

{-- BONUS -----------------------------------------------------------------

Recreate the triage report app that downloads the articles and triages them,
but this time, first removes the AP articles
--}

triageSansAP :: Connection
             -> Logger IO ([ArticleMetaData], Map Triage [ArticleTriageInformation])
triageSansAP conn = undefined

main' :: [String] -> IO ()
main' args = undefined
