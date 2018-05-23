module Y2018.M05.D17.Solution where

{--
Today's Haskell problem is partially written out. That provides its own set of
problems: you have code that you are inheriting, and you need to build function-
ality on top of the existing code base.

Not that that ever happens in industry.

So, today you have to learn what exists and how to work with that. This exercise
also works with a customized Writer/State/IO monad, so working in the monadic
domain is part of today's problem.

HAVE AT IT!
--}

import Codec.Text.IConv (convert, EncodingName)

import Control.Monad.State

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Data.Logger
import Data.LookupTable
import Data.Time.Stamped (stampIt)

import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging (insertStampedEntries)

import Y2017.M11.D01.Solution (SpecialCharTable)
import Y2018.M04.D02.Solution -- Article
import Y2018.M04.D04.Solution (MemoizedAuthors)
import Y2018.M05.D08.Solution

{--
In a prior exercise (see above import) we were able to triage articles we are
getting from a REST endpoint into NEW, UPDATED, REDUNDANT vis-a-vis already-
stored articles in a PostgreSQL database.

Today we're going to be handling two cases REDUNDANT and UPDATED; tomorrow, 
we'll handle the case where we insert NEW articles into the database.

(justification: REDUNDANT and UPDATED, together, are less work than assembling
everything that needs to go into inserting a NEW article)

Our dispatch function is as follows:
--}

processArts :: Connection -> Integer -> LookupTable -> SpecialCharTable
            -> (Triage, [WPJATI]) -> MemoizedAuthors IO
processArts conn _ lk _ (REDUNDANT, infos) =
   roff conn lk INFO
       ("Ignoring " ++ show (length infos) ++ " redundant articles.")
processArts conn _ lk _ (UPDATED, infos) =
   lift (mapM_ (\wpj -> updateArts conn [wpj] >>= updateJSON conn wpj) infos) >>
   roff conn lk INFO ("Updated " ++ show (length infos) ++ " articles.")
processArts conn pidx lk spc (NEW, infos) = undefined

-- I write all the above to show the work-flow. For redundant articles, we
-- simply log that they are redundant, so we need a logging function:

roff :: Connection -> LookupTable -> Severity -> String -> MemoizedAuthors IO
roff conn severityLookup level msg = lift (roff' conn severityLookup level msg)

roff' :: Connection -> LookupTable -> Severity -> String -> IO ()
roff' conn sev lvl =
   stampIt . Entry lvl "ETL" "Y2018.M05.D09.Solution" >=> \ent ->
   if lvl > DEBUG then print ent else return () >>
   insertStampedEntries conn sev [ent]

-- For updating articles we need to define updateArts and updateJSON

-- UPDATE UPDATE UPDATE -------------------------------------------------------

-- ... updating articles is much simpler (lift-n-shift from Y2018.M02.D07):

updateArtStmt :: WPJATI -> Query
updateArtStmt (ATI _ (_, Art (Art' ix _ up _ _ _ _ _ _) pl ht tit sum) (Just (IxV idx _))) =
   let (p:arams) = mapMaybe sequence
                       [("excerpt", Just sum),
                        ("update_dt", show <$> up),
                        ("plain_text", Just pl),
                        ("html", Just ht)]
       strfn (a,b) = a ++ ('=':enquote b)
       rendered = strfn p ++ concatMap ((", " ++) . strfn) arams
   in Query (B.pack ("UPDATE article SET " ++ rendered
                   ++ " WHERE art_id=" ++ show ix ++ " returning json_id"))

enquote :: String -> String
enquote str = quote ++ convert' "LATIN1" "UTF8" str ++ quote
   where quote = "$sqs$"

-- takes a string converts it from "LATIN1" to "UTF8"
-- (hint: see Codec.Text.IConv.convert) and surrounds it with '$sqs$'

convert' :: EncodingName -> EncodingName -> String -> String
convert' src dest = BL.unpack . convert src dest . BL.pack

-- and we get the id from the Index of the IxValue of the ArticleMetaData

updateArts :: Connection -> [WPJATI] -> IO [Index]
updateArts conn arts = 

   -- it looks like, because of the nature of the return, that we have
   -- to do this as a mapM function. m'kay. This will hurt.

   concat <$> mapM (query_ conn . updateArtStmt) arts

-- we also have to update the JSON block for the article

updateJSON :: Connection -> WPJATI -> [Index] -> IO ()
updateJSON conn wpj =
   void . execute_ conn . Query . B.pack
        . intercalate "; " . map (updateJSONStmt (fst $ article wpj))

updateJSONStmt :: Value -> Index -> String
updateJSONStmt val ix =
   "UPDATE article_json SET json=" ++ enquote (BL.unpack (encodePretty val))
      ++ " WHERE id=" ++ show ix

-- and do some logging and auditing here and there, don't forget to tie in
-- author insertion using memoization, and there you go! 'SIMPLE'! ... um.

-- we'll look at processing articles that are NEW tomorrow.
