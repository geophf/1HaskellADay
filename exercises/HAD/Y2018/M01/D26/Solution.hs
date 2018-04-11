module Y2018.M01.D26.Solution where

{--
Today's Haskell problem is broken up into two-parts: we're going to be building
an app that downloads a set of articles from a REST endpoint and then upload
the articles to a PostgreSQL data store, but only those articles that are new
or updated.

How do we do the latter? We'll look at that tomorrow.

How do we do the former? Well, we'll eyeball it.

WHY will we eyeball it? eh.

* 'eh' is a precise scientific term of measure.

Or, less 'eh'-ily: we don't know what articles are new or updated, but MOST
new articles or updated articles occur within the last week, so we'll download
a week's worth of articles and then worry about which ones are new or updated
tomorrow.

TODAY's exercise is to download a week's-worth of articles from a REST endpoint.
--}

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.State (lift)

import Data.Aeson (fromJSON, Value)
import Data.Aeson.Types
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- below imports available via 1HaskellADay git repositry

import Data.Logger
import Data.Time.Stamped

-- of course, we've accessed the REST endpoint before so:

import Y2018.M01.D16.Solution (readPacket)

-- So now we need to download packets until we've gone back to a week of them:

import Y2017.M12.D20.Solution (Packet, Block, rows, next)
import Y2017.M12.D27.Solution (DatedArticle, starttime, lastupdated)

-- how many packets constitute a week's-worth of articles?

oneWeek :: StampedWriter LogEntry [Packet]
oneWeek = lift getCurrentTime >>= \(UTCTime d _) -> ow (addDays (-7) d) 0 []

{--
>>> packs <- oneWeek
>>> length packs
4
--}

ow :: Day -> Integer -> [Packet] -> StampedWriter LogEntry [Packet]
ow term offset acc =
   lift (readPacket offset) >>=
   either (processArts term acc) (errOut term offset acc)

logerr :: String -> StampedWriter LogEntry ()
logerr msg = sayIO (Entry ERROR "daily upload" "Y2018.M01.D26.Solution" msg) >>
   lift (putStrLn msg)

errOut :: Day -> Integer -> [Packet] -> String -> StampedWriter LogEntry [Packet]
errOut term offset acc err =
   logerr ("Error reading packet at " ++ show offset ++ ": " ++ err) >>
   ow term offset acc

{--
>>> packs <- oneWeek
*** Exception: Error reading packet at 200: Too Many Requests
CallStack (from HasCallStack):
  error, called at Y2018/M01/D25/Solution.hs:56:4 in main:Y2018.M01.D25.Solution

... so that works!
--}

processArts :: Day -> [Packet] -> Packet -> StampedWriter LogEntry [Packet]
processArts term acc p =
   case pa term (rows p) of
      []         -> return acc
      a@(_:_) -> ow term (fromIntegral $ next p) (p { rows = map fst a }:acc)

pa :: Day -> [Block] -> [(Block, DatedArticle Value)]
pa term = mapMaybe (pa' term . (id &&& fromJSON))

pa' :: Day -> (Block, Result (DatedArticle Value))
    -> Maybe (Block, DatedArticle Value)
pa' term (_, Error _) = Nothing
pa' term (b, Success v) =
   starttime v                                        >>=
   guard . (term <) . localDay . zonedTimeToLocalTime >>
   return (b, v)
