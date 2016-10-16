module Y2016.M10.D13.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid -- for summing entries
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)

-- below modules available from 1HaskellADay git repository

import Control.Scan.CSV
import qualified Data.MultiMap as MM
import Data.XHTML

{--
Okay, let's say you have this person, call him Ismael, who jogs, and who 
records how far he jogs. Why does Ismael record these data? Because that's
just what joggers do: they record data.

Ismael has recorded his daily running entries into ActivityLog.csv in this
directory which is also available from this URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D13/ActivityLog.csv

Read in these rows and answer the below questions.
--}

type Secs = Int
type Miles = Float
type KM = Float
type Pace d = Float
type Weight = Float

data Entry dist = Row Day Secs dist (Pace dist) Weight
   deriving (Eq, Ord, Show)

instance Fractional d => Num (Entry d) where
   Row day1 s1 d1 p1 w1 + Row day2 s2 d2 p2 w2 =
      Row day1 (s1 + s2) (d1 + d2) (µ [p1, p2]) (µ [w1, w2])

-- we actually only care about progression, so that makes Entry d more Ord-like

-- but anyway:

   (*) = undefined
   abs = undefined
   signum = undefined -- or 1
   fromInteger = undefined
   negate = undefined

-- So, with Entry d defined as a Num, do we get Sum entry <> for free?

-- nah:

instance Fractional d => Monoid (Entry d) where
   mempty = undefined
   r1 `mappend` r2 = r1 + r2

-- so, really, Entry d is just a monoid, not a number ...
-- more precisely: Entry d is a semigroupoid, as it has no empty value

lengthr :: Fractional b => [a] -> b
lengthr = fromIntegral . length

µ :: Fractional a => [a] -> a
µ = sum &&& lengthr >>> uncurry (/)

-- converts a line in ActivityLog.csv to an Entry

-- TRICKY! Embedded returns in comments!
-- TRICKY! Walking days do not contain pacing!

scanEntry :: String -> Maybe (Entry Miles)
scanEntry line =
   case csv line of
      (date:kind:_:_:secs:_:dist:pace:wt:_:_) ->
         if kind == "walk" then Nothing else
         Just (Row (read date) (read secs) (read dist) (read pace) (read wt))
      somethingElse -> Nothing

-- with that scanner, read in ActivityLog.csv as a mapping from Day -> Entry

type ActivityLog d = Map Day (Entry d)

readActivityLog :: FilePath -> IO (ActivityLog Miles)
readActivityLog =
   fmap (MM.store . foldr (uncurry MM.insert . (dayOf &&& id)) (MM.MM Map.empty id)
       . mapMaybe scanEntry . tail . lines)
   . readFile

dayOf :: Entry d -> Day
dayOf (Row d _ _ _ _) = d

{--
*Y2016.M10.D13.Solution> readActivityLog "Y2016/M10/D13/ActivityLog.csv" ~> log

n.b. entry for 2016-10-03:

*Y2016.M10.D13.Solution> log Map.! (read "2016-10-03")
Row 12037 20.79 9.653254 162.5

20 miles, even though the original spreadsheet has two entries of 10 miles each

Monoids + MultiMaps == Love'm!
--}

-- So: questions

-- What is the extent of the log? That is: how many days total does it cover?

extent :: ActivityLog d -> Integer
extent = (maximum &&& minimum >>> uncurry diffDays) . Map.keys

-- *Y2016.M10.D13.Solution> extent log ~> 73

-- What are the average number of days Ismael exercises each week?

µdaysPerWeek :: ActivityLog d -> Float
µdaysPerWeek =
   µ . map lengthr . groupBy ((==) `on` (\(y,w,d) -> w)) . map toWeekDate . Map.keys

-- *Y2016.M10.D13.Solution> µdaysPerWeek log ~> 3.6363637

-- On the days in which Ismael exercises, what are the average number of miles jogged?

distance :: Entry d -> d
distance (Row _ _ x _ _) = x

µmilesPerDayExercised :: ActivityLog Miles -> Miles
µmilesPerDayExercised = µ . map distance . Map.elems

-- *Y2016.M10.D13.Solution> µmilesPerDayExercised log ~> 8.961451

-- What is Ismael's pace whilst jogging?

pace :: Entry d -> Pace d
pace (Row _ _ _ p _) = p

µpaceMPH :: ActivityLog Miles -> Pace Miles
µpaceMPH = µ . map pace . Map.elems

-- *Y2016.M10.D13.Solution> µpaceMPH log ~> 10.9822

-- not too shabby ... for Ishmael, that is.

{--
Now, some countries, notably Britian, do NOT follow the British Imperial System.
I don't know why this is so, but people have started to make noises about a
new-fangled measuring system. It's called 'the system of measuring,' or some
boring name like that.

Convert the ActivityLog so that instead of using Miles it uses kilometers.
--}

m2km :: Miles -> KM
m2km = (1.6 *)

mph2kmphr :: Pace Miles -> Pace KM
mph2kmphr = (/ 1.6)

metricize :: Entry Miles -> Entry KM
metricize (Row d s m p w) = Row d s (m2km m) (mph2kmphr p) w

µpaceKMpHr :: ActivityLog KM -> Pace KM
µpaceKMpHr = µpaceMPH
µKMperDayExercised :: ActivityLog KM -> KM
µKMperDayExercised = µmilesPerDayExercised

{--
*Y2016.M10.D13.Solution> let logKM = Map.map metricize log
*Y2016.M10.D13.Solution> head (Map.toList log)
(2016-07-27,Row 2307 3.79 10.145119 164.0)
*Y2016.M10.D13.Solution> head (Map.toList logKM )
(2016-07-27,Row 2307 6.064 6.340699 164.0)

-- n.b. I didn't care about weight being Imperial, as you see.

*Y2016.M10.D13.Solution> µpaceKMpHr logKM ~> 6.863875
*Y2016.M10.D13.Solution> µKMperDayExercised logKM ~> 14.33832
--}

{-- BONUS -----------------------------------------------------------------

Represent Entry as a Rasa-instance and show of your beautiful exercise log
to all your 1,000s of friends on FB.
--}

instance Show d => Rasa (Entry d) where
   printRow (Row d s m p w) =
      Elt "tr" [] (E (Elt "th" [] [S $ show d]):
         map (E . Elt "td" [] . pure . S) [show m, show p, show w])

asHTMLTable :: ActivityLog KM -> Element      -- for our Metricized friends
asHTMLTable =
   tabulate [Attrib "border" "1"] [thdrs (words "date KM pace weight")]
   . Map.elems

{--
*Y2016.M10.D13.Solution> writeFile "Y2016/M10/D13/IshmaelJogging.html" 
         (show (rep (Doc [Elt "title" [] [S "Ishmael's Jogging Log"]] 
                         [asHTMLTable logKM])))

Saved in this directory as IshmaelJogging.html
--}
