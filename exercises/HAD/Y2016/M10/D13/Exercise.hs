module Y2016.M10.D13.Exercise where

import Data.Map (Map)
import Data.Time (Day)

-- below modules available from 1HaskellADay git repository

import Control.Scan.CSV
import Data.XHTML

{--
Okay, let's say you have this person, call him Ismael, who jogs, and who 
records how far he jogs. Why does Ismael record these data? Because that's
just what joggers do: they record data.

Ismael has recorded his daily running entries into ActivityLog.csv in this
directory which is also available from this URL:

xxx

Read in these rows and answer the below questions.
--}

data Entry = SomethingThatCapturesARowOfData

-- converts a line in ActivityLog.csv to an Entry

scanEntry :: String -> Entry
scanEntry line = undefined

-- hint: you may wish to use Control.Scan.CSV.csv

-- with that scanner, read in ActivityLog.csv as a mapping from Day -> Entry

type ActivityLog = Map Day Entry

readActivityLog :: FilePath -> ActivityLog
readActivityLog file = undefined

-- So: questions

-- What is the extent of the log? That is: how many days total does it cover?

extent :: ActivityLog -> Int
extent log = undefined

-- What are the average number of days Ismael exercises each week?

µdaysPerWeek :: ActivityLog -> Float
µdaysPerWeek = undefined

-- On the days in which Ismael exercises, what are the average number of miles jogged?

µmilesPerDayExercised :: ActivityLog -> Float
µmilesPerDayExercised = undefined

-- What is Ismael's pace whilst jogging?

µpaceMPH :: ActivityLog -> Float
µpaceMPH = undefined

{--
Now, some countries, notably Britian, do NOT follow the British Imperial System.
I don't know why this is so, but people have started to make noises about a
new-fangled measuring system. It's called 'the system of measuring,' or some
boring name like that.

Convert the ActivityLog so that instead of using Miles it uses kilometers.
--}

µpaceKMpHr, µKMperDayExercised :: ActivityLog -> Float
µpaceKMpHr = undefined
µKMperDayExercised = undefined

{-- BONUS -----------------------------------------------------------------

Represent Entry as a Rasa-instance and show of your beautiful exercise log
to all your 1,000s of friends on FB.
--}

instance Rasa Entry where
  printRow = undefined

asHTMLTable :: ActivityLog -> Element
asHTMLTable = undefined
