module Y2017.M11.D06.Exercise where

{--
Another parsing exercise: let's parse in a CSV file fron some analysis on the
NYT article archives.
--}

import Control.Monad
import Data.Map (Map)

-- Below import available via 1HaskellADay git repository

import Control.Scan.CSV

data Value a = QRY | VAL a
   deriving (Eq, Ord, Show)

instance Read a => Read (Value a) where
   readsPrec = undefined

data Score = Row { idx :: Integer, title :: String, score :: Value Float }
   deriving (Eq, Ord, Show)

parseLine :: MonadPlus m => String -> m Score
parseLine line = undefined

-- parse this line to a Score value:

line :: String
line = "3041,At Rally  Trump Blames Media for Countrys Deepening Divisions',0.70604"

-- Now that you're parsing a line, parse the file into a map Id -> Score

scoreFile :: FilePath
scoreFile = "Y2017/M11/D06/tf_with_scores_charlottesville.csv"

-- and here's what we would like to do, but ...

readScoreFile :: FilePath -> Map Integer Score
readScoreFile file = undefined

-- hint: look after the title line: QRY is the score-value. Huh.

-- The CSV file has spurious tick-marks (') sprinkled throughout the titles,
-- either at the beginning of the title, at the end, or both. Remove those
-- tick marks from the titles.

cullTicks :: String -> String
cullTicks title = undefined

-- AND THEN read in the file. What does it look like, sorted by id?
