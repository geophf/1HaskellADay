module Y2017.M11.D06.Solution where

{--
Another parsing exercise: let's parse in a CSV file fron some analysis on the
NYT article archives.
--}

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map

-- Below import available via 1HaskellADay git repository

import Control.Scan.CSV
import Store.SQL.Util.Indexed

data Value a = QRY | VAL a
   deriving (Eq, Ord, Show)

instance Read a => Read (Value a) where
   readsPrec _ str | str == "QRY" = [(QRY, "")]
                   | otherwise    =
      let ans = reads str in
      if null ans then error ("Can't read '" ++ str ++ "'")
      else ans >>= \(val, rest) -> 
           guard (null rest) >>
           return (VAL val, "")

data Score = Row { rowIdx :: Integer, title :: String, score :: Value Double }
   deriving (Eq, Ord, Show)

instance Indexed Score where
   idx (Row i _ _) = i

parseLine :: String -> [Score]
parseLine = pl . csv

pl :: [String] -> [Score]
pl [idx,title,score] =
   reads idx >>= \(id, rest) ->
   guard (null rest) >>
   reads score >>= \(scr, rest1) ->
   guard (null rest1) >>
   return (Row id (cullTicks title) scr)
pl eh = fail ("Cannot parse line " ++ show eh)

-- parse this line to a Score value:

line :: String
line = "3041,At Rally Trump Blames Media for Countrys Deepening Divisions',0.70604"

{--
>> parseLine line
[Row {idx = 3041,
      title = "At Rally  Trump Blames Media for Countrys Deepening Divisions",
      score = VAL 0.70604}]
--}

-- Now that you're parsing a line, parse the file into a map Id -> Score

scoreFile :: FilePath
scoreFile = "Y2017/M11/D06/tf_with_scores_charlottesville.csv"

-- and here's what we would like to do, but ...

readScoreFile :: FilePath -> IO (Map Integer Score)
readScoreFile =
   fmap (Map.fromList . map (idx &&& id) . concatMap parseLine . tail . lines)
   . readFile

-- The CSV file has spurious tick-marks (') sprinkled throughout the titles,
-- either at the beginning of the title, at the end, or both. Remove those
-- tick marks from the titles.

cullTicks :: String -> String
cullTicks = filter (/= '\'')

-- AND THEN read in the file. What does it look like, sorted by id?
-- What are the values of just the keys of the returned map?

{--
>>> scores <- readScoreFile scoreFile 
>>> length scores
30
>>> Map.keys scores
[1685,1733,1790,1891,2210,2256,2277,2315,2327,2340,2389,2729,2778,3041,3104,
 3112,3183,3548,3713,3798,3984,4166,6169,6252,6333,6495,6641,6858,9785,10571]
>>> (head &&& last) (Map.toList scores)
((1685,Row {idx = 1685,
            title = "Trump Tower a Home for Celebrities and Charlatans", 
            score = VAL 0.579907}),
(10571,Row {idx = 10571, title = "Trumps Scandals a List", score = VAL 0.550488}))
--}
