{-# LANGUAGE TupleSections #-}

module Y2016.M12.D15.Solution where

import Control.Arrow (second, (&&&))
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (break, sortBy, isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

-- below import available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin, (<<-))
import Control.Scan.CSV (csv)
import Y2016.M12.D13.Exercise hiding (pop)
import Y2016.M12.D13.Solution (readStr)

{--
Back to the Census bureau data.

The SAIPE, Small Area Income and Poverty Estimates, is a breakdown of the 
numbers of people in poverty in the USA by Country, State, and county.

Okay. That's not an insignificant amount of data. I've gzipped the CSV into this
directory.

So, TODAY's task, is simply to read in these data then to organize these county
data rows by USState
--}

type County = String

data SAIPERow = SAIPE { pop, poverty :: PopulationCount }
   deriving Show
type SAIPEData = Map USState (Map County SAIPERow)

readSAIPERaw :: FilePath -> IO [[String]]
readSAIPERaw =
   fmap (map csv . init . tail . lines . BL.unpack . decompress) . BL.readFile

parseSAIPEData :: [[String]] -> SAIPEData
parseSAIPEData = addRows Map.empty "of being" . map line2SAIPERow

readSAIPEData :: FilePath -> IO SAIPEData
readSAIPEData = fmap parseSAIPEData . readSAIPERaw

{--
*Y2016.M12.D15.Solution> let liz =
    readSAIPEData "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" 
*Y2016.M12.D15.Solution> fmap (Map.! "Connecticut")  liz
{("Fairfield County",SAIPE {pop = 929031, poverty = 83612}),
 ("Hartford County",SAIPE {pop = 871493, poverty = 96763}),
 ("Litchfield County",SAIPE {pop = 181510, poverty = 13383}),
 ("Middlesex County",SAIPE {pop = 159572, poverty = 10744}),
 ("New Haven County",SAIPE {pop = 831687, poverty = 112801}),
 ("New London County",SAIPE {pop = 259949, poverty = 28760}),
 ("Tolland County",SAIPE {pop = 135731, poverty = 9593}),
 ("Windham County",SAIPE {pop = 111942, poverty = 12211})]

YES!
--}

line2SAIPERow :: [String] -> Either USState (County, SAIPERow)
line2SAIPERow line =
   either Left (Right .  (,line2row line) . fst) (stateContext (line !! 3))
line2SAIPERow x = error ("Could not parse line '" ++ show x ++ "'")

line2row (_yr:_pre:_id:_name:pop:poverty:_rest) =
   SAIPE (readStr pop) (readStr poverty)

-- problem for addRows: its input is an embedded context, a line is
-- EITHER the USState in which the following counties are placed OR it is
-- the county to be placed into the USState map

-- now, how to we switch contexts on a new state? That, by definition, turns
-- out to be a rather easy thing to do.

addRows :: SAIPEData -> USState -> [Either USState (County, SAIPERow)] -> SAIPEData
addRows daters _ [] = daters                          -- DONE!
addRows daters _ (Left state:rest) =  -- SWITCH STATE TO THIS!
   addRows daters state rest
addRows daters state (Right (county,datum):rest) = -- ADD ROW!
   addRows (addRow state county datum daters) state rest

-- and we're done. THAT WAS EASY!

{-- 
Welp, first we need to define what a USState is. This is not distinct,
structurally, in the data, so we are at a disadvantage, parse-wise, relying
on context. But that's: WHY THEY PAY US THE BIG BUCKS! (tm)

So, what, semi-structurally, is a State?

A State is this: a name that is not parenthsized, and the next line defines
the State's lookup in the parentheses

So, a County, then, is a composed in the line that has the

County name (State abbreviation)

And we have our multi-line, contextual, parsing problem.

Or they could've put this in some hierarchically-structured data language?

Perhaps?

Just saying.
--}

type StateAbbrev = String

parseCounty :: String -> (County, StateAbbrev)
parseCounty = second tail . adjoin init . break (== '(')

-- *Y2016.M12.D15.Solution> parseCounty "Manitowoc County (WI)" ~>
-- ("Manitowoc County","WI")  THAT WAS TOO EASY!

-- So, given the context of the parse-state of the State in the previous line

-- okay: parse-state of the State. How many of you have ever said that, huh?

-- and the state abbreviation. What is the State for the County, and do we
-- ever care after we determine it? No, so we DON'T NEED a StateAbbrev-State
-- map.

k :: a -> b -> a   -- you'd get this if you knew combinator logic
k = const          -- you'd get this, too, if you knew combinator logic.

-- *Y2016.M12.D15.Solution> k "Wisconsin" "WI" ~> "Wisconsin"

-- but we do need the transformation from (County, StateAbbrev, SAIPERow)
-- to USState -> County -> SAIPERow

addRow :: USState -> County -> SAIPERow -> SAIPEData -> SAIPEData
addRow state county row m =
   Map.insert state (Map.insert county row (mbrow state m)) m

mbrow :: USState -> SAIPEData -> Map County SAIPERow
mbrow st m = case Map.lookup st m of
   Nothing -> Map.empty
   Just mc -> mc

-- Now we need to figure out, in this line, is this a state or a county?

stateContext :: String -> Either USState (County, StateAbbrev)
stateContext cell =
   if elem '(' cell then Right (parseCounty cell) else Left cell

-- Question: Which USState has the most counties? Which USState, the least?
-- What are these Counties?

countiesFor :: SAIPEData -> USState -> [County]
countiesFor = Map.keys <<- (Map.!)

{--
*Y2016.M12.D15.Solution> fmap (flip countiesFor "Connecticut") liz
["Fairfield County","Hartford County","Litchfield County","Middlesex County",
 "New Haven County","New London County","Tolland County","Windham County"]
--}

countyCounts :: SAIPEData -> [(USState, [County])]
countyCounts saipe = sortBy (compare `on` Down . length . snd)
                   . map (id &&& countiesFor saipe) $ Map.keys saipe

-- How would you define functions to answer the above questions?

{--
*Y2016.M12.D15.Solution> fmap (head . countyCounts) liz ~>
("Texas",["Anderson County","Andrews County","Angelina County", ...])
*Y2016.M12.D15.Solution> length (snd it)                ~> 254
*Y2016.M12.D15.Solution> fmap (last . countyCounts) liz ~>
("District of Columbia",["District of Columbia"])

AHA!
--}

-- Simpsons question: How many USStates have Springfield as a county?

-- What is the expression that will get you the Simpsons answer?

{--
*Y2016.M12.D15.Solution> fmap (filter (any (isInfixOf "Springfield") . snd) . countyCounts) liz ~> []

Shoot! No Springfield counties in the USA? How sad! How about Middlesex county?

*Y2016.M12.D15.Solution> fmap (filter (any (isInfixOf "Middlesex") . snd) . countyCounts) liz ~>
[("Virginia",["Accomack County","Albemarle County", ...])]
*Y2016.M12.D15.Solution Data.List> length it ~> 4

So, four States (including Virginia) have Middlesex as a county. Which four?

*Y2016.M12.D15.Solution> fmap (map fst . filter (any (isInfixOf "Middlesex") . snd) . countyCounts) liz ~> 
["Virginia","New Jersey","Massachusetts","Connecticut"]
--}
