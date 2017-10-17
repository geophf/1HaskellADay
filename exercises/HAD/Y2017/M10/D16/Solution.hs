{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D16.Solution where

{--
Today's exercise idea comes via @ahnqir:

Scan in skyscrapers.txt (included), parse it and, well, it's already sorted
from tallest to shortest, so ... let's fly by the seat of our pants here.

1) Graph number of highrises by cities. OOH! VISUAL!

(any graphing software you like)

The format of the file is thus:

ahnqir @ahnqir Cities with the most highrises

01. Hong Kong, China - 317
...
10. Singapore - 81

>>> words "01. Hong Kong, China - 317"
["01.","Hong","Kong,","China","-","317"]

Ugh! That last one!

>>> words "10. Singapore - 81"
["10.","Singapore","-","81"]

--}

import Control.Arrow ((>>>))

-- below imports available via 1HaskellADay

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

import Control.Scan.CSV

type City = String
type Country = String
type Population = Integer
type Skyscraper = Integer

data SkyLine =
   Stat { city :: Maybe City, country :: Country, count :: Skyscraper }
      deriving (Eq, Show)

parseSkyline :: String -> SkyLine
parseSkyline = words >>> tail >>>

-- it's actually easier to parse this from back-to-front in my mind
   
   reverse >>> \(num:dash:citystuff) ->

-- and reverse again to get the city and its State (some say: 'Country')

   let mbcitystate = break (== ',') (unwords (reverse citystuff)) in
   uncurry Stat (parseCityAndState mbcitystate) (read num)

parseCityAndState :: (String, String) -> (Maybe City, Country)
parseCityAndState (city, state) | null state = (Nothing, city)
                                | otherwise  = (Just city, drop 2 state)

-- so, the whole shootin' match:

skyfall :: FilePath -> IO [SkyLine]
skyfall file = map parseSkyline . drop 2 . lines <$> readFile file

{--
>>> head <$> skyfall "Y2017/M10/D16/skyscrapers.txt" 
Stat {city = Just "Hong Kong", country = "China", count = 317}

Anybody remember when Hong Kong was a country, or are we not allowed to say that?
--}

skySoFly :: FilePath -> [SkyLine] -> IO ()
skySoFly file = writeFile file . unlines . ("city\thighrises":)
   . map (\(Stat mbcity state n) -> 
         maybe "" (++ ", ") mbcity ++ state ++ ('\t':show n))

-- we use Mike Bostock's tutorial on D3 to create a bar-chart from this file
-- see: https://bost.ocks.org/mike/bar/3/

{--
>>> clouds <- skyfall "Y2017/M10/D16/skyscrapers.txt" 
>>> skySoFly "Y2017/M10/D16/hirises.tsv" clouds

-- and we have a bar-chart from D3js.org. I'm good with that.
--}

{--
2) Do some research:

What are the populations of the respective cities, in millions? You're going
to have to make Singapore a city, so: eh, that's on you to figure out.

Also, you have to figure out how to collect these population statistic.

Wikidata? Perhaps. Or choose your own data sources.
--}

populations :: [City] -> IO [Population]
populations cities = undefined

-- What city has the most skyscrapers per person?
-- What city has the most people per skyscraper?

per :: [(City, Skyscraper, Population)] -> [(City, Rational)]
per stats = undefined

{-- BONUS -----------------------------------------------------------------

Now that you have cities with skyscrapers and populations, chart those as
a bar chart or group chart.

Use whatever charting tool you prefer. There's some on D3js.org.
--}
