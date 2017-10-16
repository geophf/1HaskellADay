{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D16.Exercise where

{--
Today's exercise idea comes via @ahnqir:

Scan in skyscrapers.txt (included), parse it and, well, it's already sorted
from tallest to shortest, so ... let's fly by the seat of our pants here.

1) Graph number of highrises by cities. OOH! VISUAL!

(any graphing software you like)

2) Do some research:

What are the populations of the respective cities, in millions? You're going
to have to make Singapore a city, so: eh, that's on you to figure out.

Also, you have to figure out how to collect these population statistic.

Wikidata? Perhaps. Or choose your own data sources.
--}

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

type City = String
type Population = Integer
type Skyscraper = Integer

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
