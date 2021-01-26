{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D26.Solution where

{--
At this point in the game, we have two largish wine data sets, one from 
wikidata, one from neo4j, both with their distinctive 'personalities' (?) shall
we say? Let's grasp at understanding each data set and see where we can find
similarities and where we can mend differences.

We started this work with country aliases up to now, let's add in wineries.

From each data set, return a wineries-by-country mapping.
--}

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Aeson.WikiDatum (Name, name)
import Graph.Query
import Graph.JSON.Cypher (Cypher)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D22.Solution (Wineries, Winery)
import qualified Y2021.M01.D22.Solution as WW  -- WikiWineries

type ByCountry thing = Map Country (Set thing)
type WineriesByCountry = ByCountry Winery
type Country = Name

wikiWineriesByCountry :: Wineries -> ByCountry Winery
wikiWineriesByCountry = foldr build Map.empty
   . map (name . WW.country &&& id)
   . Map.elems

build :: Ord a => (Name, a) -> ByCountry a -> ByCountry a
build (n, w) m =
   Map.insert n (maybe (Set.singleton w) (Set.insert w) (Map.lookup n m)) m

{--
>>> WW.readWineries (WW.wineriesDir ++ WW.wineriesJSON)
...
>>> let wikiwineries = it
>>> let wws = wikiWineriesByCountry wikiwineries 
>>> Map.size wws
27
>>> head $ Map.toList wws
("Argentina",fromList [Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q2829326", 
                                            name = "Al Este"},
                               country = WD {qid = "http://www.wikidata.org/entity/Q414", 
                                             name = "Argentina"},
                               location = point({ latitude: -38.8, longitude: -62.68333333 })},
                       Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q5731065", 
                                            name = "Bodega B\243rbore"}, 
                               country = WD {qid = "http://www.wikidata.org/entity/Q414", 
                                             name = "Argentina"},
                               location = point({ latitude: -31.54805556, longitude: -68.32722222 })}])
--}

-- getting wineries by country from the neo4j graph involves query-magic

countryWineriesQuery :: Cypher
countryWineriesQuery =
   T.concat ["MATCH (w:Winery)-[:FROM_PROVENCE]->()-[:PROVINCE_COUNTRY]->",
             "(c:Country) RETURN c.name, w.name"]

neo4jWineriesByCountry :: Endpoint -> IO (ByCountry Name)
neo4jWineriesByCountry url = 
   foldr build Map.empty . map (toPair . RR.row) . RR.justRows
   <$> getGraphResponse url [countryWineriesQuery]

toPair :: [Name] -> (Name, Name)
toPair = head &&& last

{--
>>> graphEndpoint 
...
>>> let url = it
>>> neo4jWineriesByCountry url
...
>>> let n4js = it
>>> head $ Map.toList n4js
("Argentina",fromList ["2 Copas","25 Lagunas","Achaval-Ferrer","Aconcagua",
                       "Aconga","Acorde\243n","Adoquin","Aguij\243n De Abeja",
                       "Aitor Ider Balbo","Alamos","Alba Roja","Alberti 154",
                       "Alfredo Roca", ...])
>>> Map.size n4js
44
--}

-- How many wineries-by-country are there in each data set? Particularly,
-- how many wineries are in "No Country [... for old men, lol]"?

{--
>>> Map.map Set.size wws
fromList [("Argentina",2),("Australia",18),("Austria",3),("Bulgaria",1),
          ("Canada",4),("Chile",1),("Denmark",1),("France",165),
          ("German Democratic Republic",1),("Germany",56),("Golan Heights",1),
          ("Greece",1),("Hungary",4),("India",1),("Israel",9),("Italy",9),
          ("Japan",6),("Moldova",2),("New Zealand",4),("North Macedonia",2),
          ("Portugal",10),("South Africa",12),("Spain",32),("Switzerland",10),
          ("Ukraine",2),("United Kingdom",1),("United States of America",247)]
>>> Map.map Set.size n4js
fromList [("Argentina",531),("Armenia",1),("Australia",474),("Austria",228),
          ("Bosnia and Herzegovina",1),("Brazil",11),("Bulgaria",24),
          ("Canada",45),("Chile",317),("China",1),("Croatia",32),("Cyprus",7),
          ("Czech Republic",3),("Egypt",1),("England",17),("France",3864),
          ("Georgia",24),("Germany",256),("Greece",99),("Hungary",41),
          ("India",1),("Israel",47),("Italy",2934),("Lebanon",6),
          ("Luxembourg",2),("Macedonia",3),("Mexico",25),("Moldova",11),
          ("Morocco",2),("New Zealand",300),("No Country",27),("Peru",2),
          ("Portugal",430),("Romania",19),("Serbia",3),("Slovakia",1),
          ("Slovenia",25),("South Africa",294),("Spain",1435),("Switzerland",4),
          ("Turkey",15),("US",5375),("Ukraine",4),("Uruguay",19)]
--}

-- we'll start looking at matching wineries across data sets, ... tomorrow.

-- also: do we want to enhance Countries and Wineries with QNames from
-- wikidata? Hm.
