{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.WikiDatum where

import Data.Aeson
import Data.Aeson.Types

import Data.List (stripPrefix)

import Data.Text (Text)
import qualified Data.Text as T

type Qname = Text
type Name = Text

data WikiDatum = WD { qid :: Qname, name :: Name }
   deriving (Eq, Ord, Show)

-- this is a ... 'little' (?) tricky, because we need to parse a datum by
-- its labels.

-- we introduce the *: operator

(*:) :: Object -> Text -> Parser WikiDatum
obj *: label = WD <$> obj .: label <*> obj .: (T.concat [label, "Label"])

{-- ... and with this new operator, we can do this naturally:

type Alliance = Text

data AllianceMember = AllianceMember { alliance :: WikiDatum,
                                       country :: WikiDatum }
   deriving (Eq, Ord, Show)

instance FromJSON AllianceMember where
   parseJSON = withObject "AllianceMember" $ \v -> AllianceMember
     <$> v *: "alliance" <*> v *: "country"

-- let's try this out on a sample first:

samp :: ByteString
samp = BL.pack (concat ["{\"alliance\":\"http://www.wikidata.org/entity/Q7184\",",
       "\"allianceLabel\":\"NATO\",",
       "\"country\":\"http://www.wikidata.org/entity/Q142\",",
       "\"countryLabel\":\"France\"}"])

>>> (decode samp) :: Maybe AllianceMember 
Just (AllianceMember {
        alliance = WD {qid = "http://www.wikidata.org/entity/Q7184", 
                       name = "NATO"}, 
        country  = WD {qid = "http://www.wikidata.org/entity/Q142", 
                       name = "France"}})
WOOT!

implementation from Y2020.M10.D28.Solution.hs
--}

-- now: for LongLat values

data LongLat = Point { lon :: Double, lat :: Double }
   deriving (Eq, Ord)

instance Show LongLat where
   show (Point lon lat) = "point({ latitude: " ++ show lat ++ ", longitude: "
                       ++ show lon ++ " })"

-- the Show-instance is for exporting data to neo4j graph database.

instance ToJSON LongLat where
   toJSON (Point lon lat) = object ["longitude" .= lon, "latitude" .= lat]

-- we parse: "Point(28.2125 47.8625)"
ll :: String -> [LongLat]

-- "Point" +++ = -- yeah, I'm writing a String DCG in Haskell, wheeeeeeee!

ll str = maybe [] ll' (stripPrefix "Point(" str)

ll' :: String -> [LongLat]
ll' str = reads str            >>= \(lat, r:est) ->
          reads est            >>=
          return . Point lat . fst

{--
>>> ll "Point(28.2125 47.8625)"
[point({ latitude: 47.8625, longitude: 28.2125 })]

So: ...
--}

(@:) :: Object -> Text -> Parser LongLat
obj @: label = head . ll <$> obj .: label

{--
usage, e.g.:

data CountryInfo = CI { country :: WikiDatum,
                        capital :: WikiDatum,
                        latLong :: LongLat }
   deriving (Eq, Show)

instance FromJSON CountryInfo where
    parseJSON = withObject "CountryInfo" $ \v ->
       CI <$> v *: "country" <*> v *: "capital" <*> v @: "latlongLabel"

samp :: ByteString
samp = BL.concat ["{\"country\":\"http://www.wikidata.org/entity/Q31\"",
       ",\"countryLabel\":\"Belgium\",\"capital\":\"http://www.wikidata.org/",
       "entity/Q239\",\"capitalLabel\":\"Brussels\",\"latlongLabel\":\"Point",
       "(4.351666666 50.846666666)\"}"]

>>> (decode samp) :: Maybe CountryInfo
Just (CI {country = WD {qid = "http://www.wikidata.org/entity/Q31",
                        name = "Belgium"},
          capital = WD {qid = "http://www.wikidata.org/entity/Q239",
                        name = "Brussels"},
          latLong = point({ latitude: 50.846666666, longitude: 4.351666666 })})

WOOT!

Now, to get a point from the graph-store, e.g.:

instance FromJSON Capital where
   parseJSON = withObject "Capital" $ \v ->
      Capital <$> (WD <$> v .: "qid" <*> v .: "name")
              <*> (v .: "location" >>= convertPoint)

A sample capital returned from the graph-store is:

>>> getGraphResponse url [capitalOfQuery (Country "Malaysia" Nothing [])]
{"results":[{"columns":["c"],
             "data":[{"row":[{"name":"Kuala Lumpur",
                              "location":{"type":"Point",
                                  "coordinates":[101.695277777,3.147777777],
                                  "crs":{"srid":4326,"name":"wgs-84",
                                         "type":"link",
                                         "properties":{"href":"http://spatialreference.org/ref/epsg/4326/ogcwkt/",
                                                 "type":"ogcwkt"}}},
                              "qid":"http://www.wikidata.org/entity/Q1865"}],
 "meta":[{"id":1292,"type":"node","deleted":false}]}]}],"errors":[]}

Sheesh.

Anyway, we use the following:
--}

convertPoint :: Value -> Parser LongLat
convertPoint =
   withObject  "location" $ \v ->  (v .: "coordinates") >>= convertPoint'

convertPoint' :: [Double] -> Parser LongLat
convertPoint' = return . (Point . head <*> last)
