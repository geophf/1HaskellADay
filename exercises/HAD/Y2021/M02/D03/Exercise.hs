{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D03.Exercise where

import Data.Aeson
import Data.Aeson.WikiDatum (Name)

import Graph.Query
import Graph.JSON.Cypher

import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D21.Solution (Idx)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text as T

{--
The CSV file from yesterday has reviews, prices and ratings. I've pulled those,
along with the reviewer and the wine reviewed, into a JSON file here.
--}

wineReviewsDir :: FilePath
wineReviewsDir = "Y2021/M02/D03/"

wineReviewsJSON :: String -> FilePath
wineReviewsJSON post = wineReviewsDir ++ "wine-reviews" ++ post ++ ".json"

{--
But there's a problem.

I mean, yeah, sure, you can look up the node of the reviewer and the wine
reviewed by name each time, but the names aren't indexed ...

... there's a hint and a half for somebody. :/

And even if they were, if you had the indices, PERHAPS the lookup might
be faster?

Perhaps.

So, let's do that.

Given a node-type and an attribute name, return a mapping from the attribute
value (which should be of type Name) to the index.
--}

nodeMapQuery :: Name -> Name -> Cypher
nodeMapQuery node attrib = 
   T.concat ["MATCH (n:", node, ") RETURN n.", attrib,", id(n)"]

type NodeIds = Map Name Idx

nodeMap :: Endpoint -> Name -> Name -> IO NodeIds
nodeMap = undefined

{--
With the nodeMap-function, what are the mappings for the Taster-name's and for
the Wine-title's? How many tasters are there? How many wines?

Okay, let's put those mappings to good use.
--}

data RawReview = Raw { reviewer :: Maybe Name, wine :: Name,
                       reviewtxt, scoreVal :: Text, mbprice :: Maybe Text }
   deriving (Eq, Ord, Show)

instance FromJSON RawReview where
   parseJSON = undefined

sampJSONTiny, sampJSONSmol, sampJSONYuge :: FilePath
sampJSONTiny = wineReviewsJSON "-tiny"
sampJSONSmol = wineReviewsJSON "-smol"
sampJSONYuge = wineReviewsJSON ""
 
{--
>>> BL.readFile sampJSONTiny
...
>>> let vl = it
>>> (decode vl) :: Maybe [RawReview]
Just [Raw {reviewer = Just "Roger Voss", 
           wine = "Winzer Krems 2011 Edition Chremisa Sandgrube ...",
           reviewtxt = "\"Chremisa,\" the ancient name of Krems, is ...",
           scoreVal = "85", mbprice = Just "24"}]

... also, nulls in JSON are converted to Nothings here:

>>> BL.readFile sampJSONSmol
...
>>> let vl = it
>>> (decode vl) :: Maybe [RawReview]
Just [Raw {reviewer = Just "Roger Voss", 
           wine = "Winzer Krems 2011 Edition Chremisa Sandgrube 13 ...",
           reviewtxt = "\"Chremisa,\" the ancient name of Krems, is ...",
           scoreVal = "85", mbprice = Just "24"},
      Raw {reviewer = Nothing, 
           wine = "Jamieson Ranch 2011 Whiplash Chardonnay (California)", 
           reviewtxt = "$14 is a pretty good price for a Chardonnay that...",
           scoreVal = "86", mbprice = Just "14"},
      Raw {reviewer = Just "Roger Voss", 
           wine = "Ch\226teau Rieussec 2011 Carmes de Rieussec  (Sauternes)", 
           reviewtxt = "2011 was a great year for Sauternes and this ...",
           scoreVal = "90", mbprice = Nothing}]

I call this thing a RawReview because there are bunches of things wrong with
it.

Firstly, when the reviewer is not present, replace it with the Name "No Taster"

Also, score and price should be Integer values.

Also-also, the reviewer and the wine should have their indexed values from
above. Please confirm the "No Taster" index.

Give the above caveats, convert the RawReview to a review
--}

data Review = Review { reviewerIx, wineIx :: Idx, review :: Text,
                       score :: Integer, price :: Maybe Integer }
   deriving (Eq, Ord, Show)

rr2r :: NodeIds -> NodeIds -> RawReview -> Review
rr2r = undefined

{--
Now that you have the revised reviews, you can upload the properties to the
review relations.
--}

uploadReviewQuery :: Review -> Cypher
uploadReviewQuery r =
   T.pack (concat ["match (t:Taster)-[r:RATES_WINE]->(w:Wine) ",
                   "where id(t) = ", show (reviewerIx r), " and id(w) = ",
                   show (wineIx r), " SET r += { review: ", show (review r),
                   showPrice (price r), ", score: ", show (score r), " }"])

showPrice :: Maybe Integer -> String
showPrice Nothing = ""
showPrice (Just p) = ", price: " ++ show p

-- upload the reviews, prices and scores to the graph-store.
