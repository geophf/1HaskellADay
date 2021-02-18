{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D18.Solution where

{--
We uploaded the reviews yesterday, but the unicoded wine-reviews also have an
(optional) score and an (optional) price-point. We need to upload those data
now, as well.
--}

import Data.Maybe (mapMaybe)

import qualified Data.Text as T

import Y2021.M02.D03.Solution (Review, Review(Review))

import Graph.Query (graphEndpoint, getGraphResponse)
import Graph.JSON.Cypher (showAttribs, Cypher)

import Data.Relation

import Y2021.M02.D08.Solution (extractReviews, fetchWineContext)
import qualified Y2021.M02.D15.Solution as Fixed

-- so, from a review, we need a set of attributes:

rev2attribs :: Review -> [Attribute Integer]
rev2attribs (Review _ _ _ mbsc mbpr) =
   mapMaybe sequence [("score", mbsc), ("price", mbpr)]

-- recall that these attributes are of type Maybe Int

-- now, the matcher is interesting, because we're matching the relation,
-- not the node

matchQuery :: Review -> Cypher
matchQuery rev@(Review rx wx _rev _mbsc _mbpr) =
   T.pack (unwords ["MATCH (t:Taster)-[r:RATES_WINE]->(w:Wine)",
                    "WHERE id(t) =", show rx, "AND id(w)=", show wx,
                    "SET r +=", showAttribs (rev2attribs rev)])

-- with the above, you should be able to update all the unicoded wine-reviews
-- with prices and scores.

{--
>>> snd <$> (graphEndpoint >>= fetchWineContext >>= extractReviews (Fixed.thisDir ++ Fixed.fixedFile))
>>> let unis = it
>>> graphEndpoint >>= flip getGraphResponse (map matchQuery unis)
"...{\"columns\":[],\"data\":[]}],\"errors\":[]}\n"

So, now we have a set of these, completed, relations of wine-reviews with scores
and prices:

{
  "identity": 424079,
  "start": 55204,
  "end": 163100,
  "type": "RATES_WINE",
  "properties": {
"score": 94,
"review": "Aromas of menthol, star anise, clove and mature berry unfold in the 
           glass. The firmly structured palate offers mature black cherry, star 
           anise, espresso and a touch of exotic spice framed in taut, 
           fine-grained tannins that lend elegance. Drink 2020\u8211\&2035.",
"price": 105
  }
}
--}
