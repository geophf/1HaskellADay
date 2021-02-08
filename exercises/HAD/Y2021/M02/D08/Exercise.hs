{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D08.Exercise where

-- Now, let's upload the reviews!

import Y2021.M02.D03.Solution (Review, Review(Review))
import qualified Y2021.M02.D03.Solution as WR

import Graph.Query
import Graph.JSON.Cypher

import qualified Data.Text as T

{--
The nodeMap is back the orginal, because we've now eliminated duplicates,
and we have to change the upload script somewhat.
--}

wineDir :: FilePath
wineDir = "Y2021/M02/D08/parts/"

winePart :: String -> FilePath
winePart x = wineDir ++ "wine-reviews-" ++ x

uploadReviewQuery :: Review -> Cypher
uploadReviewQuery (Review rix wix rev sc mbp) =
   T.pack (unwords ["MATCH (t:Taster), (w:Wine)",
                    "WHERE id(t) =", show rix, "AND id(w)=", show wix,
                    "CREATE (t)-[:RATES_WINE { review:", show rev,
                    WR.showInt "price" mbp, WR.showInt "score" sc, "} ]->(w)"])

-- Upload all the reviews, scores, and prices to the graph store.

-- How many reviews did you upload? You may want to chunk your results.

-- Also, it may be that some reviews have unicode? Filter the reviews into
-- unicode and ASCII-only reviews. What are the sizes of each of these sets of
-- reviews?

partitionReviews :: [Review] -> ([Review], [Review])
partitionReviews = undefined
