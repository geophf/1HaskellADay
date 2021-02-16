{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D08.Solution where

-- Now, let's upload the reviews!

import Y2021.M02.D03.Solution (Review, Review(Review), RawReview, NodeIds)
import qualified Y2021.M02.D03.Solution as WR

import Graph.Query
import Graph.JSON.Cypher

import Data.Aeson

import qualified Data.Text as T

import Data.List (partition)
import Data.Maybe (mapMaybe)
import Data.Char (ord)

import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as Map

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
partitionReviews =
   partition (\(Review _ _ rev _ _) -> all isASCII (T.unpack rev))

isASCII :: Char -> Bool
isASCII = (< 128) . ord

{--
>>> graphEndpoint 
...
>>> let url = it
>>> :set -XOverloadedStrings 
>>> WR.nodeMap url "Taster" "name"
fromList [("Alexander Peartree",55207),("Anna Lee C. Iijima",55209),...]
>>> let tasty = it
>>> WR.nodeMap url "Wine" "title"
fromList [...,("Xavier Flouret 2012 Pavo Real  (Ribera del Duero)",107860),...]
>>> let winy = it
>>> BL.readFile (winePart "aa-aa-aa-aa")
...
>>> let rawrevs = it
>>> let (Just rawRevs1) = (decode (BL.drop 3 rawrevs)) :: Maybe [RawReview]
>>> head rawRevs1
Raw {reviewer = Just "Roger Voss", 
     wine = Just "Winzer Krems 2011 Edition Chremisa Sandgrube 13 ...",
     reviewtxt = Just "\"Chremisa,\" the ancient name of Krems, ...",
     scoreVal = Just "85", mbprice = Just "24"}
>>> length rawRevs1 
1428
>>> let revs = mapMaybe (WR.rr2r tasty winy) rawRevs1 
>>> length revs
1428
>>> let (asc, uni) = partitionReviews revs
>>> length asc
1048
>>> length uni
380
>>> getGraphResponse url (map uploadReviewQuery asc)
"...\"errors\":[]}\n"

... okay, let's try ...-ab

>>> BL.readFile (winePart "aa-aa-aa-ab")
>>> let rawrevs = it
>>> let (Just rawRevs1) = (decode rawrevs) :: Maybe [RawReview]
>>> head rawRevs1

*** Exception: <interactive>:58:5-59: Irrefutable pattern failed for pattern Just rawRevs1

SHOOT! ... we have a long way to go here, on top of which, we have to save
out the reviews with unicode points.

But that's for tomorrow :/

... or not. The next 357 records work:

>>> BL.readFile (winePart "aa-aa-aa-ab-aa-aa")
>>> let rawrev = it
>>> let (Just rawRev1) = (decode rawrev) :: Maybe [RawReview]
>>> length rawRev1 
357

... So! We've uploaded some reviews, but are still trying to find the proverbial
needle in the haystack with the record in JSON causing a parse error.

And we still have unicode-y things to take care of, as well.
--}

type WineContext = (NodeIds, NodeIds)

fetchWineContext :: Endpoint -> IO WineContext
fetchWineContext url =
   WR.nodeMap url "Taster" "name" >>= \tasty  ->
   WR.nodeMap url "Wine" "title"  >>= \winy   ->
   return (tasty, winy)

extractReviews :: FilePath -> WineContext -> IO ([Review], [Review])
extractReviews file (tasty, winy) =
   BL.readFile file >>= \rawrev ->
   let (Just rawRevs1) = decode (BL.drop 3 rawrev)
       revs = mapMaybe (WR.rr2r tasty winy) rawRevs1
   in  return (partitionReviews revs)

grind :: Endpoint -> FilePath -> WineContext -> IO [Review]
grind url file ctx =
   extractReviews file ctx                          >>= \(asc, uni) ->
   mesg asc uni                                     >>
   getGraphResponse url (map uploadReviewQuery asc) >>
   return uni

mesg :: [a] -> [b] -> IO ()
mesg xs ys =
   pu ["Uploading", sl xs, "wine reviews."] >>
   pu ["Ignoring", sl ys, "wine reviews (due to unicode)."]
      where sl = show . length
            pu = putStrLn . unwords

{--
>>> graphEndpoint
...
>>> let url = it
>>> fetchWineContext url
fromList ...
>>> let ctx = it
>>> grind url (winePart "ai-ab-aa-aa-ab-aa-ab-ab-aa-ab-aa-aa") ctx
Uploading 5 wine reviews.
Ignoring 1 wine reviews (due to unicode).
[Review {reviewerIx = 55210, wineIx = 161815, 
         review = "This offers excellent refinement, with high-toned red-berry
                   fruit and sarsaparilla that are on the edge of jammy\8212in 
                   a good way. Noticeable yet balanced acidity and generous 
                   tannins provide a solid structure.", 
         score = Just 90, price = Just 26}]
>>> let unis = it
>>> getGraphResponse url (map uploadReviewQuery unis)
"{\"results\":[{\"columns\":[],\"data\":[]}],\"errors\":[]}"

Huh. That worked. Surprising.

But the unicode point is icky :<

match (w:Wine)-[r:RATES_WINE]-(t:Taster)
where id(w) = 161815 and id(t) = 55210
return r

{
  "identity": 413483,
  "start": 55210,
  "end": 161815,
  "type": "RATES_WINE",
  "properties": {
"score": 90,
"review": "This offers excellent refinement, with high-toned red-berry fruit 
           and sarsaparilla that are on the edge of jammy\8212in a good way. 
           Noticeable yet balanced acidity and generous tannins provide a 
           solid structure.",
"price": 26
  }
}
--}
