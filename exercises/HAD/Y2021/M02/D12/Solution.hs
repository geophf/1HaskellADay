{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D12.Solution where

import Data.Char

import Data.Text (Text)
import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher

import Y2021.M02.D03.Solution (Review, Review(Review), RawReview, NodeIds)
import qualified Y2021.M02.D03.Solution as WR

import qualified Y2021.M02.D08.Solution as ETL

{--
We left on on inserting wine reviews with the wine-reviews with unicode points
being insert into the graph store with this result:

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

Looking at the review, we see the unicode point:

>>> filter (> 127) $ map ord (T.unpack . WR.review $ head unis)
[8212]

So, the problem is that that unicode point is not being rendered as:

\u8212

but as:

\8212

Now WHY it has the latter representation? ... the only justification I can come
up with is: to make my life difficult?

Let's undifficultize my life, then.

Today's Haskell problem. Take a Text value and have our own String 
representation of it such that unicode points are represented in a digestible
way. Then, lift that String representation back to Text, because, ya know, Text
is supposed to handle that correctly? Because that's what Text is for.

Stupid Text.
--}

sampleText :: Text
sampleText = "edge of jammy—in a good way."

unirep :: Text -> Text
unirep = T.pack . concat . map toUnirep . T.unpack

toUnirep :: Char -> String
toUnirep = tur . ord

tur :: Int -> String
tur c | c < 128 = return (chr c)
      | otherwise = "\\u" ++ fourChar c

fourChar :: Int -> String
fourChar c = reverse (take 4 (reverse ("0" ++ show c)))

{--
>>> unirep "edge of jammy—in a good way."
"edge of jammy\\u8212in a good way."

>>> putStrLn (T.unpack it)
edge of jammy\u8212in a good way.

----- BONUS -------------------------------------------------------

Using unirep, upload all reviewed, now properly unicoded.
--}

translateReview :: Review -> Review
translateReview r@(Review _ _ rev _ _) =
   r { WR.review = unirep rev }

-- now, for the Show-instance, I have to translate "\\" to "\"

data T' = T' Text

instance Show T' where
   show (T' t) = fst $ foldr mbShowChr ("", False) (show $ T.unpack t)

mbShowChr :: Char -> (String, Bool) -> (String, Bool)
mbShowChr c (s, b) = if b && c == '\\' then (s, False) else (c:s, c == '\\')

uploadReviewQuery :: Review -> Cypher
uploadReviewQuery (Review rix wix rev sc mbp) =
   T.pack (unwords ["MATCH (t:Taster), (w:Wine)",
                    "WHERE id(t) =", show rix, "AND id(w)=", show wix,
                    "CREATE (t)-[:RATES_WINE { review:", show (T' rev),
                    WR.showInt "price" mbp, WR.showInt "score" sc, "} ]->(w)"])


-- Okay, but even with all this, it still ends up in the graph store as \8212
-- We have to come at this problem in a different way.

-- And the putStrLn-result may be key to the new approach.
