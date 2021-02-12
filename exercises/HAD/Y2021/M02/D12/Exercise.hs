{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D12.Exercise where

import Data.Char

import Data.Text
import qualified Data.Text as T

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
unirep = undefined

{--
>>> unirep "edge of jammy—in a good way."
"edge of jammy\u8212in a good way."

----- BONUS -------------------------------------------------------

Using unirep, upload all reviewed, now properly unicoded.
--}
