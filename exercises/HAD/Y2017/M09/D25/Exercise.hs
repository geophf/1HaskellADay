{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D25.Exercise where

import qualified Codec.Compression.GZip as GZ
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2017.M09.D22.Exercise (scanArticles, articleTextById, dir, arts)

{--
So, 'yesterday' (Friday) we were able to scan a compressed NYT article archive,
separate out each article, then return those articles as Haskell structures
(that could be output as JSON if desired). Great!

Today, we will start rending that structure into smaller pieces.

From your scanning exercise yesterday, you noticed that the articles follow this
form:

_______________________________

Document x of 1000

[title]

Author: [author]

Publication info: [publisher]

[article url]

Abstract: [article synopsis]

Links: [url separated by newlines]

Full text: [the full, well, text]

[more metadata in key: value format]

_______________________________

Okay, so, first of all, that's the first blush for the first article in the
compressed archive. Is this assessment correct, or am I trippin' on some really
bad 'shrooms?

Second of all, parse the document set into this enriched Article format.
--}

data Article =
   Art { srcId              :: Integer,
         title              :: String,
         author             :: Maybe String,
         url                :: FilePath,
         abstract, fullText :: ByteString,
         metadata           :: Map String String }
      deriving (Eq, Show)

-- n.b. I ignore some attributes in the raw text

{--
So. Toady –  ... no, wait: TODAY, because 'toady' is just wrong – take the 
articles separated out in yesterday's exercise then parse them into the 
structure given above.
--}

parseArticle :: Int -> ByteString -> Article
parseArticle artId txt = undefined

{-- BONUS -----------------------------------------------------------------

Output the above structure as JSON. Yeah, I went there. Fight me.
--}

instance ToJSON Article where
   toJSON article = undefined
