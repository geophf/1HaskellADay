{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D22.Exercise where

import qualified Codec.Compression.GZip as GZ
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import Network.HTTP.Conduit

{--
Okay, back to something 'simple' like document processing.

We have a gzipped archive of a set of NYT (New York Times) articles as 
unstructured plain text and we need to

1) find out the underlying (implied) structure, then
2) reify Article values from the unstructured input

First problem, and it's an easy one (relatively speaking).

Read in the compressed archive and realize a set of articles from that archive.

Here's why I say it's an easy problem:
--}

data Article = Art { artId :: Int, rawText :: ByteString }
   deriving (Eq, Ord, Show)

-- so an article is just a BLOB of data, still unstructured

scanArticles :: ByteString -> [Article]
scanArticles text = undefined

-- you should get 11 articles from your scan of 

dir :: FilePath
dir = "Y2017/M09/D22/"

arts :: FilePath
arts = "NYT_articles.txt.gz"

-- and go!

{-- BONUS -----------------------------------------------------------------

convert the list of articles into JSON
--}

instance ToJSON Article where
  toJSON article = undefined

{-- BONUS-BONUS -----------------------------------------------------------

Define this function. Just for the love of it
--}

articleTextById :: [Article] -> Map Int ByteString
articleTextById arts = undefined
