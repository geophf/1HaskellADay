{-# LANGUAGE OverloadedStrings #-}

module Y2018.M08.D03.Exercise where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)

{--
Another 'discovering the structure of JSON' Haskell exercise today.

You have the follow JSON file:
--}

exDir, newsJSON :: FilePath
exDir = "Y2018/M08/D03/"
newsJSON = "news.json"

-- 1. read in the JSON (unprettified) and write it out as pretty JSON

prettify :: FilePath -> FilePath -> IO ()
prettify unprettyIn prettyOut = undefined

-- this function may be helpful for solving prettify ...

listVals :: ByteString -> [Value]
listVals = fromJust . decode

-- 2. what structures can you see from this enprettified set?

-- ... or ... that's a tough question, let's take this approach, instead:

data Article = Art { author :: String, image :: FilePath, url :: FilePath
                     published, updated :: Date, article :: String, 
                     idx :: Integer, summary, title :: String }
   deriving (Eq, Show)

{--
The mapping from the Haskell values to the JSON is as follows:

author       ==> author_meta.display_name
image        ==> image.url
url          ==> link
article      ==> content.rendered
published    ==> date
updated      ==> modified
idx          ==> id
summary      ==> lede
title        ==> title

map the JSON to the above structure.
--}

readArticles :: FilePath -> IO [Article]
readArticles json = undefined

-- will listVals help here?

-- a. how many articles are there?

-- b. what was the max id of the article set?
