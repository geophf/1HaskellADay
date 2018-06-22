{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D22.Exercise where

import Data.Aeson

import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Y2018.M06.D19.Exercise

{--
Okay, a rather simple exercise today involving a REST call.

You have an AWS Lambda function at:
--}

type Endpoint = FilePath

rpc :: Endpoint
rpc = "https://ukbj6r6veg.execute-api.us-east-1.amazonaws.com/echo-chamber"

{--
That takes an article ID and article text and returns a set of entities, or
key phrases of that article (... this is the work that it does ... in theory).

You have a set of article ids and texts extracted from the data archive that
we did analysis on before (see above import)
--}

-- Read in the articles and send them, one at a time, to the endpoint.
-- What is the response for each article?

processArticle :: Endpoint -> Article -> IO Value
processArticle endpoint art = undefined
