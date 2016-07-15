{-# LANGUAGE OverloadedStrings #-}

module Y2016.M07.D14.Solution where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import qualified Data.Text as T

import Network.HTTP

import Control.Logic.Frege (adjoin)
import Control.Scan.CSV
import Graph.JSON.Cypher
import Graph.Query

endpoint :: Endpoint
endpoint = "http://neo4j:flower-exhausts-occurrence@54.226.247.28:32778/"

query :: Cypher
query = "match (prob:Link)<-[:CONTAINS]-(ex:Tweet)<-[:REPLY_TO]-(ans:Tweet)"
    ++ "-[:CONTAINS]->(sol:Link) return ex.id_str, prob.url, ans.id_str,sol.url"

{--
Create a HADProblem structure (Haskell A Day) such that it contains the twitter
id of the problem and solution announcement tweets as well as the links to the
problem and solution Haskell sources.

From the CSV (or, if you're feeling your neo4j-graph-moxie, from the neo4j
graph), populate this structure.
--}

type Tweet = String
type URL = String
data TweetUrl = TU { tweetId :: Tweet, url :: URL } deriving Show

data HADProblem = HAD { prob, sol :: TweetUrl } deriving Show

populateHADProblems :: FilePath -> IO [HADProblem]
populateHADProblems url = getGraphResponse url [query] >>= \json ->
   let (Just (GR res errs)) = decode (BL.pack json)
   in  pure . map tr2Had . rows $ head res

{--
Here's a common-enough row:
TR {row = Array [String "491931488925925376",
                 String "http://lpaste.net/107955",
                 String "491982029068566528",
                 String "http://lpaste.net/107955"]}

instance FromJSON HADProblem where
   parseJSON = withArray "4 strings" $ \arr ->
      let vect = V.toList arr in
      return (HAD (pairwise (take 2 vect)) (pairwise (drop 2 vect)))

... could have worked, if Aeson didn't make so horribly complicated the
Array/Vector/List/whatever-type.
--}

tr2Had :: TableRow -> HADProblem
tr2Had (TR (Array v)) = uncurry HAD . adjoin pairwise . splitAt 2 $ V.toList v

pairwise :: [Value] -> TweetUrl
pairwise [String a,String b] = uncurry TU (adjoin T.unpack (a, b))

{-- so:
*Y2016.M07.D14.Solution> populateHADProblems (endpoint ++ transaction ) ~> probs
*Y2016.M07.D14.Solution> head probs
HAD {prob = TU {tweetId = "724772635016617984", 
                url = "http://lpaste.net/2187209441198211072"},
      sol = TU {tweetId = "725870294234177537", 
                url = "http://lpaste.net/5818908382240702464"}}
--}

{-- BONUS -----------------------------------------------------------------

From a HADProblem-structure, return the source code of either the problem or
the solution (the URL of each is part of HADProblem)
--}

type HaskellSource = String

problem, solution :: HADProblem -> IO HaskellSource
problem = pullFrom prob

{--
*Y2016.M07.D14.Solution> problem (head probs) ~>
"<!DOCTYPE HTML>\n<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">

saved it out as prob.html
--}

solution = pullFrom sol

pullFrom :: (HADProblem -> TweetUrl) -> HADProblem -> IO HaskellSource
pullFrom f = getResponseBody <=< simpleHTTP . getRequest . url . f

{--
*Y2016.M07.D14.Solution> solution (head probs) ~>
"<!DOCTYPE HTML>\n<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">

saved it out as salu.html
--}

-- Now a trick will be to extract the sources from the HTML returned, ... but
-- that is for another day.
