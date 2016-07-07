{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher where

{--
Converts Cypher to JSON to query graph databases, then also allows the JSON
result to be converted back to something a little more haskell-y.

Rerealizes much of the misnamed Data.Relation module
--}

import Control.Arrow ((&&&))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (fromJust)

import Data.Relation

-- Okay, one view of a set of relations is a Graph.

-- One view of a graph is expressed using OpenCypher:

type Cypher = String
type Var = String

mkCypher :: (Node a, Node b, Edge rel)
         => Var -> Var -> Var -> Relation a rel b -> Cypher
mkCypher vara varrel varb r@(Rel a rel b) =
   unwords [mergeNode vara a, mergeNode varb b, mergeRel r vara varrel varb]

data CypherOp = MATCH | MERGE | CREATE deriving Show

mergeNode, match :: Node a => Var -> a -> Cypher
mergeNode = varNode MERGE
match = varNode MATCH

varNode :: Node a => CypherOp -> Var -> a -> String
varNode op var node = show op ++ " (" ++ var ++ (':' : asNode node) ++ ")"

mergeRel :: Edge rel => Relation a rel b -> Var -> Var -> Var -> Cypher
mergeRel (Rel _ rel _) vara varrel varb =
   "MERGE (" ++ vara ++ ")-[" ++ varrel ++ (':' : asEdge rel ++ "]->("
             ++ varb ++ ")")

-- Okay, now we need to convert these Cypher queries to JSON for REST calls

data Statement = AndMain Cypher

instance ToJSON Statement where
   toJSON (AndMain stmt) = object ["statement" .= stmt]

data Statements = Decl [Statement]

instance ToJSON Statements where
   toJSON (Decl sts) = object ["statements" .= sts]

cypher2JSON :: Foldable t => t Cypher -> BL.ByteString
cypher2JSON = encode . Decl . map AndMain . toList

printJSON :: Foldable t => t Cypher -> IO ()
printJSON = BL.putStrLn . cypher2JSON

saveJSON :: Foldable t => FilePath -> t Cypher -> IO ()
saveJSON file = BL.writeFile file . cypher2JSON

{--
with language extensions: TypeSynonymInstances, FlexibleInstances

instance Edge String where asEdge = id
instance Node String where asNode = id

*Data.Cypher> printJSON [mkCypher "a" "r" "b" (Rel "France" "ALLIES" "Russia")] ~>
{"statements":[
  {"statement":"MERGE (a:France) MERGE (b:Russia) MERGE (a)-[r:ALLIES]->(b)"}]}
--}

-- Super! The above translate relations to Cypher queries, then Cypher queries
-- to JSON. Now, when we get results back, we need to do the translation from
-- JSON to some serviceable Haskell structure:

data GraphResults = GR { results :: [TabledResults], errors :: [Errors] }
   deriving Show

instance FromJSON GraphResults where
   parseJSON (Object v) = GR <$> v .: "results" <*> v .: "errors"

data TabledResults = Table { columns :: [String], rows :: [TableRow] }
   deriving Show

instance FromJSON TabledResults where
   parseJSON (Object v) = Table <$> v .: "columns" <*> v .: "data"

type Errors = Object

data TableRow = TR { row :: Value } deriving Show

instance FromJSON TableRow where
   parseJSON (Object v) = TR <$> v .: "row"

{--
*Main> getGraphResponse (endpoint ++ ('/': transaction)) queryTop5shows ~> ans

see Graph.Query                   -- http://lpaste.net/6813513488191717376
for getGraphResponse

*Main> let json = BL.pack ans
*Main> let (Just (GR res errs)) = (decode json) :: Maybe GraphResults 
*Main> length res ~> 1
*Main> columns (head res) ~> ["Security","Date"]
*Main> last . rows $ head res ~> 
TR {row = Array [String "ZSPH",
                 Array [String "2015-09-10",String "2015-10-07",...]]}

... the above gets you parsed to any Cypher query to graph DaaS

Now, to translate a TableRow into your structure, you need simply translate
that Value to your structure.

e.g.:

row2Top5s :: TableRow -> Top5sAppearance
row2Top5s = parseTop5sShow . init . tail . BL.unpack . encode . row 

Then, given that, you can take the string (the untyped-JSON response) and 
convert that to a data set you can work with directly, e.g.:
--}

type QueryResult = String

{--
top5shows :: QueryResult -> [Top5sAppearance]
top5shows = map row2Top5s . rows . head . results . fromJust . decode . BL.pack

*Main> let tops = top5shows ans
*Main> head tops
Top5Show AA [2015-07-22,2015-07-08,2015-06-22,...]
*Main> last tops
Top5Show ZSPH [2015-09-10,2015-10-07,2015-10-22,2015-11-05,2015-11-06]

let's codify that:
--}

justRows :: QueryResult -> [TableRow]
justRows = rows . head . results . fromJust . decode . BL.pack
