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

import Data.Relation

import Data.Text (Text)
import qualified Data.Text as T

-- Okay, one view of a set of relations is a Graph.

-- One view of a graph is expressed using OpenCypher:

type Cypher = Text
type Var = Text

mkCypher :: (Node a, Node b, Edge rel)
         => Var -> Var -> Var -> Relation a rel b -> Cypher
mkCypher vara varrel varb r@(Rel a rel b) =
   T.unwords [mergeNode vara a, mergeNode varb b, mergeRel r vara varrel varb]

data CypherOp = MATCH | MERGE | CREATE deriving Show

mergeNode, match :: Node a => Var -> a -> Cypher
mergeNode = varNode MERGE
match = varNode MATCH

varNode :: Node a => CypherOp -> Var -> a -> Text
varNode op var node =
   T.concat [T.pack (show op), " (", var, ":", asNode node, ")"]

mergeRel :: Edge rel => Relation a rel b -> Var -> Var -> Var -> Cypher
mergeRel (Rel _ rel _) vara varrel varb =
   T.concat ["MERGE (", vara, ")-[", varrel, ":", asEdge rel, "]->(",
             varb, ")"]

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

-- reading in the row-results is moved to Graph.JSON.Cypher.Read.Rows
