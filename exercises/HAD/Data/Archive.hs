module Data.Archive where

-- An archive is sets of information, a, that is housed on a daily basis.

import Data.Map (Map)
import Data.Time

-- below imports available via 1HaskellADay git repository

import Data.Relation
import Graph.JSON.Cypher
import Graph.Query

type Archive a = Map Day a

-- now, with archives there're a set of things you can do, now that the 
-- information is stored daily, such as traversal, or knowing what today is
-- or where to find the previous day.

data Temporal = PRIOR_DAY | CURRENT_DAY deriving Show

instance Edge Temporal where asEdge = show

data Date = Date Day | TODAY deriving (Eq, Ord, Show)

-- Date is a monoid-wrapper around Day
-- (that is to say, including mempty of TODAY)

instance Node Date where
   asNode (Date day) = "Day { date: '" ++ show day ++ "' }"
   asNode TODAY    = "TODAY { name: 'Today' }"

rels :: [Day] -> [Relation Date Temporal Date]
rels (d1:d2:rest) = Rel (Date d2) PRIOR_DAY (Date d1):rels (d2:rest)
rels _            = []

{--
*Main> take 2 (rels (days top5s)) ~>
[Rel (Date 2015-05-18) PRIOR_DAY (Date 2015-05-14),
 Rel (Date 2015-05-19) PRIOR_DAY (Date 2015-05-18)]
*Main> mkCypher "d1" "rel" "d2" (head relz) ~>
"MERGE (d1:Day { date: '2015-05-18' }) 
 MERGE (d2:Day { date: '2015-05-14' }) 
 MERGE (d1)-[rel:PRIOR_DAY]->(d2)"
--}

-- Third: Now that you have the relations, convert those to Cypher and upload
-- that set of cypher statements to the endpoint

relateDays :: Endpoint -> [Relation Date Temporal Date] -> IO String
relateDays endpoint = getGraphResponse endpoint . map (mkCypher "d1" "r" "d2")

-- ZONKS! And away! (of course, AFTER we take a snapshot of the endpoint ...)
-- Backups are lifesavers: a part of living!

{--
*Main> relateDays (_myendpoint ++ ('/': transaction)) relz ~>
"{\"results\":[{\"columns\":[],\"data\":[]},{\"columns\":[],\"data\":[]},...,
 {\"columns\":[],\"data\":[]}],\"errors\":[]}"

YAY!

Now, to kick it all off, we relate a TODAY-node to some given Day
--}

relateToday :: Endpoint -> Day -> IO String
relateToday endpoint =
   getGraphResponse endpoint
   . (:[]) . mkCypher "today" "current" "day"
   . Rel TODAY CURRENT_DAY . Date
