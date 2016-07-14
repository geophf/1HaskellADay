module Y2016.M07.D14.Exercise where

import Control.Scan.CSV
import Graph.JSON.Cypher
import Graph.Query

import Network.HTTP

{--
This is interesting!

So I downloaded the entire @1HaskellADay twitter list as a neo4j graph at
http://network.graphdemos.com

Here is the log-in information so you can view the graph directly yourself:

URL: http://54.226.247.28:32778
Username: neo4j
Password: flower-exhausts-occurrence

Well, how do I publish @1HaskellADay problems and solutions?

Day X I post the problem, Day Y (which may be Day X or may be Day X+1 or can
be some other number greater than X) I post the solution, linking the solution
tweet to the problem tweet.

Both tweets contain a link to their respective Haskell code. So, to extract
a problem and solution and the links to the Haskell sources we would issue
the following Cypher query:

-- BEGIN-CYPHER --------------------------------------------------------------
match trip=(prob:Link)<-[:CONTAINS]-(exercise:Tweet)<-[:REPLY_TO]-(ans:Tweet)-[:CONTAINS]->(sol:Link)
return exercise.id AS Question, prob.url, ans.id as Answer, sol.url
limit 10
-- END-CYPHER   --------------------------------------------------------------

And from that query, I downloaded the graph data as CSV as haskell-tweets.csv
located at the following url:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M07/D14/haskell-tweets.csv

Today we're going to focus on just the extraction of the graph structure.

Create a HADProblem structure (Haskell A Day) such that it contains the twitter
id of the problem and solution announcement tweets as well as the links to the
problem and solution Haskell sources.

From the CSV (or, if you're feeling your neo4j-graph-moxie, from the neo4j
graph), populate this structure.
--}

data HADProblem = AStructureYouDeclare

populateHADProblems :: FilePath -> IO [HADProblem]
populateHADProblems = undefined

{-- BONUS -----------------------------------------------------------------

From a HADProblem-structure, return the source code of either the problem or
the solution (the URL of each is part of HADProblem)

--}

type HaskellSource = String

problem, solution :: HADProblem -> IO HaskellSource
problem = undefined
solution = undefined
