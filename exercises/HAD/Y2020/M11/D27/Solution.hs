{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D27.Solution where

-- 'Yesterday,' or 'one turkey-ago' (now an unit of measure): ...

import Y2020.M11.D25.Solution     -- for Morse-type, and stuff

{--
... we laid out the type for Morse code, and I also said there was a graph
that represented morse code, but then I blithely (I love that word: 'blithely')
implemented morse code by hand from a messed up wikitable.

THE. NERVE.

But: doing things by hand is 'faster,' right?

Cha. Shur.

But what ... 'asshurances' do we have that the by-hand product is viable?

Were there any duplicates in 'yesterday's/one turkey-ago's'-table?
--}

import Data.Aeson     -- for (FromJSON, withObject, decode)
import Data.Aeson.Types   -- for Parser
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Char (toUpper)
import Data.Map (Map)
import Data.Set (Set)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows

import Data.Char (toUpper)
import Data.Aeson (decode)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

duplicates :: MorseTable -> Map [Morse] [Char]
duplicates = undefined

{-- 
Okay, so there were (weren't) duplicates. Fine. But, were all the 
representations correct? Tougher question. And that's today's Haskell problem:
to build the morse table the 'right' way: automated, from the graph.

But that means we first have to build the graph.
--}

data Letter = START_HERE | Chr Char
   deriving (Eq, Ord, Show)

instance Node Letter where
   asNode START_HERE = "START_HERE"
   asNode (Chr c) = constr "Letter" [("letter", [c])]

instance Edge Morse where
   asEdge DA = constr "DA" [("rep", "-")]
   asEdge DIT = constr "DIT" [("rep", ".")]

-- we use morse-code.png to build our graph

type MorseRel = Relation Letter Morse Letter

morseGraph :: [MorseRel]
morseGraph = 
   [Rel START_HERE DIT (Chr 'E'), Rel START_HERE DA (Chr 'T')]
   ++ (map r . words . map toUpper)
           ("e.i i.s s.h s-v i-u u.f e-a a.r r.l a-w w.p w-j t-m m-o "
         ++ "t.n n-k k-y k.c n.d d-x d.b m.g g.z g-q")
      where r (c:[h,r]) = Rel (Chr c) ((read . return) h) (Chr r)

-- okay, but how many (unique) 'letters' are there in this graph?

uniqueLetters :: [MorseRel] -> Set Letter
uniqueLetters = undefined

-- now, using the above graph, spit out the morse table ... THE SEQUEL:

morseTable :: [MorseRel] -> MorseTable
morseTable = undefined

-- in this NEW AND IMPROVED Morse table, are there any duplicate?

anyDuplicatesInTheNewAndImprovedMorseTable :: MorseTable -> Map [Morse] [Char]
anyDuplicatesInTheNewAndImprovedMorseTable = undefined

-- are there any differences between the old and unimproved table and the new one?

morseTableDifferences :: MorseTable -> MorseTable -> Set Char
morseTableDifferences = undefined

-- and, using our NEW AND IMPROVED MORSE TABLE!!!, translate the following:

konamiCode :: String
konamiCode = "Up up down down left right left right b a"

----- BONUS -------------------------------------------------------

-- Upload these relations to the graph store:

-- yeah, using cyphIt, or something like that.

{--
>>> graphEndpoint 
>>> let url = it

>>> :set -XOverloadedStrings 
>>> mapM_ (putStrLn . T.unpack . T.concat . (:[";"]) . mkCypher "a" "morse" "b") morseGraph 

MERGE (a:START_HERE) MERGE (b:Letter { letter: "E" }) MERGE (a)-[morse:DIT { rep: "." }]->(b);
MERGE (a:START_HERE) MERGE (b:Letter { letter: "T" }) MERGE (a)-[morse:DA { rep: "-" }]->(b);
MERGE (a:Letter { letter: "E" }) MERGE (b:Letter { letter: "I" }) MERGE (a)-[morse:DIT { rep: "." }]->(b);
MERGE (a:Letter { letter: "I" }) MERGE (b:Letter { letter: "S" }) MERGE (a)-[morse:DIT { rep: "." }]->(b);
... blah blah blah

So, now, to get the graph daters out:

>>> graphEndpoint
>>> let url = it
>>> getGraphResponse url ["match p=(START_HERE)-[*]->(:Letter { letter: 'R' }) return p"]
>>> let ans = it
>>> let json = BL.pack ans
>>> let (Just (GR res errs)) = (decode json) :: Maybe GraphResults
>>> mapM_ print (rows $ head res)
TR {row = Array [Array [Object (fromList []),
                        Object (fromList [("rep",String ".")]),
                        Object (fromList [("letter",String "E")]),
                        Object (fromList [("rep",String "-")]),
                        Object (fromList [("letter",String "A")]),
                        Object (fromList [("rep",String ".")]),
                        Object (fromList [("letter",String "R")])]]}

So, for R, the morse representation is .-. and we get that by extracting
the "rep" values from the above.

Now, we can get all paths from :START_HERE to some letter, but, as you see
above, subpaths are embedded in longer paths, so all we need are the terminal
paths, ... but, to find terminal paths, don't we need to explore all paths?

Hm. It's a conundrum.

Actually, ... we can query the graph for the terminals:

>>> getGraphResponse url ["match ()-[]->(n) where not (n)-[]->() return n.letter"]
>>> let ans = it
>>> concat $ map (head . row) ((justRows ans) :: [TableRow [String]])
"HVFLPJOYCXBZQ"

With these terminals, we can get all letters and their encodings with a set
of cypher queries:
--}

cyphQuery :: Text
cyphQuery = T.concat ["match ()-[]->(n) where not (n)-[]->() ",
          "with n.letter as ltr match p=(:START_HERE)-[*]->(l:Letter) ",
          "where l.letter in ltr return p"]

{--
>>> getGraphResponse url [cyphQuery]
>>> let ans = BL.pack it
>>> let (Just (GR res errs)) = (decode ans) :: Maybe GraphResults
>>> let paths =  map (derefArr' . derefArr . row) . rows $ head res
>>> length paths
13
>>> head paths
[Object (fromList []),
   Object (fromList [("rep",String ".")]),
 Object (fromList [("letter",String "E")]),
   Object (fromList [("rep",String ".")]),
 Object (fromList [("letter",String "I")]),
   Object (fromList [("rep",String ".")]),
 Object (fromList [("letter",String "S")]),
   Object (fromList [("rep",String ".")]),
 Object (fromList [("letter",String "H")])]

So, now we need to decode that. Yay!
--}

type MorsePair = (Morse, Letter)

data Path = Path [MorsePair]
   deriving (Eq, Show)

{--
This is what a path-row looks like:

[[{},{"rep":"."},{"letter":"E"},{"rep":"."},{"letter":"I"},{"rep":"."},
 {"letter":"S"},{"rep":"."},{"letter":"H"}]]

where the first {} represents (:START_HERE).
--}

instance FromJSON Path where
   parseJSON = withArray "morse code" $ pathify . unarray . V.toList

unarray :: [Value] -> Parser [Value]
unarray = withArray "an array of arrays" (return . V.toList) . head

{--
nupe
unarray :: [Value] -> [Value]
unarray = V.toList . head
--}

pathify :: Parser [Value] -> Parser Path
pathify pvs = pvs >>= pathy' [] . tail

{--
nupe
doSomethingHereToConvertAValue :: Value -> [Value]
doSomethingHereToConvertAValue = undefined
--}

pathy' :: [MorsePair] -> [Value] -> Parser Path
pathy' path [] = return (Path $ reverse path)
pathy' acc (a:b:rest) =
   parseJSON a >>= \m ->
   parseJSON b >>= \l ->
{--
  withObject "morse" (\v -> v .: "rep") a >>= \m ->
                        withObject "letter" (\v -> v .: "letter") b >>= \l ->
--}
   pathy' ((m, l):acc) rest 

instance FromJSON Morse where
   parseJSON = withObject "da-dit" $ \v -> v .: "rep" >>= return . read

instance FromJSON Letter where
   parseJSON = withObject "ltr" $ \v -> Chr <$> v .: "letter"

theLetterH :: String
theLetterH = "[[{},{\"rep\":\".\"},{\"letter\":\"E\"},{\"rep\":\".\"},"
          ++ "{\"letter\":\"I\"},{\"rep\":\".\"},{\"letter\":\"S\"},"
          ++ "{\"rep\":\".\"},{\"letter\":\"H\"}]]"

justE :: String    -- for pathy'
justE = "[[{},{\"rep\":\".\"},{\"letter\":\"E\"}]]"

{--
>>> (decode (BL.pack justE)) :: (Maybe Path)
Just (Path [(.,Chr 'E')])

>>> (decode (BL.pack theLetterH)) :: (Maybe Path)
Just (Path [(.,Chr 'E'),(.,Chr 'I'),(.,Chr 'S'),(.,Chr 'H')])

Whew. Okay. That took a while. Arrays of arrays. Joy.

So, to get the morse codes of letters from a path:
--}

path2Morses :: Path -> [(Char, [Morse])]
path2Morses (Path p) = p2ms' [] p

p2ms' :: [Morse] -> [MorsePair] -> [(Char, [Morse])]
p2ms' _ [] = []
p2ms' acc ((da, Chr ltr):rest) = (ltr, reverse (da:acc)):p2ms' (da:acc) rest

{--
>>> maybe [] path2Morses (decode (BL.pack theLetterH))
[('E',.),('I',..),('S',...),('H',....)]

Okay, but let's choose a more serpentine one, and see if we're correct.

>>> getGraphResponse url ["match p=(:START_HERE)-[*]->(l:Letter { letter: 'Q' }) return p"]

which yields:
--}

theLetterQ :: String
theLetterQ = "[[{},{\"rep\":\"-\"},{\"letter\":\"T\"},{\"rep\":\"-\"},"
          ++ "{\"letter\":\"M\"},{\"rep\":\".\"},{\"letter\":\"G\"},"
          ++ "{\"rep\":\"-\"},{\"letter\":\"Q\"}]]"

{--
>>> maybe [] path2Morses (decode (BL.pack theLetterQ))
[('T',-),('M',--),('G',--.),('Q',--.-)]

Yup. That checks out.

So, to build the new morse table, we traverse all paths, then load the resulting
pairs into a map.
--}

newMorseTable :: [Path] -> Map Char [Morse]
newMorseTable = Map.fromList . concat . map path2Morses

{--
>>> getGraphResponse url [cyphQuery]
>>> let padme = (justRows it) :: [TableRow Path]
>>> let nmt = newMorseTable (map row padme)
>>> take 5 $ Map.toList nmt
[('A',.-),('B',-...),('C',-.-.),('D',-..),('E',.)]

>>> Map.size nmt
26

Woot!
--}
