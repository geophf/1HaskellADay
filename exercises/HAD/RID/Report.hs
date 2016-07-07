module RID.Report where

{-- A solution to the problem posted at http://lpaste.net/4139233297970495488

A logical follow-up to yesterday's problem would be to UN-JSON-ify the RID,
that is, to parse JSON to read in the RID, and then use that to analyze
documents.

But we're not going to do that today. No. Tomorrow, maybe, but today, no.

Okay, so, you were able to read in the RID and then apply it to documents for
analysis. Great! But what does that all mean? (analysis? mean? whatever)

That is to say, the Show-instance of the RIDAnalysis leaves something to be
desired. Let's amend that today.

As you've seen before from the Python report, one can present the analysis
results as something other than a big bag of pathways and some percentages, vis:
We're going to present our results as a big bag of paths and some percentages

... WITH STYLE!

And it doesn't have to look like the Python report, either (an excerpt I'll
snapshot for you to see and compare), but it can be a way that provides a
meaningful or useful summary for a consumer of RID-analysis. When somebody
is getting a report on a document, what indicies are meaningful and sufficient
for somebody to get the gist of a document?

You decide.

Then, generate a report and share it with us.

As before, the accumulated RID-resources are available at:

http://logicaltypes.blogspot.com/p/regressive-imagery-dictionary.html

A document to parse could be Pride and Prejudice by Jane Austen on
Project Gutenberg:

http://www.gutenberg.org/cache/epub/1342/pg1342.txt

(makes me wonder what documents 1 through 1341 are ... hmmmmmm)

and, as reference, the Python RID-parser is located at:

http://lemonodor.com/code/rid.py

Get to report-generating, you report-generator-writers, you!
--}

import Control.Arrow ((***))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid

import qualified Data.Bag as Bag
import RID.Analysis
import RID.Tree

type Title = String

ridReport :: Title -> RIDAnalysis -> IO ()
ridReport document (RA bag p s e) =

-- so many ways to go with this, to be sure.
-- let's spit out a report of the top-ten categorical hits

    let words = Map.toList bag
        cats  = Bag.rank (foldr (uncurry Bag.addn) Map.empty
                                (map (init *** getSum) words))
        wc    = getSum (wordcount words)
    in  putStrLn ("Regressive Imagery Dictionary analysis for " ++ document) >> 
        section "Major categories"                                           >>
        mapPrint cat (take 10 cats)                                          >>
        section "Axes"                                                       >>
        mapPrint pesDispenser (zip [PRIMARY ..] [p,s,e])                     >>
        nl                                                                   >>
        putStrLn ("Number of root-words analyzed       : " ++ show wc)

-- and your basic formatting function...

nl :: IO ()
nl = putStrLn ""

section :: String -> IO ()
section s = nl >> putStrLn s >> nl

mapPrint :: Show a => (a -> String) -> [a] -> IO ()
mapPrint f = mapM_ (putStrLn . f)

cat :: (Path, Sum Int) -> String
cat = column 70 . (intercalate ":" *** show . getSum)

pesDispenser :: Show a => (Cognition, a) -> String
pesDispenser = column 50 . (show *** show)

column :: Int -> (String, String) -> String
column x (a, b) = a ++ replicate (x - length a) ' ' ++ ": " ++ b

{--
*RID.Report> urlToTxt "http://www.gutenberg.org/cache/epub/1342/pg1342.txt" ~> pnp
*RID.Report> readinRID "RID" ~> rid
*RID.Report> let ra@(RA _ p _ _) = parseDocument rid pnp
*RID.Report> p ~> 27.53%

Yup, so:

*RID.Report> ridReport "Pride and Prejudice" ra

Regressive Imagery Dictionary analysis for Pride and Prejudice

Major categories

SECONDARY:SOCIAL_BEHAVIOR                                             : 3906
SECONDARY:ABSTRACT_TOUGHT                                             : 3859
SECONDARY:TEMPORAL_REPERE                                             : 3338
PRIMARY:REGR_KNOL:CONCRETENESS                                        : 2591
SECONDARY:INSTRU_BEHAVIOR                                             : 1608
EMOTIONS:AFFECTION                                                    : 1287
PRIMARY:SENSATION:VISION                                              : 1093
SECONDARY:RESTRAINT                                                   : 822
EMOTIONS:POSITIVE_AFFECT                                              : 697
EMOTIONS:AGGRESSION                                                   : 693

Axes

PRIMARY                                           : 27.53%
SECONDARY                                         : 56.10%
EMOTIONS                                          : 16.36%

Number of root-words analyzed       : 25631
--}
