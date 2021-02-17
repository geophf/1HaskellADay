{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D17.Solution where

{--
Okay, yesterday we ALMOST saved CSV ... the unicode problem. Again.

Because Text -> String of a unicode point

\8212 -> \u8212

worked just fine

But then the Text -> String of this unicode point:

\258 -> \u258

Is incorrect.

The unicode point should be

\u0258

So, that's today's Haskell problem, or as the gecko said in "Hoodwinked!"

"Do everything you did yesterday, but this time: do it good!"
--}

import Control.Arrow ((***))

import Data.Char

import Y2021.M02.D08.Solution (extractReviews, fetchWineContext)
import qualified Y2021.M02.D15.Solution as Fixed
import Y2021.M02.D03.Solution (Review, Review(Review))

import Graph.Query (graphEndpoint)

import Control.Presentation   -- for Univ
import Control.Scan.CSV

saveUnicodeReviews :: FilePath -> [Review] -> IO ()
saveUnicodeReviews outfile =
   writeFile outfile . unlines . (header:) . map uncsv

header :: String
header = "reviewer,wine,review,score,price"

instance Univ Review where
   explode (Review rix wix rev mbs mbp) = 
      [show rix, show wix, dqshow rev, mbshow mbs, mbshow mbp]

mbshow :: Show a => Maybe a -> String
mbshow Nothing = ""
mbshow (Just a) = show a

dqshow :: Show a => a -> String
dqshow = ('"':) . dq' . tail . show

dq' :: String -> String
dq' [x] = [x] -- the trailing quote; don't process
dq' (h:h':t) = 
   let (x,y) = if h == '\\' then unicodeMode t h' else ([h], h':t)
   in  x ++ dq' y

unicodeMode :: String -> Char -> (String, String)
unicodeMode str c | c == '"' = ("\"", c:str)
                  | isDigit c = unicodify (take 3 str) c str
                  | otherwise = ("\\", c:str)

unicodify :: String -> Char -> String -> (String, String)
unicodify digs c str =
   (("\\u" ++) *** flip drop str) (ucf' digs (all isDigit digs) c)

ucf' :: String -> Bool -> Char -> (String, Int)
ucf' digs True c = (c:digs, 3)
ucf' digs False c = ('0':c:take 2 digs, 2)

{--
>>> snd <$> (graphEndpoint >>= fetchWineContext >>= extractReviews (Fixed.thisDir ++ Fixed.fixedFile))
>>> let unis = it
>>> length unis
18615
>>> saveUnicodeReviews "Y2021/M02/D17/unicoded-wine-reviews.csv" unis

remember to do something like:

$ cp Y2021/M02/D17/unicoded-wine-reviews.csv ~/Library/Application\ Support/com.Neo4j.Relate/Data/dbmss/dbms-ee96.../import 

in your shell before you load the CSV file to your graph-store.
--}
