{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D08.Solution where

{--
One of the things about a startup is that you're learning things as you go.

One thing we got was that taking the first 140 characters of an article as its
summary isn't the way to go, but the article-summarizing tool that we have is
actually something that we can market to media companies that are doing this
by hand, ... if they're doing it at all.

So, we have this summarizing tool that, given an article's full text, outputs
a summary. Let's take those summaries and upload them to the database (with
a new data table we've created for that purpose)

First of all, the summary information is in the following format:

which is:

[1, 0, 'So the researchers would like to see more attention  from parents, from 
health care providers and from researchers  to the populations that are not 
being reached as successfully black adolescents, young women and people from 
lower socioeconomic backgrounds.I think this is a real public health success 
that hasnt actually been really celebrated, said Dr. Scott Hadland, a 
pediatrician and adolescent addiction specialist at the Grayken Center for 
Addiction Medicine at Boston Medical Center, who was a co-author of a 
commentary on the study.\nAnd its important, even while we celebrate the public 
health success we think is connected to effective messaging and good preventive 
care, to think about whether there are adolescents cut off from the potential 
benefits of anti-drinking programs.Not all youth have the same access to high 
quality care and to high-quality screening and referral services, Dr. Hadland 
said.']

So we're going to need our SQS (singly-quoted string) type, also, the second 
index is internal, the first index is the article_id.

So, here we go.
--}

import Control.Arrow ((&&&), second)
import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (inserter)

import Y2017.M11.D03.Solution  -- for SingleQuotedString type

summaryFile :: FilePath
summaryFile = "Y2017/M12/D08/index_summary_file1.txt"

-- so, the question is: how do we parse the input. We know that a bracketed
-- list is one per line, so ...

parseLine :: String -> (Integer, Maybe SingleQuotedString)
parseLine line =
   -- let's get the first number:
   let (num,_comma1:rest1) = break (==',') (tail line)
   -- and past the second one
       (_num1,_comma2:rest) = break (==',') (trim rest1)
   in  (read num, let str@(quote:rest') = trim (init rest) in
           case quote of
                'u' -> Nothing
                '"' -> Just (SQS (init rest')) -- see note below
                _   -> Just (read str))

-- that's a mouthful! ... does it work? Let's see!

{--
>>> sums <- map parseLine . lines <$> readFile summaryFile 
>>> length sums
72
>>> (head &&& last) sums
((1,Just (SQS {string = "So the researchers would like to see more attention...
 (68,Nothing))

yup. ... nope. On a (much) larger data set this starts to happen:

[620, 614, "They began Friday night with Bolt qualifying for the 100-meter 
semifinals by winning his heat after a shaky start and a lot of love from the 
crowd in the same stadium where he won three Olympic gold medals in 2012.He has 
really been a game-changer for the sport, said Christian Taylor, the reigning 
world and Olympic champion in the triple jump.\nJustin Gatlin, the fast-talking 
American whose comments have sometimes inspired Bolt to rise up and hit the 
weight room, is 35 and running slower than in recent seasons.So is Bolt, whose 
best time coming into London in the 100 meters is just 9.95 seconds, which came
in a labored performance last month in Monaco.[Video Usain Bolt 9.95 sweeps the 
field in the Men's 100m - IAAF Diamond League Monaco 2017 Watch on YouTube.\n
Under the new rules, Bolt would never have run in the Berlin final he had 
false-started in the semifinals.For now, his only failure to win a major 
100-meter race came when he false-started in the final of the world 
championships in Daegu, South Korea, in 2011 and was disqualified.He was 
distraught that day, ripping off his shirt and pounding a wall in frustration."]

so we have to accomodate this form, too: 'this form' being a double-quoted 
string-as-data interspersed with single-quoted strings.

Then we want to do this transformation:
--}

maybeify :: (a, Maybe b) -> Maybe (a,b)
maybeify = sequence

{--
Why do we want to do that? Because we don't want to take an action at all for
article_ids that don't have summaries, so we distribute the maybe over the whole
tuple.

Now that we have that, we upload the summaries to the database. Or, put another
way, we transform the tuples to IxValue types and simply upload those.
--}

uploadSummariesStmt :: Query
uploadSummariesStmt =
   [sql|INSERT INTO article_summary (article_id,summary) VALUES (?,?)|]

uploadSummaries :: Connection -> [IxValue String] -> IO ()
uploadSummaries conn = inserter conn uploadSummariesStmt

{-- BONUS -----------------------------------------------------------------

Write an application that snarfs in a summary file and uploads that to the
database.
--}

main' :: [String] -> IO ()
main' [summariesFile] = do
   sums <- mapMaybe (maybeify . parseLine) . lines <$> readFile summariesFile
   withConnection (flip uploadSummaries (map (uncurry IxV . second string) sums))
   putStrLn ("Uploaded " ++ show (length sums)
          ++ " article summaries to the database.")
main' _ = putStrLn (unlines ["","upsums <summary file>","","\tUploads article "
                          ++ "summaries to the database.",""])
