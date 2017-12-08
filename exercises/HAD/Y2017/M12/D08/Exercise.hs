{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D08.Exercise where

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

summaryFile :: FilePath
summaryFile = "Y2017/M12/D09/index_summary_file1.txt"

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

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (inserter)

import Y2017.M11.D03.Exercise  -- for SingleQuotedString type

-- so, the question is: how do we parse the input. We know that a bracketed
-- list is one per line, so ...

parseLine :: String -> (Integer, Maybe SingleQuotedString)
parseLine line = undefined

{--
That is to say: parseLine takes the input line and returns the article_id and
the singly-quoted string. Recall: the article_id is the first index. Ignore the
second index.

But why do I say 'Maybe SingleQuotedString'?

because we can have this:

[2, 1, u'']

that is to say: the summarizer couldn't come up with a summary for the article.

Oh, and error-handling. What happens when you can't parse a line? How do you 
make error-handling useful in determining this?

Then we want to do this transformation:
--}

maybeify :: (a, Maybe b) -> Maybe (a,b)
maybeify (idx, mbstr) = undefined

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

uploadSummaries :: Connection -> IxValue String -> IO ()
uploadSummaries conn sums = undefined

{-- BONUS -----------------------------------------------------------------

Write an application that snarfs in a summary file and uploads that to the
database.
--}

main' :: [String] -> IO ()
main' [summariesFile] = undefined
