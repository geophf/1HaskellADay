module Y2017.M10.D12.Solution where

{--
Today, let's build an app, as in yore, that you ran on the 'command-line' as
some call in, or in the shell, as do others ... or, we can get wild-and-crazy,
and put it on a (web-)server and run it as a (web-)service.

My middle name isn't 'wild-and-crazy' for nothing.

So, today.

The context here is that I'm doing a lot of parsing of the NYT archive housed
by some Maryland company and uploading those parsed documents to a data store
for further analyses. Well, the funny thing about parsing real-world data is
that sometimes those data defy expectations, as we have seen, e.g. in
yesterday's exercise.

So, I'd like to have an app that tells me that a compressed archive parses
before I attempt to upload it to the database.

Here's the approach I have in mind. Of course, we need module Main to launch
the app, but I'd like a main' to take the command-line arguments and output
hints if those arguments go against the grain.
--}

import qualified Data.ByteString.Lazy.Char8 as BL

-- below import available via 1HaskellADay git repository

import Y2017.M09.D26.Solution (extractArticles)

-- so we have a top-level module: Main, that calls main' here with the
-- arguments:

main' :: [String] -> IO ()
main' [] =
   putStrLn (unlines ["","scanner <compressed archives>","",
                      "\tscans compressed NYT article archives."])
main' files@(_:_) = mapM_ scanner files

-- main' launches the app with args, or prints out the help message if the
-- args aren't as expected.

{--
Now, we, today, want to create an app called scanner that scans a compressed
archive and returns ... well, it just returns if the scan was successful, or
it throws an error if the scan trips up somewhere.

Hint: see Y2017.M09.D26.Exercise.extractArticles.
--}

scanner :: FilePath -> IO ()
scanner archive =
   BL.readFile archive >>=
   putStrLn . ("Extracted " ++)
            . (++ " articles from compressed archive " ++ archive)
            . show . length . extractArticles
                   

-- create the application scanner and run it against the compressed archives
-- in Y2017/M10/D03/NYT*.txt.gz. What do you get?

{--
$ scanner

scanner <compressed archives>

	scans compressed NYT article archives.

$ scanner Y2017/M10/D03/NYTOnline_09-05-17_09-10-17_ALLSecs-pt*.txt.gz
Extracted 20 articles from compressed archive Y2017/M10/D03/NYTOnline_09-05-17_09-10-17_ALLSecs-pt1.txt.gz
Extracted 100 articles from compressed archive Y2017/M10/D03/NYTOnline_09-05-17_09-10-17_ALLSecs-pt2.txt.gz
--}
