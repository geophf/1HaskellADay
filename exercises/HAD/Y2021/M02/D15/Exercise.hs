{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D15.Exercise where

{--
Look at this directory structure!

wine-reviews-aa-aa-aa-aa
wine-reviews-aa-aa-aa-ab-aa-aa
wine-reviews-aa-aa-aa-ab-aa-ab-aa-aa
... 60 more files ...
wine-reviews-ai-ab-ab
wine-reviews-aj
wine-reviews-ak

This is me, trying to hand-parse-out where embedded newlines broke a JSON
file of wine-reviews.

There has to be a better way than this slog!

Today's Haskell problem: ingest a biggish JSON file (~800k lines) then
show line numbers and lines where embedded newlines break the file.
--}

import qualified Y2021.M02.D03.Solution as WR

type LineNumber = Integer
type Line = String

illegalJSON :: FilePath -> IO [(LineNumber, Line)]
illegalJSON = undefined

{-- BONUS ------------------------------------------------------------------

Now that you've identified the problem areas, ... fix the file.
--}

repairJSON :: FilePath -> FilePath -> IO ()
repairJSON brokenFileIn fixedFileOut = undefined
