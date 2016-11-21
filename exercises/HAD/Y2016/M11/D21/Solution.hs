module Y2016.M11.D21.Solution where

import Data.Time
import Data.Time.Clock

{--
Let's say you've joined the National November Writing Month challenge to write
a novel of 50,000 words during the month of November. Let's say you've ...
'kinda' started that novel, and it's here in this directory at foo.txt or at
the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D21/foo.txt

And let's say today is today, and the novel of at least 50,000 words is due by
the end of the month, because I love deadlines: I love the sound they make as
they whoosh by!

Douglas Adams.

So, today's Haskell exercise.

1) how many words are in this novel so far
--}

wordCount :: FilePath -> IO WordsWritten
wordCount = fmap (length . words) . readFile

-- *Y2016.M11.D21.Solution> wordCount "Y2016/M11/D21/foo.txt" ~> 1798

-- 2) How many days until the deadline, the end of the month

daysLeft :: IO DaysLeft  -- from 'today.' Where today is defined as today.
daysLeft = fmap (diffDays (read "2016-12-01") . utctDay) getCurrentTime

-- *Y2016.M11.D21.Solution> daysLeft ~> 10 days left?!? ERMIGOSH!

-- 3) How many words per day must be written to complete the novel (or at least
--    the first 50,000 words of it) in the time remaining?

type DaysLeft = Integer
type WordsPerDay = Integer
type WordsWritten = Int

wordsPerDay :: DaysLeft -> WordsWritten -> WordsPerDay
wordsPerDay daysLeft = (`div` daysLeft) . fromIntegral . (50000 -)

-- *Y2016.M11.D21.Solution> wordsPerDay 10 1798 ~> 4820

-- SOMEbody has to write 5k words per day to complete #NaNoWriMo2016. *glares*
