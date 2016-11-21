module Y2016.M11.D21.Exercise where

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
wordCount = undefined

-- 2) How many days until the deadline, the end of the month

daysLeft :: IO DaysLeft  -- from 'today.' Where today is defined as today.
daysLeft = undefined

-- hint: maybe use something in the Data.Time module set.

-- 3) How many words per day must be written to complete the novel (or at least
--    the first 50,000 words of it) in the time remaining?

type DaysLeft = Integer
type WordsPerDay = Integer
type WordsWritten = Int

wordsPerDay :: DaysLeft -> WordsWritten -> WordsPerDay
wordsPerDay daysLeft wordsAlreadyWritten = undefined
