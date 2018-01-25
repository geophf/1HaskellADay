module Y2017.M12.D29.Exercise where

{--
OOH! It's the last 1HaskellADay problem of the year! Let's go out with a BANG!

BANG!

Okay. That's accomplished. Now, today's problem.

Kent Beck had a hot-take on writing code. He said that you go from coding things
wrong "because you're stupid" to coding something that actually does what you
need it to do.

I like the 'because you're stupid'-part of his talk: it keeps me humble.

So, yesterday, because I was stupid I didn't do one necessary thing, and that
was this: Associated Press articles are NOT to be stored in the database. Why?

1. because the customer said so,
2. because they are aged out after 30 days, and we don't need no ageimacation!

(ya see what I did with that?)

So, given a set of unprocessed documents (Block values), process them, then
filter out the AP ('Associated Press') articles, storing the rest in the
database.

What are the AP articles? That, I leave as an exercise to the Haskeller.
--}

import Control.Monad.Writer

import Data.Aeson
import Database.PostgreSQL.Simple (Connection)

-- below imports available via 1HaskellADay git repository

import Control.DList (DList)

import Y2017.M12.D20.Exercise
import Y2017.M12.D26.Exercise
import Y2017.M12.D27.Exercise

type Logger m a = WriterT (DList String) m a

processBlock :: FromJSON a => Monad m => BlockParser m a
             -> Integer -> Block -> Logger m (Maybe (DatedArticle a))
processBlock processor idx block = undefined
say :: Monad m => String -> Logger m ()
say msg = undefined

-- hint: we kinda had to do that to solve D27 exercise
-- hint-hint: process the block, if you can, if not report out an error and 
-- include the index and reason for failing to process the block. Your report
-- can be either to the WriterMonad for logging or to the IO monad...for logging

type BlockParser m a =
   Integer -> Result (DatedArticle a) -> Logger m (Maybe (DatedArticle a))

elide :: FromJSON a => Monad m => BlockParser m a
      -> (DatedArticle a -> Bool) -> [Block]
      -> Logger m [(Block, Maybe (DatedArticle a))]
elide generate crit blocks = undefined

-- hint: Besides some type-handwaving, this looks familiar, fam

-- Now: upload the blocks to the database from Y2017/M12/D20/sample.json

-- Question: why do I leave the type as Maybe DatedArticle?

-- Answer: because I have to have a one-to-one match between blocks and articles
-- so that I can assign block ids to the correct article ids, AND I also cannot
-- store blocks of AP articles, so the blocks, too, must be elided even if 
-- processing succeeds (for any AP article)

etl :: Connection -> FilePath -> IO ()
etl conn json = undefined
