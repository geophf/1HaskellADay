module Y2017.M12.D29.Solution where

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

import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Simple (Connection)

-- below imports available via 1HaskellADay git repository

import Control.DList
import Store.SQL.Connection (withConnection)

import Y2017.M12.D20.Solution
import Y2017.M12.D26.Solution
import Y2017.M12.D27.Solution

{--
Let's not do this just yet ...

import Y2018.M01.D02.Solution hiding (uuid)
   -- okay, so I'm importing from the future.
--}

type Logger m a = WriterT (DList String) m a

processBlock :: Monad m => Integer -> Block -> Logger m (Maybe (DatedArticle Value))
processBlock idx = pb idx . fromJSON

say :: Monad m => String -> Logger m ()
say = tell . dl'

pb :: Monad m => Integer -> Result (DatedArticle Value) -> Logger m (Maybe (DatedArticle Value))
pb idx (Success art) = 
   say ("Parsed " ++ uuid art) >> return (Just art)
pb idx (Error err) =
   say ("Could not parse article " ++ show idx ++ ", error: " ++ err) >>
   return Nothing

elide :: Monad m => (DatedArticle Value -> Bool) -> [Block]
      -> Logger m [(Block, Maybe (DatedArticle Value))]
elide crit blocks =
   zipWithM (liftM2 fmap (,) . processBlock)   -- Bazzargh @bazzargh
            [1..] blocks >>= filterM (\(blk, mbda) ->
      case mbda of
         Nothing  -> return True    -- give unparsed blocks a free pass
         Just art -> decide crit art)

decide :: Monad m => (DatedArticle a -> Bool) -> DatedArticle a -> Logger m Bool
decide crit art = if crit art then return True
   else say (uuid art ++ " is an Associated Press article") >> return False

apArt :: DatedArticle a -> Bool
apArt art = case byline art of
   Nothing    -> True
   Just bylin -> not ("Associated Press" `isInfixOf` bylin)

-- Now: upload the blocks to the database from Y2017/M12/D20/sample.json

-- Question: why do I leave the type as Maybe DatedArticle?

-- Answer: because I have to have a one-to-one match between blocks and articles
-- so that I can assign block ids to the correct article ids, AND I also cannot
-- store blocks of AP articles, so the blocks, too, must be elided even if 
-- processing succeeds (for any AP article)

etl :: Connection -> FilePath -> IO ()
etl conn json =
   readSample json >>= \pac ->
   let blocks = rows pac
       (blxArts, log) = runWriter (elide apArt blocks) in
   mapM_ putStrLn (dlToList log) >>
   insertStagedArt conn (map fst blxArts) >>= \ixs ->
   let ins = unzip (catMaybes (zipWith (\ ix (_,mbart) -> sequence (ix, mbart))
                                ixs blxArts)) in
   uncurry (insertArts conn) ins >>= \artIds ->
   putStrLn ("Wrote " ++ (show $ length artIds) ++ " articles to the database.")

{--
>>> withConnection (`etl` "Y2017/M12/D20/sample.json")
...
Parsed 14eb45b4-8875-5ba5-bccf-bafcb4fa7261
Parsed 78b33904-79d6-5631-ab66-3e9167de9e82
daeb2b25-63b1-5063-99bd-0802876a6911 is an Associated Press article
9b0a88eb-3303-54df-854c-a83fcc92bd27 is an Associated Press article
Wrote 98 articles to the database.
--}
