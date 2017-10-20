module Control.Scan.Config where

{--
ANOTHER CUSTOMER REQUEST!

So, they like the soap bubbles from Y2017.M10.D17.Exercise BUT!

... there's always a 'but.' If you expect it, it's cool when it comes ...

BUT! They want to filter out some 'catch-all' subjects that seem to classify
articles, regardless of their topicality. If you look at the NYT article 
archive, you see that the subjects 'Social media' and 'Presidents' seem to
show up for articles, regardless of how slight the mention (if at all).

Well, we don't like clouding our analyses with noise, so, today, we're going
to construct a '.gitignore' for our charting tool, we'll call it '.chartignore'
because we're so original like that, and put it in $HOME so whoever is using
charter can configure it to ignore everything, if they so desire, without
messing up others' experience with charter.

That is, if you're on time-sharing system, like I am on my Macbook Air.

So, our .chartignore file is here at this directory (pretend it's $HOME)

Write a parser that extracts the subjects to ignore
--}

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

home :: IO FilePath
home = getEnv "HOME"

parseConfig :: FilePath -> IO (Set String)
parseConfig = fmap (Set.fromList . concatMap fetchLine . lines) . readFile

-- this function may help

fetchLine :: MonadPlus m => String -> m String
fetchLine [] = mzero
fetchLine line@(h:t) | h == '#' = mzero
                     | otherwise = return line

-- returns the line if it's not empty nor a comment

{--
>>> home >>= parseConfig . (++ "/.chartignore")
fromList ["Presidents","Social networks"]
--}

{--                             
Now that you have the subjects to ignore, we filter them out from the resulting
groups returned. Rolling this change into Y2017.M10.D17.Solution
--}

-- This, generally, parses configuration files.
