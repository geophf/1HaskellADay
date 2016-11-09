module Y2016.M11.D10.Exercise where

-- below import available from cabal (aeson)

import Data.Aeson

-- Below import available from 1HaskellADay git repository

import Y2016.M11.D08.Exercise

{--
Today we're going to look at one particular user of the 1HaskellADay followers,
say: 81457442, for example (la, la, la), and see which followers of THIS
1HaskellADay follower follows OTHER followers of 1HaskellADay

Today's Haskell exercise:

Read in the follower ids of 81457442 located on this directory named for its
screen_name, geophf (well, I'll be! I wonder why I picked that name!), or at
the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D10/geophf.json

as you did in yesterday's exercise.
--}

geophfsFollowers :: FilePath -> IO Tweeps
geophfsFollowers = undefined

{--
ALSO, read in the 1HaskellADay twitter-followers data set, gzipped at the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D08/1haskfollowersids.json.gz

Got it? Great.

Now, we start to build a (slightly) extended social network.

Which twitter ids do both geophf and 1HaskellADay share?
--}

sharedIds :: Tweeps -> Tweeps -> [TwitterId]
sharedIds tweepA tweepB = undefined

-- It'll start to get really interesting when we do this for a bunch of user_id
