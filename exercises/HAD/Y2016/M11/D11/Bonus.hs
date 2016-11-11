module Y2016.M11.D11.Bonus where

-- available from 1HaskellADay git repository
import Y2016.M11.D08.Exercise

{--
So, in honor of me solving today's problem yesterday, let's have a little
bonus exercise today, again, because of honors and bonuses and such-like.

Given that the followers of geophf, twitter id 81457442, are captured in JSON
at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D10/geophf.json

And the followers of 1HaskellADay, gzipped at the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D08/1haskfollowersids.json.gz

which has an id_str of 2359501010 (I used users/lookup?screen_name=1HaskellADay
to confirm this).

Today's bonus problem. Read in the followers of geophf (as before) and the 
followers of 1HaskellADay (as before). Now, with these data, and using whatever
graphing tool you desire, plot the network of respective followers for each
of the examined tweeps (geophf and 1HaskellADay) and also plot or indicate
the shared followers of the two as a network.
--}

networkFollowers :: Tweeps -> Tweeps -> IO ()
networkFollowers tweep1 tweep2 = undefined

-- now imagine this network extended by the followers of those followers, but
-- that is for another day. If you are in the USA, happy Veterans Day! And
-- have a great weekend, all!
