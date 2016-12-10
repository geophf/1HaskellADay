{-# LANGUAGE OverloadedStrings #-}

module Y2016.M12.D09.Solution where

import Control.Arrow ((&&&))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)

-- below imports available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Control.Presentation (laxmi)
import Data.Twitter
-- import Graph.JSON.Cypher.Read.Tweets

{--
So, there was this tweet:

RC Guerrilla @RCGuerrilla  29m29 minutes ago
#FF #Catholic @OnePeterFive @SteveSkojec @VaticanCricket @vatican_en @CrisisMag
@geophf @hanmariams @EyeOfTheTiber @theghissilent #Follow

Leading to the following response:

Math problem
Who is the most exclusive tweep below?
Who is the most profligate in their follow-backs?
What is the mean follow-back ratio?

So, including RCGuerrilla, tease out the follower/friend information from
each tweep and answer the above questions.
--}

instance FromJSON User where
   parseJSON (Object o) =
      Tweep <$> o .: "screen_name"   <*> o .: "name"
            <*> o .: "location"      <*> o .: "followers_count"
            <*> o .: "friends_count" <*> o .: "profile_image_url"

readTweeps :: FilePath -> IO [User]
readTweeps = fmap (fromJust . decode) . BL.readFile

friendFollowerRatio :: User -> Rational
friendFollowerRatio =
   uncurry (/) . adjoin fromIntegral . (following &&& followers)

{-- 
The JSON for the tweeps I got from my twitter-app: twittercritter by running
the command:

geophf:twitter geophf$ 
    php info.php screen_name "RCGuerrilla,geophf,OnePeterFire,SteveSkojec,
                              VaticanCricket,vatican_en,CrisisMag,hanmariams,
                              EyeOfTheTiber,theghissilent" > ff-count.json

and placed it in this directory as ff-count.json. Have fun!

so:

*Y2016.M12.D09.Solution> readTweeps "Y2016/M12/D09/ff-count.json" ~> ffs

(*eheh*)

*Y2016.M12.D09.Solution> length ffs ~> 9  ... YAY!

n.b.: ' ... YAY!' is not part of the return value

*Y2016.M12.D09.Solution> friendFollowerRatio (head ffs) ~> 973 % 1013

yup, yup, yup.

So, we sort on the ratio to answer the first two questions:

*Y2016.M12.D09.Solution> let ans = map (screen &&& friendFollowerRatio) ffs
*Y2016.M12.D09.Solution> sortBy (compare `on` snd) ans ~>
[("vatican_en",83 % 12114),("EyeOfTheTiber",93 % 7283),
 ("CrisisMag",413 % 12303),("theghissilent",50 % 1279),
 ("VaticanCricket",77 % 542),("hanmariams",655 % 2428),
 ("SteveSkojec",447 % 1405),("geophf",137 % 173),("RCGuerrilla",973 % 1013)]

So, vatican_en is the least follow-backer and RCGuerrilla is the most.

The mean follow-back ratio is:

*Y2016.M12.D09.Solution Data.List Data.Function> sum (map snd it) / 9 ~>
32172262422529344404132517287 % 112460315642822738295206175420

Or:

*Y2016.M12.D09.Solution> laxmi 2 it ~> "0.28"

in human-readable form... so, let's see each follow-back ratio:

*Y2016.M12.D09.Solution> map (screen &&& laxmi 2 . friendFollowerRatio) ffs ~>
[("RCGuerrilla","0.96"),("geophf","0.79"),("SteveSkojec","0.31"),
 ("VaticanCricket","0.14"),("vatican_en","0.00"),("CrisisMag","0.03"),
 ("hanmariams","0.26"),("EyeOfTheTiber","0.01"),("theghissilent","0.03")]

and, after storing (see sortBy espression, above):

*Y2016.M12.D09.Solution> mapM_ (\(a,b) -> putStrLn (a ++ (',':b))) it
vatican_en,0.00
EyeOfTheTiber,0.01
CrisisMag,0.03
theghissilent,0.03
VaticanCricket,0.14
hanmariams,0.26
SteveSkojec,0.31
geophf,0.79
RCGuerrilla,0.96

What are other measures of 'impact of YOUR twitter brand'? Retention? 
Number of likes numbers of tweets receive?
--}
