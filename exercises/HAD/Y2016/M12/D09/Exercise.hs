module Y2016.M12.D09.Exercise where

import Data.Aeson

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

data Tweep = SomeStructureYouDefine

instance FromJSON Tweep where
   parseJSON = undefined

readTweeps :: FilePath -> IO [Tweep]
readTweeps json = undefined

friendFollowerRatio :: Tweep -> Rational
friendFollowerRatio = undefined

{-- 
The JSON for the tweeps I got from my twitter-app: twittercritter by running
the command:

geophf:twitter geophf$ 
    php info.php screen_name "RCGuerrilla,geophf,OnePeterFire,SteveSkojec,
                              VaticanCricket,vatican_en,CrisisMag,hanmariams,
                              EyeOfTheTiber,theghissilent" > ff-count.json

and placed it in this directory as ff-count.json. Have fun!
--}
