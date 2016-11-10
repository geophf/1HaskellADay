module Y2016.M11.D11.Exercise where

-- below module available from cabal (aeson)

import Data.Aeson

{--
Okay, yesterday, we saw a structure. That structure being the network of 
followers shared by 1HaskellADay and this geophf twitter account, whoever
that is.

BUT WHO IS THIS geophf-PERSON! DISCERNING HASKELLERS MUST KNOW!

Well, we'll look at that today.

Today's Haskell exercise. I did an info-pull on this geophf-person by doing
the twitter API REST-call:

https://api.twitter.com/1.1/users/lookup.json?screen_name=geophf

with appropriate OAUTH via my twitter app (thanks to @abraham for the PHP
twitter query client).

So, with this file: geophfinfo.json on this directory or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D11/geophfinfo.json

parse out the following information:

screen_name, id_str, followers_count, friends_count (the 'following' count),
and user_mentions (which structure you must also declare).

Question: the users who mention geophf, are they friends with him? We may
look at this question tomorrow. Not today.
--}

data Users = StructureOfUsersMentionedInTwitterInfoJSON
data Entities = SomeEntityStructureYouDefineWhichIncludes Users
data Status = SomeStatusStructureIncluding Entities
data TweepInfo = SomeStructureYouDefine Status

instance FromJSON TweepInfo where
   parseJSON = undefined

instance FromJSON Status where
   parseJSON = undefined

instance FromJSON Entities where
   parseJSON = undefined

instance FromJSON Users where
   parseJSON = undefined

{--
Hint: be sparse in what information you pull from the JSON. You don't need to
parse everything, just the relevant bits.

'Relevant' is a technical term, it's precise definition is: 'whatevs.'

With the above declarations, read in the JSON parsing a set of TweepInfo.
--}

readTweepInfo :: FilePath -> IO [TweepInfo]
readTweepInfo json = undefined

-- How many user mentions are in geophf's status?
