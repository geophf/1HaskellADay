module Y2020.M07.D20.Exercise where

{--
JSON processing!

So, this message/notification:
--}

import Data.Aeson

deleteMsg :: FilePath
deleteMsg = "Y2020/M07/D20/delete-notif.json"

-- is it valid JSON? Can you parse it

readNotification :: FromJSON a => FilePath -> IO a
readNotification msg = undefined

-- given that we can read this thingie (that's a technical term), what
-- is the text of message body? That is to day: what airspace are we deleting?

airspace :: a -> String
airspace msg = undefined

-- Of course, you'll have to structure the 'a' data-type in order to retrieve
-- the message.
