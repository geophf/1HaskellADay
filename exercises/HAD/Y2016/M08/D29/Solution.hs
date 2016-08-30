module Y2016.M08.D29.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.List (group)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Compression.GZip as GZ

{--
Now for something completely different.

My daughter, EM, posed this #math problem to me. Can you solve it?

WHAT IS THE NEXT NUMBER?
3
13
1113
3113
132113
?
--}

series :: [Integer]
series = map read (words "3 13 1113 3113 132113")

nextInSeries :: [Integer] -> Integer
nextInSeries = read
             . concatMap (show . length &&& pure . head >>> uncurry (++))
             . group . show . last

-- *Y2016.M08.D29.Solution> nextInSeries series ~> 1113122113

{-- BONUS -----------------------------------------------------------------

Now that you solved the above ...

... you have solved the above ...

J. M. Varner @JMVarnerBooks laments: "Not a very efficient encoding scheme ;)"

And posits that 'DEFLATE' is an effient one... whatever 'DEFLATE' is.

So, devise an efficient encoding scheme for the above series. Share.
--}

type Encoding = BL.ByteString

encode :: [Integer] -> Encoding
encode = GZ.compress . BL.pack . show

{--
*Y2016.M08.D29.Solution> encode series ~> weirdo-characters ~> zippo
... but it worked because:

*Y2016.M08.D29.Solution> GZ.decompress zippo ~> "[3,13,1113,3113,132113]"

YAY!
--}
