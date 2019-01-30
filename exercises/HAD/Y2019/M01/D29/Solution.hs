module Y2019.M01.D29.Solution where

{--
Yesterday we parsed in a bunch of articles and simply admired their contents.

Today, let's do something about them. We have a set of articles, some stored
'simply' (there's that word, again) as HTML, and the others stored as strings
that look 'kinda' like JSON.

Our job is to convert the 'kinda like JSON' strings in pilot-not-json.json
into HTML strings.
--}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromMaybe)

import Y2019.M01.D28.Solution

convert2HTML :: Article -> String
convert2HTML art = c2h (text art)

-- convert2HTML takes an article and returns the HTML string of the text.
-- If it's already HTML, it just returns that string, if it's in the JSON-like
-- format it converts that JSON-like string to an HTML string.

-- be careful, some of the JSON-like strings are invalid, so correct the
-- invalidity first before parsing those strings.

-- Use the work you did yesterday to load the articles for processing here.

c2h :: String -> String
c2h text@(c:_) = if c == '{' then convertJSON text else text

-- first we need to replace {...} with [...]
convertJSON :: String -> String
convertJSON (t:ext) = concat (decodeS (bracket (enquote (init ext))))

{--
First, this:

>>> (decode "[\"Hello\",\"World\"]") :: Maybe [String]
Just ["Hello","World"]
--}

bracket :: String -> String
bracket str = '[':str ++ "]"

decodeS :: String -> [String]
decodeS = fromMaybe [] . decode . BL.pack

enquote :: String -> String
enquote text@(h:uh) = if h == '"' then text else nq (break (==',') text)

-- converts "<h5>bleh</h5>,\"rest" to "\"<h5>bleh</h5>\",\"rest"

nq :: (String, String) -> String
nq (a,b) = '"':a ++ ('"':b)

{--
>>> a <- readJsonFile (exDir ++ head articleFiles)
>>> a' = map convert2HTML a
>>> mapM_ (putStrLn . take 70) a'
<h5>BUXTON, N.C.</h5><p>Three girls from Ohio who otherwise would be h


<h5>WASHINGTON</h5><p>Narrowly outnumbered in the Senate, Democrats ar
<h5>PORTSMOUTH</h5><p>A court-commissioned psychologist believes a 16-

<p>ONWARD TO OPPORTUNITY</p><p>Ron Lewis, the Chambers vice chair for
<h5>VIRGINIA BEACH</h5><p>More practice for handling emergencies. Secu
<h5>NORFOLK</h5><p>Grenah Garnett survived getting shot several times 
<h5>RICHMOND, Va.</h5><p>Republicans in Virginia's General Assembly ha

>>> b <- readJsonFile (exDir ++ head (tail articleFiles))
>>> b' = map convert2HTML b
>>> mapM_ (putStrLn . take 70) b'
<p>At first, President Donald Trump told us that he couldn’t do anythi
<p>I TOTALLY AGREE with the writer of the June 27 letter “<a href="htt
<p>Re “<a href="https://pilotonline.com/opinion/letters/article_c21f95
<p>LET’S SAY you’re the president of the United States and want to kee
<p>THE WORDS used to describe former Chesapeake Mayor Bill Ward recoun
<p>WE'RE NOW JUST four months, practically speaking, from the effectiv
<p>Re “<a href="https://pilotonline.com/opinion/letters/article_e10669
<p>Barbara Coombes, 51, had just been gardening in her father's backya
<p>Founded: 1938 in Norfolk as a Works Progress Administration grant p
<h5>VIRGINIA BEACH</h5>
<p>City officials urged residents to get rid o
--}
