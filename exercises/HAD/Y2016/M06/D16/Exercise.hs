module Y2016.M06.D16.Exercise where

import Data.Time
import Analytics.Trading.Data.Row        -- http://lpaste.net/109658
import Data.Graphics.SVG
import Data.Monetary.USD                 -- http://lpaste.net/109653
import Data.XHTML

{--
A few days ago we looked at box-and-whiskers using Javascript to render it.

That was unsatifying for me on several levels.

First of all: have you looked at that stateful/dynamically-scoped/mutating
javascript code? *shudder*

Anyway, there are other ways to do it, including ways that do not require you
to start up a webserver to view an image rendered dynamically.

Let's also move from the abstract to the real. Instead of using generated,
context-less data, let's use something a little more interesting for me.

Yesterday, $TWTR (Twitter) was the darling of the markets, being the possible
'next LNKD'! (Linkedin) Let's use the 'candlesticks' charting method to see
TWTRs activity yesterday:
--}

twtr20160615 :: Row
twtr20160615 = Row (read "2016-06-15") (USD 15.71) (USD 16.44)
                   (USD 15.71) (USD 15.96) 53244100 (USD 15.96)

-- represent the above data on twitter yesterday as a 'candlestick' using
-- the high, the low, and the close as the measures

candlestick :: Row -> [Element]
candlestick = undefined

-- With the above representation, display the candlestick in a boundingbox
-- (you determine the size) and show your result as SVG.

-- (hint, showing SVG is part of the SVG module; rectangles ('candlesticks') 
-- are, too; the Show-instance of Element is XML-text-friendly)

{-- BONUS ---------------------------------------------------------------------

I've downloaded the historical prices of twitter as twtr.csv. Read in the
historical prices and show a chart of candlesticks for the most recent month
of TWTR trading.
--}

candlesticks :: FilePath -> IO ()
candlesticks = undefined
