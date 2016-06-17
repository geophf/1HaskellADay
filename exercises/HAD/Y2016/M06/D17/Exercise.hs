import Analytics.Trading.Data.Row     -- http://lpaste.net/109658

import Data.Graphics.SVG
import Data.XHTML (Element)

{--
Yesterday we generated a candlestick then a set of candlesticks over a set
of historic prices of a stock (TWTR). Well, great, but what does it all mean?

The eternal question.

So! Let's provide the context.

From yesterday's data set, determine the highest high and the lowest low for
a sense of scale and then, today, what we will do is provide a grid of 
x and y axes labeled with lowest and highest values for each (you can get 
fancier if you'd like) and THEN!

... well, and then we're done with today's problem.

Simple!

The data is at twtr.csv under yesterday's exercise.
--}

axes :: [Row] -> Element    -- from a set of Rows, give labeled axes in which
                            -- to chart the data
axes = undefined

-- OR: I'm just waiting for somebody to tell me: "geophf! Use this charting
-- API, dummy!" So, ... waiting ...
