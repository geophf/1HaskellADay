module Y2016.M06.D24.Exercise where

import Control.Scan.CSV
import Data.Graphics.SVG
import Data.XHTML

{--
The name of the game today is charting (again) but this time we'll look at
the stochastic oscillators of a security.

info: https://en.wikipedia.org/wiki/Stochastic_oscillator

Using the data set included at this directory and at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D24/TWTR-kds.csv

chart the stochastic oscillators of the TWTR historical stock prices.
--}

chartKDs :: FilePath -> IO ()
chartKDs = undefined

-- Same hints here as for yesterday's exercise

{-- BONUS ----------- No ... wait .. -----------------------------------------

'Bonus' is from the Latin: Good. Bonus now means: "extra credit problem that's
harder.' But what if the following problem is easier? What is the opposite of
'bonus'?

'Malus,' obviously.

-- MALUS --------------------------------------------------------------------

From yesterday's problem and the problem set at 2016-06-16 on candlestick
charting, generate these charts then pull them all together into an HTML
report something like:

TWTR Analysis

Historical prices
[chart]

TWTR SMA
[chart]

TWTR EMA
[chart]

TWTR Stochastic Oscillators
[chart]

So, yeah: 'malus.'
--}

type Security = String

stockReport :: Security -> IO ()
stockReport = undefined

-- hint: the name of the security gives the filenames of the analyses
