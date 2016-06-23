module Y2016.M06.D23.Exercise where

import Control.Scan.CSV
import Data.Graphics.SVG
import Data.XHTML

{--
You have a CSV-file TWTR-smas.csv located at this director or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D23/TWTR-smas.csv

that contains the Simple Moving Averages (SMA) and closing prices for 
TWTR (Twitter) for the last three months.

Using whatever approach you like, chart these data. A sample output may look
something like the tweet referred to here:

https://twitter.com/logicalgraphs/status/743409829843243008

Chart the result (as whatever: SVG or D3 or what have you) and show your result
--}

chart :: FilePath -> IO ()
chart file = undefined

-- hint: Control.Scan.CSV has a function to read in a row of CSV.
-- hint: Data.Graphics.SVG has a line function
-- hint: We looked at drawing candlesticks in a grid last week

{-- BONUS --------------------------------------------------------------------

What? You want more?

There is also the file TWTR-emas.csv at this directory or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D23/TWTR-emas.csv

Chart that.

Context: Stock analysis oftentimes uses multiple technical indicators (in this
case the EMA12 ('Exponential Moving Average, 12 day') and EMA26 to provide hints
on a stock's trend, momentum, volitility. The SMAs and EMAs are usually used
for trending, and when the indicators cross each other, that signifies to a
potential buyer a shift in trend.
--}
