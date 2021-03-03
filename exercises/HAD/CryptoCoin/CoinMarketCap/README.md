# CoinMarketCap analyzer

Here are the scripts and files for extracting the daily rankings from
coinmarketcap.com and then analyzing those coins.

## What to do

FIRST! run the shell script curl-command.sh
NEXT! generate the updated rankMatrix from the downloaded JSON
FINALLY! in a brand-new ghc shell, run the Reporter

All of this needs to go in some execution script somewhere, so: TODO! (do that)

OR: ... Actually, idk the or, because we have to restart haskell, or recompile
haskell between steps, depende.

The report generates the top-10 e-coins and also the new coins for the day
(state is maintained in rankMatrix). A sample report is here:
http://logicalgraphs.blogspot.com/2021/03/top-10-e-coins-for-2021-03-03-with.html

TODOs (besides the TODO above):

* look-up prices for the above (top-10s and new coins)
* store new coins in state (graph-store?)
* check back on new coins a month later. How are they doing?

OTHER TODOs: 

* convert the manual process in Ranker to a script (like Reporter)
* track coindesk coins
* apply analytics (WHICH ONES?) against coins
* get d3.js tools working, e.g.s:

### d3.js has the following:

* https://observablehq.com/@d3/bollinger-bands
* https://observablehq.com/@d3/candlestick-chart
* https://observablehq.com/@fil/plateau-detection?collection=@fil/interpolation
* https://observablehq.com/@fil/hello-loess?collection=@fil/interpolation
* https://observablehq.com/@fil/gaussian-smoothing

Do we look at all e-coins as Voronoi? or Word-cloud?

* https://observablehq.com/@d3/voronoi-labels
* https://observablehq.com/@d3/word-cloud

... I have examples of running d3 under 
https://github.com/geophf/1HaskellADay/tree/master/exercises/HAD/Graph/D3
