# CoinMarketCap analyzer

Here are the scripts and files for extracting the daily rankings from
coinmarketcap.com and then analyzing those coins.

## Setup

You need the following environmental variables established in order to run
this system:

* `COIN_MARKET_CAP_DIR` pointing to this directory; and,
* `COIN_MARKET_CAP_API_KEY` which you get by requesting an API key from 
coinmarketcap.com

To connect to your SQL database, you'll need the following environmental 
variables:

* `SQL_DAAS_SERVER_URL_ECOIN`
* `SQL_DAAS_DB_NAME_ECOIN`
* `SQL_DAAS_USERNAME_ECOIN`
* `SQL_DAAS_PASSWORD_ECOIN`
* `SQL_DAAS_SERVER_PORT_ECOIN`

## What to do

The script `scripts/report.sh` fetches the latest e-coin rankings, updates
our historical data, then reports on the Top-10 e-coins as well as new coins
today. It also formats a tweet and title for today's report.

The report generates the top-10 e-coins and also the new coins for the day
(state is maintained in rankMatrix). A sample report is here:
http://logicalgraphs.blogspot.com/2021/03/top-10-e-coins-for-2021-03-09.html

Okay! Then, after you run the report and have it all ... reported out, and
stuff, you'll want to upload the listings and rankings (and BEARS! OH, MY!)
to the SQL data-store:

```BASH
$ ghci $COIN_MARKET_CAP_DIR/ETL/SourceFileLoader.hs
ghci> go
```

It will tell you which files it uploaded and then deleted.

Then, run the Coin extractor

```BASH
$ ghci $COIN_MARKET_CAP_DIR/ETL/CoinExtract.hs
ghci> go
```

This will read the coins from the database and extract them, and their rankings,
back into the database.

## E/R D

<img src="ETL/imgs/e-coin-erd.png"/>

TODOs (besides the TODO above):

* **SHOOT!** all the CMC data are captured in listings; rewrite ETL to use
this file only and drop rankings-file from all implementations.
* download FCAS scores: ... once we have cmc_id-to-flipside_crypto_uuid mapping
* use coin-state from SQL data-store.
* check back on new coins a month later. How are they doing?
  * we need a table for new coins.

OTHER TODOs: 

* create a portfolio: positions held, money invested, current value
* create a transaction history (you know, for tax purposes)

* track coinbase coins / track binance coins
* apply analytics (WHICH ONES?) against coins
* get d3.js tools working, e.g.s:

* get cmc_id for coindesk and binance traded-coins.
* get FCAS ids for cmc_id'd coins I want to measure with FCAS
* get price, volume, and FCAS data for monitored coins

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
