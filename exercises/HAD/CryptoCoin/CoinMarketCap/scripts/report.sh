LE_DATE=$(date +%Y-%m-%d)

RANKING_FILE=$COIN_MARKET_CAP_DIR/rankings/2021/coins-$LE_DATE.json
LISTING_FILE=$COIN_MARKET_CAP_DIR/listings/2021/listings-$LE_DATE.json

LIST_CMD="listings/latest?start=1&limit=5000&convert=USD"

CURL_CMD=$COIN_MARKET_CAP_DIR/scripts/curl-command.sh

cd $COIN_MARKET_CAP_DIR/scripts
$CURL_CMD cryptocurrency/map $RANKING_FILE
$CURL_CMD cryptocurrency/$LIST_CMD $LISTING_FILE
$COIN_MARKET_CAP_DIR/scripts/ranker

ghc ranker.hs
ghc report.hs

$COIN_MARKET_CAP_DIR/scripts/report

echo "done."
