LE_DATE=$(date +%Y-%m-%d)

OUT_FILE=$COIN_MARKET_CAP_DIR/rankings/2021/coins-$LE_DATE.json

cd $COIN_MARKET_CAP_DIR/scripts
$COIN_MARKET_CAP_DIR/scripts/curl-command.sh cryptocurrency/map $OUT_FILE
$COIN_MARKET_CAP_DIR/scripts/ranker

ghc ranker.hs
ghc report.hs

$COIN_MARKET_CAP_DIR/scripts/report

echo "done."
