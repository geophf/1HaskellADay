cd $COIN_MARKET_CAP_DIR/scripts
$COIN_MARKET_CAP_DIR/scripts/curl-command.sh
$COIN_MARKET_CAP_DIR/scripts/ranker

ghc ranker.hs
ghc report.hs

$COIN_MARKET_CAP_DIR/scripts/report

echo "done."
