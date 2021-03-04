LE_DATE=$(date +%Y-%m-%d)

OUT_FILE=rankings/2021/coins-$LE_DATE.json

ENDPOINT=https://pro-api.coinmarketcap.com/v1/cryptocurrency/map

cd $COIN_MARKET_CAP_DIR; \
curl -H "X-CMC_PRO_API_KEY: $COIN_MARKET_CAP_API_KEY" \
     -H "Accept: application/json" \
     -G  $ENDPOINT > $OUT_FILE

echo "Wrote latest rankings to $OUT_FILE."
