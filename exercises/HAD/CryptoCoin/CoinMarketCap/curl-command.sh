LE_DATE=$(date +%Y-%m-%d)

curl -H "X-CMC_PRO_API_KEY: $COIN_MARKET_CAP_API_KEY" -H "Accept: application/json" -G https://pro-api.coinmarketcap.com/v1/cryptocurrency/map > rankings/2021/coins-$LE_DATE.json
