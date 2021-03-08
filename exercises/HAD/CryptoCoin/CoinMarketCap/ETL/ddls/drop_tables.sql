ALTER TABLE "coin_market_cap_daily" DROP CONSTRAINT IF EXISTS "coin_market_cap_daily_fk0";

ALTER TABLE "token" DROP CONSTRAINT IF EXISTS "token_fk0";

ALTER TABLE "token" DROP CONSTRAINT IF EXISTS "token_fk1";

ALTER TABLE "j_tag_coin" DROP CONSTRAINT IF EXISTS "j_tag_coin_fk0";

ALTER TABLE "j_tag_coin" DROP CONSTRAINT IF EXISTS "j_tag_coin_fk1";

ALTER TABLE "flipside_crypto_daily" DROP CONSTRAINT IF EXISTS "flipside_crypto_daily_fk0";

DROP TABLE IF EXISTS "coin_market_cap_daily";

DROP TABLE IF EXISTS "coin";

DROP TABLE IF EXISTS "token";

DROP TABLE IF EXISTS "tag";

DROP TABLE IF EXISTS "j_tag_coin";

DROP TABLE IF EXISTS "flipside_crypto_daily";

DROP TABLE IF EXISTS "score";

