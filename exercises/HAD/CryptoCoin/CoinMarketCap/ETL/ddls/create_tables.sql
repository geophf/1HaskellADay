CREATE TABLE "coin_market_cap_daily" (
	"cmc_day_id" serial NOT NULL,
	"cmc_id" integer NOT NULL,
	"date" DATE not null default now(),
	"rank" integer NOT NULL,
	"num_pairs" integer,
	"max_supply" integer,
	"circulating_supply" integer,
	"total_supply" integer,
	"quote_price" double precision,
	"volume_24h" double precision,
	"percent_change_1h" double precision,
	"percent_change_24h" double precision,
	"percent_change_7d" double precision,
	"percent_change_30d" double precision,
	"percent_change_60d" double precision,
	"percent_change_90d" double precision,
	"market_cap" double precision,
	CONSTRAINT "coin_market_cap_daily_pk" PRIMARY KEY ("cmc_day_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "coin" (
	"cmc_id" serial NOT NULL,
	"name" TEXT NOT NULL,
	"symbol" TEXT NOT NULL,
	"flipsidecrypto_id" uuid,
	"is_active" bool NOT NULL DEFAULT 'true',
	"slug" TEXT,
	"first_historical_data" date,
	CONSTRAINT "coin_pk" PRIMARY KEY ("cmc_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "token" (
	"token_id" serial NOT NULL,
	"parent_id" bigint NOT NULL,
	"token_address" TEXT,
	CONSTRAINT "token_pk" PRIMARY KEY ("token_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "tag" (
	"tag_id" serial NOT NULL,
	"tag_name" TEXT NOT NULL,
	CONSTRAINT "tag_pk" PRIMARY KEY ("tag_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "j_tag_coin" (
	"j_tag_cmc_id" serial NOT NULL,
	"tag_id" bigint NOT NULL,
	"cmc_id" bigint NOT NULL,
	CONSTRAINT "j_tag_coin_pk" PRIMARY KEY ("j_tag_cmc_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "flipside_crypto_daily" (
	"fsc_day_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"date" DATE NOT NULL DEFAULT now(),
	"volume_usd" double precision NOT NULL,
	"transactions" integer NOT NULL,
	"unique_addresses" integer NOT NULL,
	"fcas" integer NOT NULL,
	"developer_behavior" integer NOT NULL,
	"user_activity" integer NOT NULL,
	"market_maturity" integer NOT NULL,
	"fcas_comparison" integer NOT NULL,
	CONSTRAINT "flipside_crypto_daily_pk" PRIMARY KEY ("fsc_day_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "score" (
	"score_id" serial NOT NULL,
	"score" integer NOT NULL,
	"grade" char NOT NULL,
	CONSTRAINT "score_pk" PRIMARY KEY ("score_id")
) WITH (
  OIDS=FALSE
);

-- I actually don't do foreign keys with SQL tables, but to each his own.

ALTER TABLE "coin_market_cap_daily" ADD CONSTRAINT "coin_market_cap_daily_fk0" FOREIGN KEY ("cmc_id") REFERENCES "coin"("cmc_id");


ALTER TABLE "token" ADD CONSTRAINT "token_fk0" FOREIGN KEY ("token_id") REFERENCES "coin"("cmc_id");
ALTER TABLE "token" ADD CONSTRAINT "token_fk1" FOREIGN KEY ("parent_id") REFERENCES "coin"("cmc_id");


ALTER TABLE "j_tag_coin" ADD CONSTRAINT "j_tag_coin_fk0" FOREIGN KEY ("tag_id") REFERENCES "tag"("tag_id");
ALTER TABLE "j_tag_coin" ADD CONSTRAINT "j_tag_coin_fk1" FOREIGN KEY ("cmc_id") REFERENCES "coin"("cmc_id");

ALTER TABLE "flipside_crypto_daily" ADD CONSTRAINT "flipside_crypto_daily_fk0" FOREIGN KEY ("flipside_crypto_id") REFERENCES "coin"("cmc_id");


