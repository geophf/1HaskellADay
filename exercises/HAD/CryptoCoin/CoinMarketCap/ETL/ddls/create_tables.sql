CREATE TABLE "coin_market_cap_daily_ranking" (
	"cmc_daily_ranking_id" serial NOT NULL,
	"cmc_id" integer NOT NULL,
	"date" DATE not null default now(),
	"rank" integer NOT NULL,
	"rank_src_id" bigint NOT NULL,
	CONSTRAINT "coin_market_cap_daily_ranking_pk" PRIMARY KEY ("cmc_daily_ranking_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "coin_market_cap_daily_listing" (
	"cmc_daily_listing_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"num_pairs" integer NOT NULL,
	"max_supply" integer NOT NULL,
	"circulating_supply" integer NOT NULL,
	"total_supply" integer NOT NULL,
	"quote_price" double precision NOT NULL,
	"volume_24h" double precision NOT NULL,
	"percent_change_1h" double precision NOT NULL,
	"percent_change_24h" double precision NOT NULL,
	"percent_change_7d" double precision NOT NULL,
	"percent_change_30d" double precision NOT NULL,
	"percent_change_60d" double precision NOT NULL,
	"percent_change_90d" double precision NOT NULL,
	"market_cap" double precision NOT NULL,
	"list_src_id" bigint NOT NULL,
	CONSTRAINT "coin_market_cap_daily_listing_pk" PRIMARY KEY ("cmc_daily_listing_id")
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
	"fcas_src_id" bigint NOT NULL,
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

create table "source" (
	"source_id" serial NOT NULL,
	"source_type_id" bigint NOT NULL,
	"file_name" TEXT NOT NULL,
	"for_day" DATE NOT NULL DEFAULT now(),
	"processed" bool NOT NULL DEFAULT "false",
	"file" text NOT NULL,
	CONSTRAINT "source_pk" PRIMARY KEY ("source_id")
) WITH (
  OIDS=FALSE
);

create table "source_type_lk" (
	"source_type_id" serial NOT NULL,
	"source_type" TEXT NOT NULL,
	CONSTRAINT "source_type_pk" PRIMARY KEY ("source_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO source_type_lk (source_type_id, source_type)
VALUES (1, 'RANKING'), (2, 'LISTING'), (3, 'FCAS');

CREATE TABLE "tracked_coin" (
	"tracked_coin_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"tracked_from" DATE NOT NULL,
	"tracked_price" double precision NOT NULL,
	"tracked_type_id" bigint NOT NULL,
	CONSTRAINT "tracked_coin_pk" PRIMARY KEY ("tracked_coin_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "tracked_type_lk" (
	"tracked_type_id" serial NOT NULL,
	"tracked_type" TEXT NOT NULL,
	CONSTRAINT "tracked_type_lk_pk" PRIMARY KEY ("tracked_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO tracked_type_lk (tracked_type_id, tracked_type)
VALUES (1, 'NEW'), (2, 'COINBASE'), (3, 'BINANCE'), (4, 'RANK_VARIANCE'),
       (5, 'PRICE_VARIANCE'), (6, 'MENTIONED_IN_NEWS');

CREATE TABLE "j_tracked_coin_tracked_type" (
	"jtctt_id" serial NOT NULL,
	"tracked_coin_id" bigint NOT NULL,
	"tracked_type_id" bigint NOT NULL,
	CONSTRAINT "j_tracked_coin_tracked_type_pk" PRIMARY KEY ("jtctt_id")
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
