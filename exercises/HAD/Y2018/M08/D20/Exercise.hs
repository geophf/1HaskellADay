module Y2018.M08.D20.Exercise where

{--
Today's haskell problem, distill:

https://pilotonline.com/news/military/nation/article_f88063e4-94d3-11e8-81a7-7310897d834f.html

to its UUID:

f88063e4-94d3-11e8-81a7-7310897d834f
--}

distill2UUID :: String -> String
distill2UUID url = undefined

-- do it for the following urls:

urls :: [FilePath]
urls = ["https://pilotonline.com/news/military/nation/article_f88063e4-94d3-11e8-81a7-7310897d834f.html",
        "https://pilotonline.com/news/local/history/article_8c086332-91b9-11e8-b080-8f148df7547f.html",
        "https://pilotonline.com/news/military/local/article_63ed2b00-9121-11e8-9662-7bbab5e2677e.html",
        "https://pilotonline.com/news/military/local/article_36228392-8c3e-11e8-9599-33072b1efcc4.html",
        "https://pilotonline.com/news/nation-world/world/article_2a53189d-3c1e-5cd5-ab9d-bbc98547764d.html",
        "https://pilotonline.com/news/military/article_c9e32f9e-9591-11e8-bccf-1bfba760ed65.html",
        "https://pilotonline.com/news/government/politics/nation/article_103e7163-64c1-5ec6-be66-38c80585fde5.html",
        "https://pilotonline.com/news/military/local/article_1d1a0a46-8ac7-11e8-943f-0baf76e7826c.html",
        "https://pilotonline.com/news/military/local/article_b3894a5c-3da9-11e8-8e6c-4b6644a76b89.html",
        "https://pilotonline.com/news/nation-world/article_57b132bf-5fe6-5dab-8379-d69e2813a32a.html",
        "https://pilotonline.com/news/military/local/article_c0ca967e-971f-11e8-a529-83f907b591c4.html",
        "https://pilotonline.com/news/military/veterans/article_304af928-9408-11e8-a6d0-93c875e5d4e4.html",
        "https://pilotonline.com/news/military/local/article_dabe6ad2-965d-11e8-9c02-d7865db31d4f.html",
        "https://pilotonline.com/news/local/history/back-in-the-day/article_09364c0c-972d-11e8-8d08-07444bfb6665.html",
        "https://pilotonline.com/news/government/nation/article_4549a986-98cd-11e8-88f9-1b5f621efee8.html",
        "https://pilotonline.com/news/military/local/article_f2bc6da2-975a-11e8-8119-4368a84d4813.html",
        "https://pilotonline.com/news/military/local/article_01a8c97a-98e4-11e8-8ac4-f7c61312dd78.html",
        "https://pilotonline.com/news/military/local/article_8145dfbe-958e-11e8-8a3a-8fdb7386c8cc.html",
        "https://pilotonline.com/news/military/local/article_f35528d2-99bf-11e8-9c48-534b44932b97.html",
        "https://pilotonline.com/news/military/local/article_5b11e72e-4e11-536e-b88d-da6f85796b09.html",
        "https://pilotonline.com/news/military/nation/article_1b8e436d-8350-5c46-81f4-2e4428822ed5.html",
        "https://pilotonline.com/news/government/politics/article_dc2af60d-fbaf-5420-bd5d-48fb58c7fd1d.html",
        "https://certification27.bloxcms.com/news/military/report-missteps-led-to-fatal-navy-helicopter-crash/article_4122eeb8-0c3a-5c81-9f79-63e3f870b06d.html",
        "https://pilotonline.com/news/government/local/article_85f812ae-7f25-11e8-b075-8f167a9541d5.html",
        "https://pilotonline.com/news/military/local/article_2b111084-9cb7-11e8-926b-f391b18ab7a6.html",
        "https://pilotonline.com/news/military/local/article_38bf927e-9b3a-11e8-bdb8-cf5b8181e232.html",
        "https://pilotonline.com/news/nation-world/national/article_22a6bf38-e300-543d-bdad-1170c1737a80.html",
        "https://pilotonline.com/news/military/nation/article_304f7890-4262-5a4e-8225-3305f598bee5.html",
        "https://pilotonline.com/news/nation-world/national/article_ba0300fa-1ce3-5dc5-8c41-434d08445013.html",
        "https://pilotonline.com/news/military/local/article_1658e7d6-a15b-11e8-a5e0-9f879f0b5bf4.html",
        "https://pilotonline.com/news/military/article_b2d01092-a158-11e8-9d2e-5fb6c188c9f4.html",
        "https://pilotonline.com/business/defense-shipyards/article_897199a2-9fcb-11e8-a3e5-27aa804852ce.html",
        "https://pilotonline.com/news/government/politics/nation/article_fd66971e-d5a5-5510-bb77-e41b31ee755a.html",
        "https://pilotonline.com/news/nation-world/national/article_59df27dc-7f0e-5d55-b3aa-961fdfe27402.html",
        "https://pilotonline.com/news/military/local/article_9e97ee28-8b6f-11e8-b486-47bd8d4146e9.html",
        "https://pilotonline.com/news/military/local/article_7db6fdd0-3dd3-5e20-aab3-4b3e9e6bf4e2.html",
        "https://pilotonline.com/news/military/local/article_9e36ee02-77e6-59b9-be4d-c3707a247348.html",
        "https://pilotonline.com/news/military/nation/article_8a2febd3-ebea-500e-a783-e89378c6d2c3.html",
        "https://pilotonline.com/news/military/local/article_3f350f44-3438-11e8-a593-8b399aed6b4d.html",
        "https://pilotonline.com/news/military/local/article_fb35e751-0f85-55f9-be9d-70f92cd2147b.html",
        "https://pilotonline.com/news/military/local/article_92b28118-214d-5b4b-a0e2-85db22f09d14.html",
        "https://pilotonline.com/news/military/local/article_e40aab36-8b8c-11e8-a008-9f886eb04e8d.html",
        "https://pilotonline.com/business/stocks/article_bbe69240-5591-57cb-ad36-65b55a788160.html",
        "https://pilotonline.com/news/nation-world/world/article_664835dd-ca2d-540d-8106-3e34efb7a3b0.html",
        "https://pilotonline.com/business/jobs/article_bd38a343-5aa1-5b4b-b98f-915acee1fe68.html",
        "https://pilotonline.com/business/article_3ac5458d-8fcf-5e6f-a891-f9e7e9fc7746.html",
        "https://pilotonline.com/news/nation-world/national/article_27a5d3d4-4ade-5b70-b658-18592a5de11f.html",
        "https://pilotonline.com/news/military/nation/article_c3777816-b2ea-55e0-8478-cbd049089b39.html",
        "https://pilotonline.com/news/military/nation/article_c3777816-b2ea-55e0-8478-cbd049089b39.html",
        "https://pilotonline.com/business/consumer/article_599976db-fea0-540a-a95b-6e3e924fc1a0.html",
        "https://pilotonline.com/news/military/nation/article_e3334ffe-18d8-5bb7-9898-9afa64fd5fd4.html",
        "https://pilotonline.com/news/military/local/article_e58b75d0-8a9c-11e8-bb50-6333f9982493.html",
        "https://pilotonline.com/news/military/nation/article_e3334ffe-18d8-5bb7-9898-9afa64fd5fd4.html",
        "https://pilotonline.com/news/nation-world/world/article_180ab1b1-8264-509d-be10-ecfd8c16dac1.html",
        "https://pilotonline.com/news/nation-world/national/article_f45a2244-c2fc-59fa-b3e5-ec048a56cf3c.html",
        "https://pilotonline.com/news/military/nation/article_bd9958c4-afbf-5d0c-b7fe-58cd2977e962.html",
        "https://pilotonline.com/news/military/local/article_1749cd88-938c-5975-ae99-6b694f595b89.html"]

-- question: how many URL did you distill?
