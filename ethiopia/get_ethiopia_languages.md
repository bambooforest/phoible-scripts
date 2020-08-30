Identify languages of Ethiopia in PHOIBLE
================
Steven Moran

In regard to [issue 299](https://github.com/phoible/dev/issues/299),
let’s check the status of certain segments in Ethiopian languages.

    library(tidyverse)
    library(knitr)
    library(testthat)

Load phoible.

    col_types <- cols(InventoryID='i', Marginal='l', .default='c')
    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types = col_types)

Load Glottolog.

    glottolog <- read_csv('glottolog_languoid.csv/languoid.csv')

Subset phoible.

    cut <- phoible %>% select(InventoryID, LanguageName, Source, Glottocode) %>% distinct()

Merge in Glottlog.

    cut <- left_join(cut, glottolog, by=c("Glottocode"="id"))

Subset languages reportedly spoken in Ethiopia.

    cut <- cut %>% filter(grepl("ET", country_ids)) %>% arrange(InventoryID)

There are 72 inventories.

    cut %>% kable()

| InventoryID | LanguageName                     | Source | Glottocode | family\_id | parent\_id | name                   | bookkeeping | level    |  latitude | longitude | iso639P3code | description | markup\_description | child\_family\_count | child\_language\_count | child\_dialect\_count | country\_ids         |
|------------:|:---------------------------------|:-------|:-----------|:-----------|:-----------|:-----------------------|:------------|:---------|----------:|----------:|:-------------|:------------|:--------------------|---------------------:|-----------------------:|----------------------:|:---------------------|
|         127 | Somali                           | spa    | soma1255   | afro1255   | east2653   | Somali                 | FALSE       | language |  4.778704 |  45.15286 | som          | NA          | NA                  |                    0 |                      0 |                     5 | DJ ET KE SO          |
|         128 | Awiya                            | spa    | awng1244   | afro1255   | cent2193   | Awngi                  | FALSE       | language | 10.981900 |  36.69160 | awn          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|         131 | Amharic                          | spa    | amha1245   | afro1255   | amha1244   | Amharic                | FALSE       | language | 11.708182 |  39.54346 | amh          | NA          | NA                  |                    0 |                      0 |                     0 | DJ ET                |
|         218 | HAMER                            | upsid  | hame1242   | sout2845   | hame1241   | Hamer-Banna            | FALSE       | language |  5.065150 |  36.51760 | amf          | NA          | NA                  |                    0 |                      0 |                     0 | ET KE                |
|         219 | AMHARIC                          | upsid  | amha1245   | afro1255   | amha1244   | Amharic                | FALSE       | language | 11.708182 |  39.54346 | amh          | NA          | NA                  |                    0 |                      0 |                     0 | DJ ET                |
|         234 | AWIYA                            | upsid  | awng1244   | afro1255   | cent2193   | Awngi                  | FALSE       | language | 10.981900 |  36.69160 | awn          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|         343 | KULLO                            | upsid  | dawr1236   | gong1255   | dawr1235   | Dawro                  | FALSE       | language |  6.950000 |  37.00000 | dwr          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|         394 | KEFA                             | upsid  | kafa1242   | gong1255   | sout2835   | Kafa                   | FALSE       | language |  7.340220 |  36.17180 | kbr          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|         433 | KUNAMA                           | upsid  | kuna1268   | NA         | NA         | Kunama                 | FALSE       | language | 14.587900 |  37.52920 | kun          | NA          | NA                  |                    0 |                      0 |                     9 | ER ET SD             |
|         457 | DIZI                             | upsid  | dizi1235   | dizo1235   | dizo1235   | Dizin                  | FALSE       | language |  6.140500 |  35.57630 | mdx          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|         473 | MURSI                            | upsid  | murs1242   | surm1244   | suri1266   | Mursi                  | FALSE       | language |  5.698010 |  36.09710 | muz          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|         552 | SOMALI                           | upsid  | soma1255   | afro1255   | east2653   | Somali                 | FALSE       | language |  4.778704 |  45.15286 | som          | NA          | NA                  |                    0 |                      0 |                     5 | DJ ET KE SO          |
|         621 | BERTA                            | upsid  | bert1248   | NA         | NA         | Berta                  | FALSE       | language | 10.647400 |  34.70420 | wti          | NA          | NA                  |                    0 |                      0 |                     7 | ET SD                |
|         625 | KOMA                             | upsid  | komo1258   | koma1264   | twam1235   | Komo (Sudan-Ethiopia)  | FALSE       | language |  8.941620 |  34.09390 | xom          | NA          | NA                  |                    0 |                      0 |                     0 | ET SD                |
|         659 | Arabe                            | aa     | suda1236   | afro1255   | suda1235   | Sudanese Arabic        | FALSE       | language | 17.802100 |  33.31790 | apd          | NA          | NA                  |                    0 |                      0 |                     5 | CF EG ER ET LY SD TD |
|         712 | Fulfulde                         | aa     | adam1253   | atla1278   | adam1260   | Adamawa Fulfulde       | FALSE       | language |  8.140326 |  13.07734 | fub          | NA          | NA                  |                    0 |                      0 |                     6 | CM ER ET NG SD TD    |
|         793 | Murle                            | aa     | murl1244   | surm1244   | didi1256   | Murle                  | FALSE       | language |  6.697560 |  33.98800 | mur          | NA          | NA                  |                    0 |                      0 |                     3 | ET SD                |
|         968 | Nuer                             | ph     | nuer1246   | nilo1247   | nuer1245   | Nuer                   | FALSE       | language |  8.139110 |  32.38290 | nus          | NA          | NA                  |                    0 |                      0 |                    10 | ET SD                |
|        1005 | Shabo                            | ph     | shab1252   | NA         | NA         | Shabo                  | FALSE       | language |  7.644070 |  35.22300 | sbf          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1080 | Sidaama; Sidamo; Sidamic         | ph     | sida1246   | afro1255   | sida1248   | Sidamo                 | FALSE       | language |  6.741760 |  38.37290 | sid          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1161 | Agaw                             | ph     | qima1242   | afro1255   | nort3158   | Qimant                 | FALSE       | language | 12.811900 |  37.05420 | ahg          | NA          | NA                  |                    0 |                      0 |                     4 | ER ET                |
|        1165 | Anywa                            | ph     | anua1242   | nilo1247   | nort2814   | Anuak                  | FALSE       | language |  7.577140 |  34.02670 | anu          | NA          | NA                  |                    0 |                      0 |                     4 | ET SD                |
|        1197 | Boro                             | ph     | boro1277   | gong1255   | gong1256   | Boro (Ethiopia)        | FALSE       | language | 10.659600 |  35.34500 | bwo          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1209 | Suri                             | ph     | suri1267   | surm1244   | suri1266   | Tirma-Chai             | FALSE       | language |  6.032050 |  35.08230 | suq          | NA          | NA                  |                    0 |                      0 |                     2 | ET SD                |
|        1234 | Adamawa Fulfulde; Adamawa Fulani | ph     | adam1253   | atla1278   | adam1260   | Adamawa Fulfulde       | FALSE       | language |  8.140326 |  13.07734 | fub          | NA          | NA                  |                    0 |                      0 |                     6 | CM ER ET NG SD TD    |
|        1241 | Fulfulde (Cameroon)              | gm     | adam1253   | atla1278   | adam1260   | Adamawa Fulfulde       | FALSE       | language |  8.140326 |  13.07734 | fub          | NA          | NA                  |                    0 |                      0 |                     6 | CM ER ET NG SD TD    |
|        1253 | fulfulde (fuunaangere)           | gm     | adam1253   | atla1278   | adam1260   | Adamawa Fulfulde       | FALSE       | language |  8.140326 |  13.07734 | fub          | NA          | NA                  |                    0 |                      0 |                     6 | CM ER ET NG SD TD    |
|        1315 | Maay                             | gm     | maay1238   | afro1255   | east2653   | Maay                   | FALSE       | language |  3.222880 |  43.61820 | ymm          | NA          | NA                  |                    0 |                      0 |                     1 | ET KE SO             |
|        1333 | Xamtanga                         | gm     | xamt1239   | afro1255   | nort3163   | Xamtanga               | FALSE       | language | 12.447900 |  38.83520 | xan          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1342 | Afar                             | gm     | afar1241   | afro1255   | saho1245   | Afar                   | FALSE       | language | 12.228100 |  41.80830 | aar          | NA          | NA                  |                    0 |                      0 |                     4 | DJ ER ET             |
|        1349 | Northern Mao                     | gm     | bamb1262   | maoo1243   | maoo1243   | Bambassi               | FALSE       | language |  9.825540 |  34.65220 | myf          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|        1350 | Tigrinya                         | gm     | tigr1271   | afro1255   | tigr1276   | Tigrinya               | FALSE       | language | 15.335900 |  38.92660 | tir          | NA          | NA                  |                    0 |                      0 |                     2 | DJ ER ET SD          |
|        1362 | Galla                            | gm     | bora1271   | afro1255   | cent2303   | Borana-Arsi-Guji Oromo | FALSE       | language |  1.056760 |  37.88200 | gax          | NA          | NA                  |                    0 |                      0 |                     8 | ET KE SO             |
|        1363 | Hadiyya                          | gm     | hadi1240   | afro1255   | hadi1242   | Hadiyya                | FALSE       | language |  7.573770 |  37.75770 | hdy          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|        1364 | Welamo                           | gm     | wola1242   | gong1255   | cent2046   | Wolaytta               | FALSE       | language |  6.326680 |  37.75370 | wal          | NA          | NA                  |                    0 |                      0 |                     1 | ET                   |
|        1373 | Kambaata                         | gm     | kamb1316   | afro1255   | kamb1318   | Kambaata               | FALSE       | language |  7.375820 |  37.90880 | ktb          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|        1396 | Argobba                          | gm     | argo1244   | afro1255   | amha1244   | Argobba                | FALSE       | language | 10.659600 |  39.75600 | agj          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1397 | Harari                           | gm     | hara1271   | afro1255   | hara1270   | Harari                 | FALSE       | language |  9.304850 |  42.13320 | har          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1414 | Silt’i                           | gm     | silt1240   | afro1255   | silt1239   | Silt’e                 | FALSE       | language |  7.770040 |  38.14490 | stv          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1440 | Dime                             | gm     | dime1235   | sout2845   | sout2845   | Dime                   | FALSE       | language |  6.209510 |  36.33290 | dim          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1451 | Alaaba                           | gm     | alab1254   | afro1255   | kamb1318   | Alaba-K’abeena         | FALSE       | language |  7.390140 |  38.17320 | alw          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1457 | Goggot                           | gm     | kist1241   | afro1255   | ngro1237   | Kistane                | FALSE       | language |  8.335610 |  38.47630 | gru          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|        1458 | Masqan                           | gm     | mesq1240   | afro1255   | ttgr1237   | Mesqan                 | FALSE       | language |  8.106170 |  38.33860 | mvz          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1459 | Muher                            | gm     | seba1251   | afro1255   | ttgr1237   | Sebat Bet Gurage       | FALSE       | language |  8.118790 |  37.98910 | sgw          | NA          | NA                  |                    0 |                      0 |                     5 | ET                   |
|        1463 | Gura                             | gm     | seba1251   | afro1255   | ttgr1237   | Sebat Bet Gurage       | FALSE       | language |  8.118790 |  37.98910 | sgw          | NA          | NA                  |                    0 |                      0 |                     5 | ET                   |
|        1464 | Gyeto                            | gm     | seba1251   | afro1255   | ttgr1237   | Sebat Bet Gurage       | FALSE       | language |  8.118790 |  37.98910 | sgw          | NA          | NA                  |                    0 |                      0 |                     5 | ET                   |
|        1465 | Ennemor                          | gm     | inor1238   | afro1255   | inor1239   | Inoric                 | FALSE       | language |  7.948830 |  37.79680 | ior          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1466 | Endegen                          | gm     | inor1238   | afro1255   | inor1239   | Inoric                 | FALSE       | language |  7.948830 |  37.79680 | ior          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1467 | Ener                             | gm     | inor1238   | afro1255   | inor1239   | Inoric                 | FALSE       | language |  7.948830 |  37.79680 | ior          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1472 | Me’en                            | gm     | meen1242   | surm1244   | past1248   | Me’en                  | FALSE       | language |  6.632070 |  35.62550 | mym          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|        1475 | Gumuz                            | gm     | gumu1244   | gumu1250   | gumu1250   | Northern Gumuz         | FALSE       | language | 11.050000 |  35.93000 | guk          | NA          | NA                  |                    0 |                      0 |                    19 | ET SD                |
|        1477 | Gimira                           | gm     | benc1235   | gong1255   | gong1255   | Bench                  | FALSE       | language |  7.046820 |  35.76730 | bcq          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|        1478 | Bench                            | gm     | benc1235   | gong1255   | gong1255   | Bench                  | FALSE       | language |  7.046820 |  35.76730 | bcq          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|        1479 | Yemsa                            | gm     | yems1235   | gong1255   | gong1255   | Yemsa                  | FALSE       | language |  7.798210 |  37.44120 | jnj          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|        1481 | Gamo                             | gm     | gamo1243   | gong1255   | dawr1235   | Gamo                   | FALSE       | language |  6.276450 |  37.23808 | gmv          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1482 | Koorete                          | gm     | koor1239   | gong1255   | east2423   | Koorete                | FALSE       | language |  5.805450 |  37.86790 | kqy          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1483 | Aari                             | gm     | aari1239   | sout2845   | aari1238   | Aari                   | FALSE       | language |  5.950340 |  36.57210 | aiw          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1484 | Komo                             | gm     | komo1258   | koma1264   | twam1235   | Komo (Sudan-Ethiopia)  | FALSE       | language |  8.941620 |  34.09390 | xom          | NA          | NA                  |                    0 |                      0 |                     0 | ET SD                |
|        1485 | Twampa                           | gm     | uduk1239   | koma1264   | twam1235   | Uduk                   | FALSE       | language |  9.160120 |  34.22930 | udu          | NA          | NA                  |                    0 |                      0 |                     4 | ET SD                |
|        1486 | Opo                              | gm     | opuu1239   | koma1264   | dana1255   | Opo                    | FALSE       | language |  8.475260 |  33.85300 | lgn          | NA          | NA                  |                    0 |                      0 |                     8 | ET SD                |
|        1487 | Kwama                            | gm     | kwam1249   | koma1264   | koma1264   | Gwama                  | FALSE       | language |  9.508360 |  34.24570 | kmq          | NA          | NA                  |                    0 |                      0 |                     2 | ET                   |
|        1510 | Zway                             | gm     | zayy1238   | afro1255   | hara1270   | Zay                    | FALSE       | language |  8.011480 |  38.82580 | zwa          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1525 | Arbore                           | gm     | arbo1245   | afro1255   | west2723   | Arbore                 | FALSE       | language |  4.921830 |  36.79860 | arv          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1534 | Nuer                             | gm     | nuer1246   | nilo1247   | nuer1245   | Nuer                   | FALSE       | language |  8.139110 |  32.38290 | nus          | NA          | NA                  |                    0 |                      0 |                    10 | ET SD                |
|        1542 | Awngi                            | gm     | awng1244   | afro1255   | cent2193   | Awngi                  | FALSE       | language | 10.981900 |  36.69160 | awn          | NA          | NA                  |                    0 |                      0 |                     3 | ET                   |
|        1552 | Oromo, Eastern                   | gm     | east2652   | afro1255   | sout3218   | Eastern Oromo          | FALSE       | language |  8.674280 |  41.43950 | hae          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        1608 | Ts’amakko                        | gm     | tsam1247   | afro1255   | dull1239   | Tsamai                 | FALSE       | language |  5.309700 |  36.91390 | tsb          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1657 | Burji                            | gm     | burj1242   | afro1255   | high1285   | Burji                  | FALSE       | language |  4.446240 |  38.46460 | bji          | NA          | NA                  |                    0 |                      0 |                     0 | ET KE                |
|        1661 | Anfillo                          | gm     | anfi1235   | gong1255   | gong1256   | Anfillo                | FALSE       | language |  8.568000 |  34.64700 | myo          | NA          | NA                  |                    0 |                      0 |                     0 | ET                   |
|        1714 | BoRo                             | ra     | boro1277   | gong1255   | gong1256   | Boro (Ethiopia)        | FALSE       | language | 10.659600 |  35.34500 | bwo          | NA          | NA                  |                    0 |                      0 |                     4 | ET                   |
|        2156 | Amharic                          | uz     | amha1245   | afro1255   | amha1244   | Amharic                | FALSE       | language | 11.708182 |  39.54346 | amh          | NA          | NA                  |                    0 |                      0 |                     0 | DJ ET                |
|        2198 | Kunama                           | uz     | kuna1268   | NA         | NA         | Kunama                 | FALSE       | language | 14.587900 |  37.52920 | kun          | NA          | NA                  |                    0 |                      0 |                     9 | ER ET SD             |

Sascha Völlmin notes that in
[Zay](https://phoible.org/inventories/view/1510), \[q\] should be \[k’\]
(the traditional notation for ejective k’ in Ethiopistic studies).

Let’s see which of these languages have \[q\] in the inventories. First
get the Ethiopian inventories.

    e_inventories <- phoible %>% filter(InventoryID %in% cut$InventoryID)

    # Double check that we have the same number of inventories
    expect_equal(nrow(e_inventories %>% select(InventoryID) %>% distinct()), nrow(cut))

Then select the ones with \[q\].

    e_inventories %>% filter(grepl("q", Phoneme)) %>% select(InventoryID, Glottocode, LanguageName, SpecificDialect, Source, Phoneme, Marginal) %>% kable()

| InventoryID | Glottocode | LanguageName           | SpecificDialect | Source | Phoneme | Marginal |
|------------:|:-----------|:-----------------------|:----------------|:-------|:--------|:---------|
|         128 | awng1244   | Awiya                  | NA              | spa    | q       | NA       |
|         128 | awng1244   | Awiya                  | NA              | spa    | qʷ      | NA       |
|         218 | hame1242   | HAMER                  | NA              | upsid  | qʼ      | FALSE    |
|         234 | awng1244   | AWIYA                  | NA              | upsid  | q       | FALSE    |
|         234 | awng1244   | AWIYA                  | NA              | upsid  | qʷ      | FALSE    |
|        1161 | qima1242   | Agaw                   | NA              | ph     | q       | FALSE    |
|        1161 | qima1242   | Agaw                   | NA              | ph     | qʷ      | FALSE    |
|        1241 | adam1253   | Fulfulde (Cameroon)    | CAM             | gm     | q       | TRUE     |
|        1253 | adam1253   | fulfulde (fuunaangere) | fuunaangere     | gm     | q       | FALSE    |
|        1333 | xamt1239   | Xamtanga               | NA              | gm     | q       | FALSE    |
|        1333 | xamt1239   | Xamtanga               | NA              | gm     | qʷ      | FALSE    |
|        1350 | tigr1271   | Tigrinya               | NA              | gm     | q       | FALSE    |
|        1350 | tigr1271   | Tigrinya               | NA              | gm     | qʼ      | FALSE    |
|        1397 | hara1271   | Harari                 | NA              | gm     | q       | FALSE    |
|        1483 | aari1239   | Aari                   | NA              | gm     | q       | FALSE    |
|        1510 | zayy1238   | Zway                   | NA              | gm     | q       | FALSE    |
|        1542 | awng1244   | Awngi                  | NA              | gm     | q       | FALSE    |
|        1542 | awng1244   | Awngi                  | NA              | gm     | qʷ      | FALSE    |
|        1608 | tsam1247   | Ts’amakko              | NA              | gm     | qʼ      | FALSE    |
|        1608 | tsam1247   | Ts’amakko              | NA              | gm     | qʼː     | FALSE    |

I suspect that the sources `spa` and `upsid` are correct, e.g. Awngi
(here listed with an inapproriate ethnonym) and Hamar indeed have a
uvular \[q\]:

-   <a href="https://en.wikipedia.org/wiki/Awngi_language" class="uri">https://en.wikipedia.org/wiki/Awngi_language</a>
-   <a href="https://en.wikipedia.org/wiki/Hamer_language" class="uri">https://en.wikipedia.org/wiki/Hamer_language</a>

And that the source `gm` inventories are potentially incorrect,
including Zway. Fulfulde is spoken mainly in West Africa:

-   <a href="https://en.wikipedia.org/wiki/Fula_language" class="uri">https://en.wikipedia.org/wiki/Fula_language</a>

But we should check the status of \[q\] there.

Agaw’s glottocode:

-   <a href="https://glottolog.org/resource/languoid/id/qima1242" class="uri">https://glottolog.org/resource/languoid/id/qima1242</a>

notes that the dialect is Qimant:

<a href="https://en.wikipedia.org/wiki/Qimant_language" class="uri">https://en.wikipedia.org/wiki/Qimant_language</a>

We should also check these sources since both Fula and Qimant seem to
lack uvular \[q\].

The rest of the `gm` languages in Ethiopia need to be checked for
systematic transcriptio errors for \[q\] and any other transcription
practices in Ethiopistic studies.
