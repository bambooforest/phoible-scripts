Update phoible index
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
library(knitr)
```

``` r
# TODOS:
## update the phoible index with the most up-to-date ISO codes (requires dialect-parent-language lookup)
## do we want to add in the canonical Glottolog language names as well?
## add in level, family, geography, etc., metadata
```

``` r
# Get phoible index (TODO: update this to a CSV file)
index <- read.table('https://raw.githubusercontent.com/phoible/dev/australian-data-update/mappings/InventoryID-LanguageCodes.tsv', sep="\t", quote="\"", header=T, stringsAsFactors=F)
expect_equal(nrow(index), 3020)
head(index)
```

    ##   InventoryID LanguageCode Glottocode LanguageName Source
    ## 1           1          kor   kore1280       Korean    spa
    ## 2           2          ket   kett1243          Ket    spa
    ## 3           3          lbe   lakk1252          Lak    spa
    ## 4           4          kbd   kaba1278    Kabardian    spa
    ## 5           5          kat   nucl1302     Georgian    spa
    ## 6           6          bsk   buru1296   Burushaski    spa

``` r
# Glottolog dialects and geo data (v 3.3)
glottolog <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-F088-DE0E-0712-0/languages_and_dialects_geo.csv')
head(glottolog)
```

    ##   glottocode       name isocodes    level macroarea latitude longitude
    ## 1   aala1237     Aalawa           dialect Papunesia       NA        NA
    ## 2   aant1238 Aantantara           dialect Papunesia       NA        NA
    ## 3   aari1239       Aari      aiw language    Africa  5.95034   36.5721
    ## 4   aari1240     Aariya      aay language   Eurasia       NA        NA
    ## 5   aasa1238      Aasax      aas language    Africa -4.00679   36.8648
    ## 6   aata1238  Aatasaara           dialect Papunesia       NA        NA

``` r
glottolog.cut <- glottolog %>% select(glottocode, name, level, macroarea, latitude, longitude, isocodes)
index <- left_join(index, glottolog.cut, by=c("Glottocode"="glottocode"))
```

    ## Warning: Column `Glottocode`/`glottocode` joining character vector and
    ## factor, coercing into character vector

``` r
# Here the PHOIBLE and Glottocode ISO codes don't match up
kable(index[which(!(index$LanguageCode == index$isocodes)),] %>% select(InventoryID, LanguageCode, isocodes, Glottocode, LanguageName, name))
```

|      |  InventoryID| LanguageCode | isocodes | Glottocode | LanguageName        | name                |
|------|------------:|:-------------|:---------|:-----------|:--------------------|:--------------------|
| 40   |           40| khl          |          | kali1299   | Kaliai              | Kaliai              |
| 136  |          136| xtc          |          | katc1250   | Katcha              | Katcha              |
| 155  |          155| naq          |          | nama1265   | Nama                | Nama                |
| 298  |          298| lda          | daf      | dann1241   | DAN                 | Dan                 |
| 613  |          613| wnw          | wit      | wint1259   | WINTU               | Wintu               |
| 691  |          691| dnj          | daf      | dann1241   | dan                 | Dan                 |
| 871  |          871| kpr          |          | kora1295   | Korafe              | Korafe              |
| 875  |          875| enb          |          | endo1242   | Endo                | Endo                |
| 885  |          885| wya          |          | huro1249   | Huron               | Huron               |
| 916  |          916| kdt          |          | kuay1244   | Kuay                | Kuay                |
| 947  |          947| nmg          |          | mvum1238   | Mvumbo              | Mvumbo              |
| 973  |          973| mpt          |          | mian1257   | Mianmin             | Mianmin             |
| 996  |          996| str          |          | saan1246   | Saanich             | Saanich             |
| 1097 |         1097| arr          |          | karo1306   | Karo                | Karo                |
| 1102 |         1102| biw          |          | bike1242   | Bikele              | Bikele              |
| 1192 |         1192| nbj          |          | bili1250   | Bilinara            | Bilinarra           |
| 1276 |         1276| kck          |          | ikal1242   | Ikalanga            | Ikalanga            |
| 1318 |         1318| cce          |          | copi1238   | Copi                | Copi                |
| 1322 |         1322| buy          |          | mman1238   | Mmani               | Mmani               |
| 1325 |         1325| gur          |          | fraf1238   | Frafra              | Frafra              |
| 1382 |         1382| oks          |          | okoo1245   | Oko                 | Oko                 |
| 1393 |         1393| lda          | daf      | dann1241   | Dan                 | Dan                 |
| 1401 |         1401| hna          |          | besl1239   | Besleri             | Besleri             |
| 1455 |         1455| nko          |          | nkon1248   | Nkonya              | Nkonya              |
| 1456 |         1456| gru          |          | sodd1242   | Soddo               | Soddo               |
| 1460 |         1460| sgw          |          | ezha1238   | Ezha                | Ezha                |
| 1461 |         1461| sgw          |          | chah1248   | Chaha               | Chaha               |
| 1462 |         1462| sgw          |          | gume1239   | Gumer               | Gumer               |
| 1480 |         1480| zay          |          | zays1236   | Zayse               | Zayse               |
| 1523 |         1523| afu          |          | efut1241   | Efutu               | Efutu               |
| 1591 |         1591| nyf          |          | kamb1298   | Kambe               | Kambe               |
| 1592 |         1592| nyf          |          | kaum1238   | Kauma               | Kauma               |
| 1625 |         1625| mgo          |          | mogh1246   | Moghamo             | Moghamo             |
| 1633 |         1633| pnz          |          | pana1294   | Pana                | Pana                |
| 1701 |         1701| mrr          |          | abuj1237   | Abujmaria           | Abujmaria           |
| 1768 |         1768| mrg          |          | miny1240   | Mising              | Mising              |
| 1771 |         1771| nit          |          | naik1250   | Naiki               | Naiki               |
| 1772 |         1772| nep          | npi      | nepa1254   | Nepali              | Nepali              |
| 1858 |         1858| boa          |          | mira1254   | Miraña              | Miraña              |
| 1994 |         1994| nab          |          | khit1238   | Kithaulhu           | Khithaulhu          |
| 2015 |         2015| shp          |          | ship1255   | Shipibo             | Shipibo             |
| 2092 |         2092| arr          |          | karo1306   | Karo                | Karo                |
| 2204 |         2204| nep          | npi      | nepa1254   | Nepali              | Nepali              |
| 2225 |         2225| kxd          |          | keda1250   | Kedayan             | Kedayan             |
| 2537 |         2537| tks          |          | khal1271   | Khalkhal            | Khalkhal            |
| 2586 |         2586| pbt          |          | sout2436   | Southwestern Pashto | Southwestern Pashto |
| 2634 |         2634| mwf          |          | murr1259   | Murrinh-patha       | Murrinhpatha        |
| 2657 |         2657| gup          |          | gund1246   | Gun-Djeihmi         | Kundjeyhmi          |
| 2658 |         2658| gup          |          | gune1238   | Kune                | Kune                |
| 2659 |         2659| gup          |          | mura1269   | Kuninjku            | Kuninjku            |
| 2660 |         2660| gup          |          | guma1252   | Kunwinjku           | Kunwinjku           |
| 2661 |         2661| gup          |          | naia1238   | Mayali              | Manyallaluk Mayali  |
| 2670 |         2670| ilg          |          | gari1254   | Garig               | Garig               |
| 2671 |         2671| ilg          |          | ilga1238   | Ilgar               | Ilgar               |
| 2684 |         2684| nug          |          | ngal1294   | Ngaliwurru          | Ngaliwurru          |
| 2689 |         2689| nji          |          | guda1242   | Gudanji             | Wambaya-Gudanji     |
| 2703 |         2703| amx          |          | east2380   | Eastern Anmatyerre  | Eastern Anmatyerre  |
| 2704 |         2704| amx          |          | west2442   | Western Anmatyerre  | Western Anmatyerre  |
| 2706 |         2706| aer          |          | mpar1238   | Central Arrernte    | Mparntwe Arrernte   |
| 2707 |         2707| adg          | axe      | ayer1246   | Ayerrerenge         | Ayerrerenge         |
| 2712 |         2712| bdy          |          | yugu1249   | Bundjalung          | Yugumbir            |
| 2713 |         2713| gih          |          | gida1240   | Gidabal             | Gidabal             |
| 2714 |         2714| bdy          |          | guwa1244   | Guwar               | Guwar               |
| 2718 |         2718| xbg          |          | warr1257   | Warrnambool         | Warrnambool         |
| 2722 |         2722| wyb          |          | wayi1238   | Wayilwan            | Wayilwan            |
| 2724 |         2724| kld          |          | wirr1237   | Wiriyaraay          | Wirriyaraay         |
| 2725 |         2725| kld          |          | yuwa1242   | Yuwaalaraay         | Yuwaalaraay         |
| 2726 |         2726| kld          |          | yuwa1243   | Yuwaliyaay          | Yuwaalayaay         |
| 2727 |         2727| xyy          |          | dhud1236   | Dhudhuroa           | Dhudhuroa           |
| 2751 |         2751| nmv          |          | kara1508   | Karangura           | Karangura           |
| 2752 |         2752| nmv          |          | ngam1284   | Ngamini             | Ngamini             |
| 2755 |         2755| ynd          | hrp      | nhir1234   | Nhirrpi             | Nhirrpi             |
| 2774 |         2774| wyi          | dgw      | daun1234   | Daungwurrung        | Daungwurrung        |
| 2781 |         2781| tbh          | xwd      | wadi1260   | Wathi Wathi         | Wadi Wadi           |
| 2792 |         2792| nay          |          | nort2756   | Keramin             | Northern Sunraysia  |
| 2829 |         2829| mnt          | xyk      | mayi1236   | Mayi-Kulan          | Mayi-Kulan          |
| 2832 |         2832| mnt          | xyj      | mayi1235   | Mayi-Yapi           | Mayi-Yapi           |
| 2851 |         2851| nbj          |          | bili1250   | Bilinarra           | Bilinarra           |
| 2852 |         2852| gue          |          | wany1244   | Wanyjirra           | Wanyjirra           |
| 2855 |         2855| gue          |          | maln1239   | Malngin             | Malngin             |
| 2867 |         2867| nys          |          | kani1276   | Kaniyang            | Kaniyang            |
| 2918 |         2918| amz          |          | angg1238   | Angkamuthi          | Angkamuthi          |
| 2920 |         2920| amz          |          | yadh1237   | Yadhaykenu          | Yadhaykenu          |
| 2924 |         2924| kjn          |          | ulku1238   | Olkol               | Ulkulu              |
| 2930 |         2930| kjn          |          | oyka1239   | Oykangand           | Oykangand           |
| 2941 |         2941| gbw          |          | gabi1248   | Gabi-Gabi           | Gabigabi            |
| 2943 |         2943| wkw          |          | duun1241   | Duungidjawu         | Duungidjawu         |
| 2952 |         2952| mpj          |          | kart1247   | Kartujarra          | Kartujarra          |
| 2953 |         2953| mpj          |          | many1256   | Manjiljarra         | Manyjilyjara        |
| 2954 |         2954| mpj          |          | pudi1238   | Putijarra           | Puditara            |
| 2955 |         2955| mpj          |          | wang1288   | Wangkajunga         | Wangkajunga         |
| 2966 |         2966| mwp          |          | kala1378   | Kala Kawaw Ya       | Kalaw Kawaw         |
| 2973 |         2973| dax          |          | dhal1246   | Dhay'yi             | Dhalwangu           |
| 2992 |         2992| duj          |          | djap1238   | Djapu               | Djapu               |
| 3018 |         3018| wbp          |          | west2437   | Ngardily            | Western Warlpiri    |

``` r
kable(index[which(!(index$LanguageCode == index$isocodes)),] %>% filter(isocodes!="") %>% select(InventoryID, LanguageCode, Glottocode, LanguageName, name, level, isocodes))
```

|  InventoryID| LanguageCode | Glottocode | LanguageName | name         | level    | isocodes |
|------------:|:-------------|:-----------|:-------------|:-------------|:---------|:---------|
|          298| lda          | dann1241   | DAN          | Dan          | language | daf      |
|          613| wnw          | wint1259   | WINTU        | Wintu        | language | wit      |
|          691| dnj          | dann1241   | dan          | Dan          | language | daf      |
|         1393| lda          | dann1241   | Dan          | Dan          | language | daf      |
|         1772| nep          | nepa1254   | Nepali       | Nepali       | language | npi      |
|         2204| nep          | nepa1254   | Nepali       | Nepali       | language | npi      |
|         2707| adg          | ayer1246   | Ayerrerenge  | Ayerrerenge  | dialect  | axe      |
|         2755| ynd          | nhir1234   | Nhirrpi      | Nhirrpi      | dialect  | hrp      |
|         2774| wyi          | daun1234   | Daungwurrung | Daungwurrung | dialect  | dgw      |
|         2781| tbh          | wadi1260   | Wathi Wathi  | Wadi Wadi    | dialect  | xwd      |
|         2829| mnt          | mayi1236   | Mayi-Kulan   | Mayi-Kulan   | dialect  | xyk      |
|         2832| mnt          | mayi1235   | Mayi-Yapi    | Mayi-Yapi    | dialect  | xyj      |
