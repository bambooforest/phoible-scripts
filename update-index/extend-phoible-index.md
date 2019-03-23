Extend the phoible index with Glottolog data
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
library(knitr)
```

``` r
# Get phoible index
index <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors=F)
expect_equal(nrow(index), 3020)
head(index)
```

    ##   InventoryID ISO6393 Glottocode LanguageName Source
    ## 1           1     kor   kore1280       Korean    spa
    ## 2           2     ket   kett1243          Ket    spa
    ## 3           3     lbe   lakk1252          Lak    spa
    ## 4           4     kbd   kaba1278    Kabardian    spa
    ## 5           5     kat   nucl1302     Georgian    spa
    ## 6           6     bsk   buru1296   Burushaski    spa

``` r
# Glottolog languiods data (v 3.3)
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/glottolog_languoid.csv.zip
languoids <- read.csv('glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors=F)
# languoids <- languoids %>% select(id, family_id, parent_id, status,  country_ids, iso639P3code)
```

``` r
index <- left_join(index, languoids, by=c("Glottocode"="id"))
expect_equal(nrow(index), 3020)
glimpse(index)
```

    ## Observations: 3,020
    ## Variables: 20
    ## $ InventoryID          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13...
    ## $ ISO6393              <chr> "kor", "ket", "lbe", "kbd", "kat", "bsk",...
    ## $ Glottocode           <chr> "kore1280", "kett1243", "lakk1252", "kaba...
    ## $ LanguageName         <chr> "Korean", "Ket", "Lak", "Kabardian", "Geo...
    ## $ Source               <chr> "spa", "spa", "spa", "spa", "spa", "spa",...
    ## $ family_id            <chr> "kore1284", "yeni1252", "nakh1245", "abkh...
    ## $ parent_id            <chr> "kore1284", "nort2746", "dagh1238", "circ...
    ## $ name                 <chr> "Korean", "Ket", "Lak", "Kabardian", "Geo...
    ## $ bookkeeping          <chr> "False", "False", "False", "False", "Fals...
    ## $ level                <chr> "language", "language", "language", "lang...
    ## $ status               <chr> "safe", "definitely endangered", "vulnera...
    ## $ latitude             <dbl> 37.50000, 63.75510, 42.13280, 43.50820, 4...
    ## $ longitude            <dbl> 128.00000, 87.54660, 47.08090, 43.39180, ...
    ## $ iso639P3code         <chr> "kor", "ket", "lbe", "kbd", "kat", "bsk",...
    ## $ description          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ markup_description   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ child_family_count   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ child_language_count <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ child_dialect_count  <int> 8, 0, 4, 1, 16, 3, 2, 17, 1, 0, 3, 4, 5, ...
    ## $ country_ids          <chr> "CN KP KR RU", "RU", "AZ GE KZ RU UA", "R...

``` r
# Glottolog geo data (v 3.3)
geo <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-F088-DE0E-0712-0/languages_and_dialects_geo.csv')
kable(head(geo))
```

| glottocode | name       | isocodes | level    | macroarea |  latitude|  longitude|
|:-----------|:-----------|:---------|:---------|:----------|---------:|----------:|
| aala1237   | Aalawa     |          | dialect  | Papunesia |        NA|         NA|
| aant1238   | Aantantara |          | dialect  | Papunesia |        NA|         NA|
| aari1239   | Aari       | aiw      | language | Africa    |   5.95034|    36.5721|
| aari1240   | Aariya     | aay      | language | Eurasia   |        NA|         NA|
| aasa1238   | Aasax      | aas      | language | Africa    |  -4.00679|    36.8648|
| aata1238   | Aatasaara  |          | dialect  | Papunesia |        NA|         NA|

``` r
index <- left_join(index, geo)
```

    ## Joining, by = c("name", "level", "latitude", "longitude")

    ## Warning: Column `name` joining character vector and factor, coercing into
    ## character vector

    ## Warning: Column `level` joining character vector and factor, coercing into
    ## character vector

``` r
expect_equal(nrow(index), 3020)
kable(head(index))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName | Source | family\_id | parent\_id | name       | bookkeeping | level    | status                |  latitude|  longitude| iso639P3code | description | markup\_description |  child\_family\_count|  child\_language\_count|  child\_dialect\_count| country\_ids      | glottocode | isocodes | macroarea |
|------------:|:--------|:-----------|:-------------|:-------|:-----------|:-----------|:-----------|:------------|:---------|:----------------------|---------:|----------:|:-------------|:------------|:--------------------|---------------------:|-----------------------:|----------------------:|:------------------|:-----------|:---------|:----------|
|            1| kor     | kore1280   | Korean       | spa    | kore1284   | kore1284   | Korean     | False       | language | safe                  |   37.5000|  128.00000| kor          | NA          | NA                  |                     0|                       0|                      8| CN KP KR RU       | kore1280   | kor      | Eurasia   |
|            2| ket     | kett1243   | Ket          | spa    | yeni1252   | nort2746   | Ket        | False       | language | definitely endangered |   63.7551|   87.54660| ket          | NA          | NA                  |                     0|                       0|                      0| RU                | kett1243   | ket      | Eurasia   |
|            3| lbe     | lakk1252   | Lak          | spa    | nakh1245   | dagh1238   | Lak        | False       | language | vulnerable            |   42.1328|   47.08090| lbe          | NA          | NA                  |                     0|                       0|                      4| AZ GE KZ RU UA    | lakk1252   | lbe      | Eurasia   |
|            4| kbd     | kaba1278   | Kabardian    | spa    | abkh1242   | circ1239   | Kabardian  | False       | language | vulnerable            |   43.5082|   43.39180| kbd          | NA          | NA                  |                     0|                       0|                      1| RU TR             | kaba1278   | kbd      | Eurasia   |
|            5| kat     | nucl1302   | Georgian     | spa    | kart1248   | geor1253   | Georgian   | False       | language | safe                  |   41.8504|   43.78613| kat          | NA          | NA                  |                     0|                       0|                     16| AM AZ GE IR RU TR | nucl1302   | kat      | Eurasia   |
|            6| bsk     | buru1296   | Burushaski   | spa    |            |            | Burushaski | False       | language | definitely endangered |   36.2161|   74.82360| bsk          | NA          | NA                  |                     0|                       0|                      3| IN PK             | buru1296   | bsk      | Eurasia   |

``` r
# Which inventories don't have Glottocodes?
index %>% filter(Glottocode=="")
```

    ##  [1] InventoryID          ISO6393              Glottocode          
    ##  [4] LanguageName         Source               family_id           
    ##  [7] parent_id            name                 bookkeeping         
    ## [10] level                status               latitude            
    ## [13] longitude            iso639P3code         description         
    ## [16] markup_description   child_family_count   child_language_count
    ## [19] child_dialect_count  country_ids          glottocode          
    ## [22] isocodes             macroarea           
    ## <0 rows> (or 0-length row.names)

``` r
kable(index %>% filter(is.na(Glottocode)))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName                  | Source | family\_id | parent\_id | name | bookkeeping | level | status |  latitude|  longitude| iso639P3code | description | markup\_description |  child\_family\_count|  child\_language\_count|  child\_dialect\_count| country\_ids | glottocode | isocodes | macroarea |
|------------:|:--------|:-----------|:------------------------------|:-------|:-----------|:-----------|:-----|:------------|:------|:-------|---------:|----------:|:-------------|:------------|:--------------------|---------------------:|-----------------------:|----------------------:|:-------------|:-----------|:---------|:----------|
|         2281| NA      | NA         | Modern Aramaic (Northeastern) | ea     | NA         | NA         | NA   | NA          | NA    | NA     |        NA|         NA| NA           | NA          | NA                  |                    NA|                      NA|                     NA| NA           | NA         | NA       | NA        |
|         2729| NA      | NA         | Djindewal                     | er     | NA         | NA         | NA   | NA          | NA    | NA     |        NA|         NA| NA           | NA          | NA                  |                    NA|                      NA|                     NA| NA           | NA         | NA       | NA        |

``` r
# Missing geo data?
index %>% filter(latitude=="")
```

    ##  [1] InventoryID          ISO6393              Glottocode          
    ##  [4] LanguageName         Source               family_id           
    ##  [7] parent_id            name                 bookkeeping         
    ## [10] level                status               latitude            
    ## [13] longitude            iso639P3code         description         
    ## [16] markup_description   child_family_count   child_language_count
    ## [19] child_dialect_count  country_ids          glottocode          
    ## [22] isocodes             macroarea           
    ## <0 rows> (or 0-length row.names)

``` r
nrow(index %>% filter(is.na(latitude)) %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, latitude, longitude))
```

    ## [1] 134

``` r
kable(index %>% filter(is.na(latitude)) %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, latitude, longitude))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName                   | Source |  latitude|  longitude|
|------------:|:--------|:-----------|:-------------------------------|:-------|---------:|----------:|
|           40| khl     | kali1299   | Kaliai                         | spa    |        NA|         NA|
|          136| xtc     | katc1250   | Katcha                         | spa    |        NA|         NA|
|          155| naq     | nama1265   | Nama                           | spa    |        NA|         NA|
|          705| eza     | ezaa1238   | ẹzaa                           | aa     |        NA|         NA|
|          735| iqw     | ikwo1238   | ikwo                           | aa     |        NA|         NA|
|          871| kpr     | kora1295   | Korafe                         | ph     |        NA|         NA|
|          875| enb     | endo1242   | Endo                           | ph     |        NA|         NA|
|          885| wya     | huro1249   | Huron                          | ph     |        NA|         NA|
|          916| kdt     | kuay1244   | Kuay                           | ph     |        NA|         NA|
|          947| nmg     | mvum1238   | Mvumbo                         | ph     |        NA|         NA|
|          973| mpt     | mian1257   | Mianmin                        | ph     |        NA|         NA|
|          996| str     | saan1246   | Saanich                        | ph     |        NA|         NA|
|         1097| arr     | karo1306   | Karo                           | ph     |        NA|         NA|
|         1102| biw     | bike1242   | Bikele                         | ph     |        NA|         NA|
|         1276| kck     | ikal1242   | Ikalanga                       | gm     |        NA|         NA|
|         1318| cce     | copi1238   | Copi                           | gm     |        NA|         NA|
|         1322| buy     | mman1238   | Mmani                          | gm     |        NA|         NA|
|         1325| gur     | fraf1238   | Frafra                         | gm     |        NA|         NA|
|         1382| oks     | okoo1245   | Oko                            | gm     |        NA|         NA|
|         1398| din     | dink1262   | Dinka                          | gm     |        NA|         NA|
|         1401| hna     | besl1239   | Besleri                        | gm     |        NA|         NA|
|         1456| gru     | sodd1242   | Soddo                          | gm     |        NA|         NA|
|         1460| sgw     | ezha1238   | Ezha                           | gm     |        NA|         NA|
|         1461| sgw     | chah1248   | Chaha                          | gm     |        NA|         NA|
|         1462| sgw     | gume1239   | Gumer                          | gm     |        NA|         NA|
|         1480| zay     | zays1236   | Zayse                          | gm     |        NA|         NA|
|         1523| afu     | efut1241   | Efutu                          | gm     |        NA|         NA|
|         1591| nyf     | kamb1298   | Kambe                          | gm     |        NA|         NA|
|         1592| nyf     | kaum1238   | Kauma                          | gm     |        NA|         NA|
|         1625| mgo     | mogh1246   | Moghamo                        | gm     |        NA|         NA|
|         1633| pnz     | pana1294   | Pana                           | gm     |        NA|         NA|
|         1701| mrr     | abuj1237   | Abujmaria                      | ra     |        NA|         NA|
|         1768| mrg     | miny1240   | Mising                         | ra     |        NA|         NA|
|         1771| nit     | naik1250   | Naiki                          | ra     |        NA|         NA|
|         1858| boa     | mira1254   | Miraña                         | saphon |        NA|         NA|
|         1994| nab     | khit1238   | Kithaulhu                      | saphon |        NA|         NA|
|         2015| shp     | ship1255   | Shipibo                        | saphon |        NA|         NA|
|         2092| arr     | karo1306   | Karo                           | saphon |        NA|         NA|
|         2225| kxd     | keda1250   | Kedayan                        | uz     |        NA|         NA|
|         2268| udm     | bese1243   | Beserman (Šamardan)            | ea     |        NA|         NA|
|         2281| NA      | NA         | Modern Aramaic (Northeastern)  | ea     |        NA|         NA|
|         2298| jih     | puxi1242   | Puxi                           | ea     |        NA|         NA|
|         2332| pbu     | nort2647   | Northwestern Pashto            | ea     |        NA|         NA|
|         2381| eus     | basq1250   | Zuberoan Basque                | ea     |        NA|         NA|
|         2388| NA      | east2773   | Dolakha Newar                  | ea     |        NA|         NA|
|         2434| NA      | vach1239   | Eastern Khanty (Vakh)          | ea     |        NA|         NA|
|         2464| mon     | mong1331   | Mongolian (Khalkha)            | ea     |        NA|         NA|
|         2473| ovd     | elfd1234   | Elfdalian                      | ea     |        NA|         NA|
|         2491| tvn     | merg1238   | Myeik Burmese                  | ea     |        NA|         NA|
|         2537| tks     | khal1271   | Khalkhal                       | ea     |        NA|         NA|
|         2544| NA      | nucl1729   | Tamang (Dhankuta)              | ea     |        NA|         NA|
|         2586| pbt     | sout2436   | Southwestern Pashto (Kandahar) | ea     |        NA|         NA|
|         2590| oss     | digo1242   | Digor Ossetic                  | ea     |        NA|         NA|
|         2593| ltg     | east2282   | Latgalian (Standard)           | ea     |        NA|         NA|
|         2596| nrf     | jerr1238   | Jèrriais (Eastern Jersey)      | ea     |        NA|         NA|
|         2634| mwf     | murr1259   | Murrinh-patha                  | er     |        NA|         NA|
|         2656| gup     | gund1246   | Gun-Dedjnjenghmi               | er     |        NA|         NA|
|         2657| gup     | gund1246   | Gun-Djeihmi                    | er     |        NA|         NA|
|         2658| gup     | gune1238   | Kune                           | er     |        NA|         NA|
|         2659| gup     | mura1269   | Kuninjku                       | er     |        NA|         NA|
|         2660| gup     | guma1252   | Kunwinjku                      | er     |        NA|         NA|
|         2661| gup     | naia1238   | Mayali                         | er     |        NA|         NA|
|         2670| ilg     | gari1254   | Garig                          | er     |        NA|         NA|
|         2671| ilg     | ilga1238   | Ilgar                          | er     |        NA|         NA|
|         2684| djd     | ngal1294   | Ngaliwurru                     | er     |        NA|         NA|
|         2688| wmb     | binb1242   | Binbinka                       | er     |        NA|         NA|
|         2689| wmb     | guda1243   | Gudanji                        | er     |        NA|         NA|
|         2692| NA      | ngum1253   | Ngumbarl                       | er     |        NA|         NA|
|         2703| amx     | east2380   | Eastern Anmatyerre             | er     |        NA|         NA|
|         2704| amx     | west2442   | Western Anmatyerre             | er     |        NA|         NA|
|         2706| aer     | mpar1238   | Central Arrernte               | er     |        NA|         NA|
|         2707| axe     | ayer1246   | Ayerrerenge                    | er     |        NA|         NA|
|         2712| xjb     | yugu1249   | Bundjalung                     | er     |        NA|         NA|
|         2713| gih     | gida1240   | Gidabal                        | er     |        NA|         NA|
|         2715| xjb     | minj1242   | Minjungbal                     | er     |        NA|         NA|
|         2716| bdy     | waal1238   | Waalubal                       | er     |        NA|         NA|
|         2722| wyb     | wayi1238   | Wayilwan                       | er     |        NA|         NA|
|         2724| kld     | wirr1237   | Wiriyaraay                     | er     |        NA|         NA|
|         2726| kld     | yuwa1243   | Yuwaliyaay                     | er     |        NA|         NA|
|         2728| jan     | jand1248   | Jandai                         | er     |        NA|         NA|
|         2729| NA      | NA         | Djindewal                      | er     |        NA|         NA|
|         2730| yxg     | yaga1262   | Turubul                        | er     |        NA|         NA|
|         2731| yxg     | yaga1262   | Yagara                         | er     |        NA|         NA|
|         2746| iin     | thii1234   | Thiin                          | er     |        NA|         NA|
|         2751| nmv     | kara1508   | Karangura                      | er     |        NA|         NA|
|         2752| nmv     | ngam1284   | Ngamini                        | er     |        NA|         NA|
|         2755| hrp     | nhir1234   | Nhirrpi                        | er     |        NA|         NA|
|         2759| xwk     | wong1246   | Kungardutji                    | er     |        NA|         NA|
|         2760| xwk     | wong1246   | Wangkumara                     | er     |        NA|         NA|
|         2767| gll     | kala1380   | Garlali                        | er     |        NA|         NA|
|         2774| dgw     | daun1234   | Daungwurrung                   | er     |        NA|         NA|
|         2778| NA      | yari1243   | Yari-Yari                      | er     |        NA|         NA|
|         2779| llj     | ladj1234   | Ladji-Ladji                    | er     |        NA|         NA|
|         2781| xwd     | wadi1260   | Wathi Wathi                    | er     |        NA|         NA|
|         2783| NA      | djad1246   | Jardwadjali                    | er     |        NA|         NA|
|         2784| rnr     | nari1241   | Nari Nari                      | er     |        NA|         NA|
|         2785| tjw     | djab1234   | Djabwurung                     | er     |        NA|         NA|
|         2787| xwt     | wotj1234   | Wotjobaluk                     | er     |        NA|         NA|
|         2789| unn     | gana1278   | Muk-Thang                      | er     |        NA|         NA|
|         2790| unn     | gana1278   | Thangguai                      | er     |        NA|         NA|
|         2791| unn     | gana1278   | Nulit                          | er     |        NA|         NA|
|         2792| NA      | kera1256   | Keramin                        | er     |        NA|         NA|
|         2793| NA      | lowe1402   | Ngayawang                      | er     |        NA|         NA|
|         2794| NA      | ngin1247   | Ngintait                       | er     |        NA|         NA|
|         2836| lkm     | kala1401   | Kalaamaya                      | er     |        NA|         NA|
|         2842| nlr     | ngar1286   | Ngarla                         | er     |        NA|         NA|
|         2852| gue     | wany1244   | Wanyjirra                      | er     |        NA|         NA|
|         2855| gue     | maln1239   | Malngin                        | er     |        NA|         NA|
|         2857| rxd     | ngar1288   | Ngardi                         | er     |        NA|         NA|
|         2865| xbp     | bibb1234   | Bibbulman                      | er     |        NA|         NA|
|         2866| xgg     | gore1235   | Goreng                         | er     |        NA|         NA|
|         2867| nys     | kani1276   | Kaniyang                       | er     |        NA|         NA|
|         2869| pnj     | pinj1244   | Pinjarup                       | er     |        NA|         NA|
|         2870| wxw     | ward1248   | Wardandi                       | er     |        NA|         NA|
|         2871| xwj     | waju1234   | Wajuk                          | er     |        NA|         NA|
|         2878| ikr     | ikar1243   | Ikarranggal                    | er     |        NA|         NA|
|         2879| ikr     | ikar1243   | Takalak                        | er     |        NA|         NA|
|         2910| dgt     | ndra1239   | Ndra'ngith                     | er     |        NA|         NA|
|         2913| NA      | luth1234   | Luthigh                        | er     |        NA|         NA|
|         2915| xpj     | mpal1238   | Mpalitjanh                     | er     |        NA|         NA|
|         2924| kjn     | ulku1238   | Olkol                          | er     |        NA|         NA|
|         2930| kjn     | oyka1239   | Oykangand                      | er     |        NA|         NA|
|         2941| gbw     | gabi1248   | Gabi-Gabi                      | er     |        NA|         NA|
|         2943| wkw     | duun1241   | Duungidjawu                    | er     |        NA|         NA|
|         2952| mpj     | kart1247   | Kartujarra                     | er     |        NA|         NA|
|         2953| mpj     | many1256   | Manjiljarra                    | er     |        NA|         NA|
|         2954| mpj     | pudi1238   | Putijarra                      | er     |        NA|         NA|
|         2955| mpj     | wang1288   | Wangkajunga                    | er     |        NA|         NA|
|         2966| mwp     | kala1378   | Kala Kawaw Ya                  | er     |        NA|         NA|
|         2969| wdk     | wadi1261   | Wadikali                       | er     |        NA|         NA|
|         2970| yxl     | yard1234   | Yardliyawarra                  | er     |        NA|         NA|
|         2973| dax     | dhal1246   | Dhay'yi                        | er     |        NA|         NA|
|         3018| wbp     | west2437   | Ngardily                       | er     |        NA|         NA|
|         3019| weg     | werg1234   | Piangil                        | er     |        NA|         NA|

``` r
# Missing language families

# Get Glottolog language family names for PHOIBLE Glottocodes
x <- index %>% select(InventoryID, Glottocode, family_id) %>% distinct()
y <- left_join(x, languoids, by=c("family_id"="id"))
y <- y %>% select(InventoryID, Glottocode, name)
colnames(y) <- c("InventoryID", "Glottocode", "LanguageFamily")

# Merge the language family names into the index
index <- left_join(index, y)
```

    ## Joining, by = c("InventoryID", "Glottocode")

``` r
expect_equal(nrow(index), 3020)

# Any blank or NA LanguageFamily?
nrow(index %>% filter(LanguageFamily==""))
```

    ## [1] 0

``` r
nrow(index %>% filter(is.na(LanguageFamily)))
```

    ## [1] 118

``` r
# TODO: identify the language family isolates
kable(index %>% filter(is.na(LanguageFamily)) %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, LanguageFamily))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName                  | Source | LanguageFamily |
|------------:|:--------|:-----------|:------------------------------|:-------|:---------------|
|            6| bsk     | buru1296   | Burushaski                    | spa    | NA             |
|           78| yuc     | yuch1247   | Yuchi                         | spa    | NA             |
|           80| tun     | tuni1252   | Tunica                        | spa    | NA             |
|           88| kyh     | karo1304   | Karok                         | spa    | NA             |
|           91| zun     | zuni1245   | Zuni                          | spa    | NA             |
|          102| pbb     | paez1247   | Paez                          | spa    | NA             |
|          103| ito     | iton1250   | Itonama                       | spa    | NA             |
|          179| eus     | basq1248   | Basque                        | spa    | NA             |
|          194| niv     | gily1242   | Gilyak                        | spa    | NA             |
|          224| ano     | ando1256   | ANDOKE                        | upsid  | NA             |
|          232| auc     | waor1240   | AUCA                          | upsid  | NA             |
|          264| bsk     | buru1296   | BURUSHASKI                    | upsid  | NA             |
|          289| con     | cofa1242   | COFAN                         | upsid  | NA             |
|          296| cyb     | cayu1262   | CAYUVAVA                      | upsid  | NA             |
|          321| eus     | basq1248   | BASQUE                        | upsid  | NA             |
|          326| faa     | fasu1242   | FASU                          | upsid  | NA             |
|          332| fun     | fuln1247   | IATE                          | upsid  | NA             |
|          357| hts     | hadz1240   | HADZA                         | upsid  | NA             |
|          371| irn     | iran1263   | IRANXE                        | upsid  | NA             |
|          374| ito     | iton1250   | ITONAMA                       | upsid  | NA             |
|          392| kbh     | cams1241   | CAMSA                         | upsid  | NA             |
|          416| kla     | klam1254   | KLAMATH                       | upsid  | NA             |
|          433| kun     | kuna1268   | KUNAMA                        | upsid  | NA             |
|          438| kyh     | karo1304   | KAROK                         | upsid  | NA             |
|          464| moq     | morb1239   | MOR                           | upsid  | NA             |
|          477| myp     | pira1253   | PIRAHA                        | upsid  | NA             |
|          479| mzp     | movi1243   | MOVIMA                        | upsid  | NA             |
|          496| niv     | gily1242   | NIVKH                         | upsid  | NA             |
|          501| nrb     | nara1262   | NERA                          | upsid  | NA             |
|          513| pbb     | paez1247   | PAEZ                          | upsid  | NA             |
|          532| sad     | sand1273   | SANDAWE                       | upsid  | NA             |
|          559| svs     | savo1255   | SAVOSAVO                      | upsid  | NA             |
|          563| tba     | aika1237   | HUARI                         | upsid  | NA             |
|          577| tiw     | tiwi1244   | TIWI                          | upsid  | NA             |
|          588| tpy     | trum1247   | TRUMAI                        | upsid  | NA             |
|          590| tqw     | tonk1249   | TONKAWA                       | upsid  | NA             |
|          596| tun     | tuni1252   | TUNICA                        | upsid  | NA             |
|          609| wba     | wara1303   | WARAO                         | upsid  | NA             |
|          621| wti     | bert1248   | BERTA                         | upsid  | NA             |
|          635| ynn     | yana1271   | YANA                          | upsid  | NA             |
|          641| yuc     | yuch1247   | YUCHI                         | upsid  | NA             |
|          648| zun     | zuni1245   | ZUNI                          | upsid  | NA             |
|          904| kxo     | kano1245   | Kanoe                         | ph     | NA             |
|          918| kto     | kuot1243   | Kuot                          | ph     | NA             |
|          919| kgg     | kusu1250   | Kusunda                       | ph     | NA             |
|          921| xwa     | kwaz1243   | Kwaza                         | ph     | NA             |
|          925| lvk     | lavu1241   | Lavukaleve                    | ph     | NA             |
|          938| ayz     | maib1239   | Maybrat                       | ph     | NA             |
|          944| cas     | mose1249   | Moseten                       | ph     | NA             |
|         1005| sbf     | shab1252   | Shabo                         | ph     | NA             |
|         1026| yuz     | yura1255   | Yuracure                      | ph     | NA             |
|         1030| was     | wash1253   | Washo                         | ph     | NA             |
|         1074| yuc     | yuch1247   | Euchee; Yuchi                 | ph     | NA             |
|         1082| cid     | chim1301   | Chimariko                     | ph     | NA             |
|         1123| mbe     | mola1238   | Molalla                       | ph     | NA             |
|         1147| sei     | seri1257   | Seri                          | ph     | NA             |
|         1210| blb     | bilu1245   | Bilua                         | ph     | NA             |
|         1218| tiw     | tiwi1244   | Tiwi                          | ph     | NA             |
|         1627| sif     | siam1242   | Siamou                        | gm     | NA             |
|         1652| sad     | sand1273   | Sandawe                       | gm     | NA             |
|         1895| con     | cofa1242   | Cofán                         | saphon | NA             |
|         1906| pue     | puel1244   | Günün Yajich                  | saphon | NA             |
|         1922| ano     | ando1256   | Andoke                        | saphon | NA             |
|         1923| ash     | abis1238   | Aʔɨwa                         | saphon | NA             |
|         1924| kbh     | cams1241   | Camsá                         | saphon | NA             |
|         1925| cbu     | cand1248   | Candoshi-Shapra               | saphon | NA             |
|         1926| cyb     | cayu1262   | Cayubaba                      | saphon | NA             |
|         1927| irn     | iran1263   | Mỹky                          | saphon | NA             |
|         1928| ito     | iton1250   | Itonama                       | saphon | NA             |
|         1929| lec     | leco1242   | Leco                          | saphon | NA             |
|         1931| omc     | moch1259   | Mochica                       | saphon | NA             |
|         1932| mzp     | movi1243   | Movima                        | saphon | NA             |
|         1933| myr     | muni1258   | Muniche                       | saphon | NA             |
|         1934| pbb     | paez1247   | Páez                          | saphon | NA             |
|         1935| pui     | puin1248   | Puinave                       | saphon | NA             |
|         1936| trr     | taus1253   | Taushiro                      | saphon | NA             |
|         1938| tit     | tini1245   | Tinigua                       | saphon | NA             |
|         1939| tpy     | trum1247   | Trumai                        | saphon | NA             |
|         1940| ura     | urar1246   | Urarina                       | saphon | NA             |
|         1941| vil     | vile1241   | Vilela                        | saphon | NA             |
|         1942| auc     | waor1240   | Waorani                       | saphon | NA             |
|         1943| wba     | wara1303   | Warao                         | saphon | NA             |
|         1944| yag     | yama1264   | Yahgan                        | saphon | NA             |
|         1945| yuz     | yura1255   | Yurakaré                      | saphon | NA             |
|         1957| cax     | chiq1248   | Bésɨro                        | saphon | NA             |
|         1959| gta     | guat1253   | Guató                         | saphon | NA             |
|         1979| fun     | fuln1247   | Yaathe                        | saphon | NA             |
|         1986| cas     | mose1249   | Mosetén de Covendo            | saphon | NA             |
|         1987| cas     | mose1249   | Mosetén de Santa Ana          | saphon | NA             |
|         1988| cas     | mose1249   | Tsimané                       | saphon | NA             |
|         1989| myp     | pira1253   | Pirahã                        | saphon | NA             |
|         2118| tba     | aika1237   | Aikanã                        | saphon | NA             |
|         2119| axg     | mato1253   | Arára do Mato Grosso          | saphon | NA             |
|         2121| kxo     | kano1245   | Kanoé                         | saphon | NA             |
|         2123| kuz     | kunz1244   | Kunza                         | saphon | NA             |
|         2124| xwa     | kwaz1243   | Kwaza                         | saphon | NA             |
|         2125| yae     | pume1238   | Pumé                          | saphon | NA             |
|         2161| eus     | basq1248   | Basque                        | uz     | NA             |
|         2198| kun     | kuna1268   | Kunama                        | uz     | NA             |
|         2203| nrb     | nara1262   | Nara                          | uz     | NA             |
|         2281| NA      | NA         | Modern Aramaic (Northeastern) | ea     | NA             |
|         2340| eus     | basq1248   | Basque (Baztan)               | ea     | NA             |
|         2370| niv     | gily1242   | Nivkh (West Sakhalin)         | ea     | NA             |
|         2419| eus     | basq1248   | Basque (Arbizu)               | ea     | NA             |
|         2561| bsk     | buru1296   | Burushaski (Werchikwar)       | ea     | NA             |
|         2577| bsk     | buru1296   | Burushaski (Nagar)            | ea     | NA             |
|         2583| eus     | basq1248   | Basque (Gernika)              | ea     | NA             |
|         2613| bsk     | buru1296   | Burushaski (Hunza)            | ea     | NA             |
|         2615| eus     | basq1248   | Basque (Ondarroa)             | ea     | NA             |
|         2636| wdj     | wadj1254   | Patjtjamalh                   | er     | NA             |
|         2642| lrg     | lara1258   | Larrakia                      | er     | NA             |
|         2644| umr     | umbu1235   | Umbugarla                     | er     | NA             |
|         2653| gbu     | gaga1251   | Gaagudju                      | er     | NA             |
|         2675| ggk     | kung1259   | Kungarakany                   | er     | NA             |
|         2691| NA      | mink1237   | Minkin                        | er     | NA             |
|         2729| NA      | NA         | Djindewal                     | er     | NA             |
|         3005| tiw     | tiwi1244   | Tiwi                          | er     | NA             |
|         3006| waq     | wage1238   | Wagiman                       | er     | NA             |

``` r
# Missing macroareas
nrow(index %>% filter(macroarea=="") %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, macroarea))
```

    ## [1] 7

``` r
kable(index %>% filter(macroarea=="") %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, macroarea))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName        | Source | macroarea |
|------------:|:--------|:-----------|:--------------------|:-------|:----------|
|         1768| mrg     | miny1240   | Mising              | ra     |           |
|         2268| udm     | bese1243   | Beserman (Šamardan) | ea     |           |
|         2716| bdy     | waal1238   | Waalubal            | er     |           |
|         2751| nmv     | kara1508   | Karangura           | er     |           |
|         2752| nmv     | ngam1284   | Ngamini             | er     |           |
|         2913| NA      | luth1234   | Luthigh             | er     |           |
|         2941| gbw     | gabi1248   | Gabi-Gabi           | er     |           |

``` r
#
nrow(index %>% filter(is.na(macroarea)) %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, macroarea))
```

    ## [1] 49

``` r
kable(index %>% filter(is.na(macroarea)) %>% select(InventoryID, ISO6393, Glottocode, LanguageName, Source, macroarea))
```

|  InventoryID| ISO6393 | Glottocode | LanguageName                  | Source | macroarea |
|------------:|:--------|:-----------|:------------------------------|:-------|:----------|
|           21| njz     | nyis1236   | Dafla                         | spa    | NA        |
|          171| hye     | nucl1235   | Armenian                      | spa    | NA        |
|          263| brx     | bodo1269   | BODO                          | upsid  | NA        |
|          302| njz     | nyis1236   | DAFLA                         | upsid  | NA        |
|          362| hye     | nucl1235   | ARMENIAN                      | upsid  | NA        |
|          387| kac     | kach1280   | JINGPHO                       | upsid  | NA        |
|          422| knn     | konk1267   | KONKANI                       | upsid  | NA        |
|          454| mcu     | came1252   | MAMBILA                       | upsid  | NA        |
|          488| nep     | east1436   | NEPALI                        | upsid  | NA        |
|          549| sma     | sout2674   | SAAMI                         | upsid  | NA        |
|          773| mcu     | came1252   | mambila                       | aa     | NA        |
|          774| mzk     | nige1255   | mambila                       | aa     | NA        |
|          893| inh     | ingu1240   | Ingush                        | ph     | NA        |
|          948| mlv     | motl1237   | Mwotlap                       | ph     | NA        |
|         1139| hrv     | croa1245   | Croatian                      | ph     | NA        |
|         1208| jya     | jiar1240   | Jiarong                       | ph     | NA        |
|         1254| fuv     | nige1253   | fulfulde (NGA)                | gm     | NA        |
|         1301| fuv     | nige1253   | Fula (Nigeria)                | gm     | NA        |
|         1398| din     | dink1262   | Dinka                         | gm     | NA        |
|         1455| nko     | nkon1248   | Nkonya                        | gm     | NA        |
|         1583| mcu     | came1252   | Mambila                       | gm     | NA        |
|         1586| coh     | chon1287   | Chonyi                        | gm     | NA        |
|         1722| adl     | galo1242   | Gallong                       | ra     | NA        |
|         1747| knn     | konk1267   | Konkani                       | ra     | NA        |
|         1789| nmf     | tang1336   | Tangkul Naga                  | ra     | NA        |
|         2107| pah     | tenh1241   | Tenharim                      | saphon | NA        |
|         2234| sje     | pite1240   | Pite Saami                    | uz     | NA        |
|         2235| sme     | nort2671   | Northern Saami                | uz     | NA        |
|         2251| inh     | ingu1240   | Ingush                        | ea     | NA        |
|         2257| jya     | jiar1240   | Caodeng rGyalrong             | ea     | NA        |
|         2260| sjd     | kild1236   | Kildin Saami                  | ea     | NA        |
|         2281| NA      | NA         | Modern Aramaic (Northeastern) | ea     | NA        |
|         2464| mon     | mong1331   | Mongolian (Khalkha)           | ea     | NA        |
|         2494| sjt     | ters1235   | Ter Saami                     | ea     | NA        |
|         2497| sje     | pite1240   | Pite Saami                    | ea     | NA        |
|         2499| srp     | serb1264   | Serbian (Standard Ekavian)    | ea     | NA        |
|         2540| kim     | kara1462   | Tofa                          | ea     | NA        |
|         2544| NA      | nucl1729   | Tamang (Dhankuta)             | ea     | NA        |
|         2566| sms     | skol1241   | Skolt Saami (Suõʹnnʼjel)      | ea     | NA        |
|         2599| xal     | kalm1243   | Kalmyk (Standard)             | ea     | NA        |
|         2729| NA      | NA         | Djindewal                     | er     | NA        |
|         2778| NA      | yari1243   | Yari-Yari                     | er     | NA        |
|         2792| NA      | kera1256   | Keramin                       | er     | NA        |
|         2794| NA      | ngin1247   | Ngintait                      | er     | NA        |
|         2882| NA      | kawa1290   | Ogh Awarrangg                 | er     | NA        |
|         2883| NA      | kawa1290   | Ogh Unyjan                    | er     | NA        |
|         2894| lby     | lamu1254   | Lamalama                      | er     | NA        |
|         2895| zmv     | mbar1253   | Rimanggudinhma                | er     | NA        |
|         3020| lby     | lamu1254   | Tableland Lamalama            | er     | NA        |
