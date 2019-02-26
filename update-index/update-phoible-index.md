Update phoible index with Glottolog data
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
# Get phoible index (TODO: update this to a CSV file; update link to master branch when merged)
index <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors=F)
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
kable(index[which(!(index$LanguageCode == index$isocodes)),] %>% select(InventoryID, LanguageCode, isocodes, Glottocode, LanguageName, Source, level))
```

|      |  InventoryID| LanguageCode | isocodes | Glottocode | LanguageName        | Source | level    |
|------|------------:|:-------------|:---------|:-----------|:--------------------|:-------|:---------|
| 40   |           40| khl          |          | kali1299   | Kaliai              | spa    | dialect  |
| 136  |          136| xtc          |          | katc1250   | Katcha              | spa    | dialect  |
| 155  |          155| naq          |          | nama1265   | Nama                | spa    | dialect  |
| 298  |          298| lda          | daf      | dann1241   | DAN                 | upsid  | language |
| 613  |          613| wnw          | wit      | wint1259   | WINTU               | upsid  | language |
| 691  |          691| dnj          | daf      | dann1241   | dan                 | aa     | language |
| 871  |          871| kpr          |          | kora1295   | Korafe              | ph     | dialect  |
| 875  |          875| enb          |          | endo1242   | Endo                | ph     | dialect  |
| 885  |          885| wya          |          | huro1249   | Huron               | ph     | dialect  |
| 916  |          916| kdt          |          | kuay1244   | Kuay                | ph     | dialect  |
| 947  |          947| nmg          |          | mvum1238   | Mvumbo              | ph     | dialect  |
| 973  |          973| mpt          |          | mian1257   | Mianmin             | ph     | dialect  |
| 996  |          996| str          |          | saan1246   | Saanich             | ph     | dialect  |
| 1097 |         1097| arr          |          | karo1306   | Karo                | ph     | dialect  |
| 1102 |         1102| biw          |          | bike1242   | Bikele              | ph     | dialect  |
| 1192 |         1192| nbj          |          | bili1250   | Bilinara            | ph     | dialect  |
| 1276 |         1276| kck          |          | ikal1242   | Ikalanga            | gm     | dialect  |
| 1318 |         1318| cce          |          | copi1238   | Copi                | gm     | dialect  |
| 1322 |         1322| buy          |          | mman1238   | Mmani               | gm     | dialect  |
| 1325 |         1325| gur          |          | fraf1238   | Frafra              | gm     | dialect  |
| 1382 |         1382| oks          |          | okoo1245   | Oko                 | gm     | dialect  |
| 1393 |         1393| lda          | daf      | dann1241   | Dan                 | gm     | language |
| 1401 |         1401| hna          |          | besl1239   | Besleri             | gm     | dialect  |
| 1455 |         1455| nko          |          | nkon1248   | Nkonya              | gm     | dialect  |
| 1456 |         1456| gru          |          | sodd1242   | Soddo               | gm     | dialect  |
| 1460 |         1460| sgw          |          | ezha1238   | Ezha                | gm     | dialect  |
| 1461 |         1461| sgw          |          | chah1248   | Chaha               | gm     | dialect  |
| 1462 |         1462| sgw          |          | gume1239   | Gumer               | gm     | dialect  |
| 1480 |         1480| zay          |          | zays1236   | Zayse               | gm     | dialect  |
| 1523 |         1523| afu          |          | efut1241   | Efutu               | gm     | dialect  |
| 1591 |         1591| nyf          |          | kamb1298   | Kambe               | gm     | dialect  |
| 1592 |         1592| nyf          |          | kaum1238   | Kauma               | gm     | dialect  |
| 1625 |         1625| mgo          |          | mogh1246   | Moghamo             | gm     | dialect  |
| 1633 |         1633| pnz          |          | pana1294   | Pana                | gm     | dialect  |
| 1701 |         1701| mrr          |          | abuj1237   | Abujmaria           | ra     | dialect  |
| 1768 |         1768| mrg          |          | miny1240   | Mising              | ra     | dialect  |
| 1771 |         1771| nit          |          | naik1250   | Naiki               | ra     | dialect  |
| 1772 |         1772| nep          | npi      | nepa1254   | Nepali              | ra     | language |
| 1858 |         1858| boa          |          | mira1254   | Miraña              | saphon | dialect  |
| 1994 |         1994| nab          |          | khit1238   | Kithaulhu           | saphon | dialect  |
| 2015 |         2015| shp          |          | ship1255   | Shipibo             | saphon | dialect  |
| 2092 |         2092| arr          |          | karo1306   | Karo                | saphon | dialect  |
| 2204 |         2204| nep          | npi      | nepa1254   | Nepali              | uz     | language |
| 2225 |         2225| kxd          |          | keda1250   | Kedayan             | uz     | dialect  |
| 2268 |         2268| udm          |          | bese1243   | Beserman            | ea     | dialect  |
| 2298 |         2298| jih          |          | puxi1242   | Puxi                | ea     | dialect  |
| 2332 |         2332| pbu          |          | nort2647   | Northwestern Pashto | ea     | dialect  |
| 2381 |         2381| eus          |          | basq1250   | Zuberoan Basque     | ea     | dialect  |
| 2384 |         2384| kom          |          | komi1277   | Yodzyak Komi        | ea     | language |
| 2491 |         2491| tvn          |          | merg1238   | Myeik Burmese       | ea     | dialect  |
| 2519 |         2519| kpt          | khg      | kham1282   | Soghpo Tibetan      | ea     | language |
| 2537 |         2537| tks          |          | khal1271   | Khalkhal            | ea     | dialect  |
| 2586 |         2586| pbt          |          | sout2436   | Southwestern Pashto | ea     | dialect  |
| 2590 |         2590| oss          |          | digo1242   | Digor Ossetic       | ea     | dialect  |
| 2634 |         2634| mwf          |          | murr1259   | Murrinh-patha       | er     | dialect  |
| 2656 |         2656| gup          |          | gund1246   | Gun-Dedjnjenghmi    | er     | dialect  |
| 2657 |         2657| gup          |          | gund1246   | Gun-Djeihmi         | er     | dialect  |
| 2658 |         2658| gup          |          | gune1238   | Kune                | er     | dialect  |
| 2659 |         2659| gup          |          | mura1269   | Kuninjku            | er     | dialect  |
| 2660 |         2660| gup          |          | guma1252   | Kunwinjku           | er     | dialect  |
| 2661 |         2661| gup          |          | naia1238   | Mayali              | er     | dialect  |
| 2670 |         2670| ilg          |          | gari1254   | Garig               | er     | dialect  |
| 2671 |         2671| ilg          |          | ilga1238   | Ilgar               | er     | dialect  |
| 2684 |         2684| nug          |          | ngal1294   | Ngaliwurru          | er     | dialect  |
| 2688 |         2688| wmb          |          | binb1242   | Binbinka            | er     | dialect  |
| 2689 |         2689| wmb          |          | guda1243   | Gudanji             | er     | dialect  |
| 2703 |         2703| amx          |          | east2380   | Eastern Anmatyerre  | er     | dialect  |
| 2704 |         2704| amx          |          | west2442   | Western Anmatyerre  | er     | dialect  |
| 2706 |         2706| aer          |          | mpar1238   | Central Arrernte    | er     | dialect  |
| 2707 |         2707| adg          | axe      | ayer1246   | Ayerrerenge         | er     | dialect  |
| 2712 |         2712| bdy          |          | yugu1249   | Bundjalung          | er     | dialect  |
| 2713 |         2713| gih          |          | gida1240   | Gidabal             | er     | dialect  |
| 2714 |         2714| bdy          |          | guwa1244   | Guwar               | er     | language |
| 2718 |         2718| xbg          |          | warr1257   | Warrnambool         | er     | language |
| 2722 |         2722| wyb          |          | wayi1238   | Wayilwan            | er     | dialect  |
| 2724 |         2724| kld          |          | wirr1237   | Wiriyaraay          | er     | dialect  |
| 2725 |         2725| kld          |          | yuwa1242   | Yuwaalaraay         | er     | dialect  |
| 2726 |         2726| kld          |          | yuwa1243   | Yuwaliyaay          | er     | dialect  |
| 2727 |         2727| xyy          |          | dhud1236   | Dhudhuroa           | er     | language |
| 2751 |         2751| nmv          |          | kara1508   | Karangura           | er     | dialect  |
| 2752 |         2752| nmv          |          | ngam1284   | Ngamini             | er     | dialect  |
| 2755 |         2755| ynd          | hrp      | nhir1234   | Nhirrpi             | er     | dialect  |
| 2774 |         2774| wyi          | dgw      | daun1234   | Daungwurrung        | er     | dialect  |
| 2781 |         2781| tbh          | xwd      | wadi1260   | Wathi Wathi         | er     | dialect  |
| 2792 |         2792| nay          |          | nort2756   | Keramin             | er     | language |
| 2829 |         2829| mnt          | xyk      | mayi1236   | Mayi-Kulan          | er     | dialect  |
| 2832 |         2832| mnt          | xyj      | mayi1235   | Mayi-Yapi           | er     | dialect  |
| 2851 |         2851| nbj          |          | bili1250   | Bilinarra           | er     | dialect  |
| 2852 |         2852| gue          |          | wany1244   | Wanyjirra           | er     | dialect  |
| 2855 |         2855| gue          |          | maln1239   | Malngin             | er     | dialect  |
| 2867 |         2867| nys          |          | kani1276   | Kaniyang            | er     | dialect  |
| 2918 |         2918| amz          |          | angg1238   | Angkamuthi          | er     | language |
| 2920 |         2920| amz          |          | yadh1237   | Yadhaykenu          | er     | language |
| 2924 |         2924| kjn          |          | ulku1238   | Olkol               | er     | dialect  |
| 2930 |         2930| kjn          |          | oyka1239   | Oykangand           | er     | dialect  |
| 2941 |         2941| gbw          |          | gabi1248   | Gabi-Gabi           | er     | dialect  |
| 2943 |         2943| wkw          |          | duun1241   | Duungidjawu         | er     | dialect  |
| 2952 |         2952| mpj          |          | kart1247   | Kartujarra          | er     | dialect  |
| 2953 |         2953| mpj          |          | many1256   | Manjiljarra         | er     | dialect  |
| 2954 |         2954| mpj          |          | pudi1238   | Putijarra           | er     | dialect  |
| 2955 |         2955| mpj          |          | wang1288   | Wangkajunga         | er     | dialect  |
| 2966 |         2966| mwp          |          | kala1378   | Kala Kawaw Ya       | er     | dialect  |
| 2973 |         2973| dax          |          | dhal1246   | Dhay'yi             | er     | dialect  |
| 2992 |         2992| duj          |          | djap1238   | Djapu               | er     | dialect  |
| 3018 |         3018| wbp          |          | west2437   | Ngardily            | er     | dialect  |

``` r
kable(index[which(!(index$LanguageCode == index$isocodes)),] %>% filter(isocodes!="") %>% select(InventoryID, LanguageCode, Glottocode, LanguageName, name, level, isocodes))
```

|  InventoryID| LanguageCode | Glottocode | LanguageName   | name          | level    | isocodes |
|------------:|:-------------|:-----------|:---------------|:--------------|:---------|:---------|
|          298| lda          | dann1241   | DAN            | Dan           | language | daf      |
|          613| wnw          | wint1259   | WINTU          | Wintu         | language | wit      |
|          691| dnj          | dann1241   | dan            | Dan           | language | daf      |
|         1393| lda          | dann1241   | Dan            | Dan           | language | daf      |
|         1772| nep          | nepa1254   | Nepali         | Nepali        | language | npi      |
|         2204| nep          | nepa1254   | Nepali         | Nepali        | language | npi      |
|         2519| kpt          | kham1282   | Soghpo Tibetan | Khams Tibetan | language | khg      |
|         2707| adg          | ayer1246   | Ayerrerenge    | Ayerrerenge   | dialect  | axe      |
|         2755| ynd          | nhir1234   | Nhirrpi        | Nhirrpi       | dialect  | hrp      |
|         2774| wyi          | daun1234   | Daungwurrung   | Daungwurrung  | dialect  | dgw      |
|         2781| tbh          | wadi1260   | Wathi Wathi    | Wadi Wadi     | dialect  | xwd      |
|         2829| mnt          | mayi1236   | Mayi-Kulan     | Mayi-Kulan    | dialect  | xyk      |
|         2832| mnt          | mayi1235   | Mayi-Yapi      | Mayi-Yapi     | dialect  | xyj      |

``` r
# Identify the parent languages of dialects
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/glottolog_languoid.csv.zip
languoids <- read.csv('glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors=F)
head(languoids)
```

    ##         id family_id parent_id       name bookkeeping    level  status
    ## 1 aala1237  aust1307  ramo1244     Aalawa       False  dialect    safe
    ## 2 aant1238  nucl1709  nort2920 Aantantara       False  dialect    safe
    ## 3 aari1238  sout2845  ahkk1235 Aari-Gayil       False   family    safe
    ## 4 aari1239  sout2845  aari1238       Aari       False language    safe
    ## 5 aari1240  book1242  book1242     Aariya        True language    safe
    ## 6 aasa1238  afro1255  unun9872      Aasax       False language extinct
    ##   latitude longitude iso639P3code description markup_description
    ## 1       NA        NA                       NA                 NA
    ## 2       NA        NA                       NA                 NA
    ## 3       NA        NA          aiz          NA                 NA
    ## 4  5.95034   36.5721          aiw          NA                 NA
    ## 5       NA        NA          aay          NA                 NA
    ## 6 -4.00679   36.8648          aas          NA                 NA
    ##   child_family_count child_language_count child_dialect_count country_ids
    ## 1                  0                    0                   0            
    ## 2                  0                    0                   0            
    ## 3                  0                    2                   0            
    ## 4                  0                    0                   0          ET
    ## 5                  0                    0                   0          IN
    ## 6                  0                    0                   0          TZ

``` r
# First check that all present phoible Glottocodes are valid 
table(index$Glottocode %in% languoids$id)
```

    ## 
    ## FALSE  TRUE 
    ##     4  3016

``` r
index[which(!(index$Glottocode %in% languoids$id)),]
```

    ##      InventoryID LanguageCode Glottocode   LanguageName Source name level
    ## 2281        2281         <NA>       <NA> Modern Aramaic     ea <NA>  <NA>
    ## 2715        2715          xjb    wee1234     Minjungbal     er <NA>  <NA>
    ## 2729        2729         <NA>       <NA>      Djindewal     er <NA>  <NA>
    ## 2961        2961         <NA>       <NA>   Ngarlawangka     er <NA>  <NA>
    ##      macroarea latitude longitude isocodes
    ## 2281      <NA>       NA        NA     <NA>
    ## 2715      <NA>       NA        NA     <NA>
    ## 2729      <NA>       NA        NA     <NA>
    ## 2961      <NA>       NA        NA     <NA>

``` r
# How many levels do we have in the index?
table(index$level)
```

    ## 
    ##  dialect language 
    ##      140     2865

``` r
# These are the dialects in the phoible index
dialects <- index %>% filter(level=="dialect") %>% select(InventoryID, LanguageName, Glottocode, LanguageCode, isocodes)
nrow(dialects)
```

    ## [1] 140

``` r
# All missing isocodes -- TODO
index %>% select(InventoryID, LanguageName, Glottocode, LanguageCode, isocodes, level) %>% filter(is.na(isocodes))
```

    ##    InventoryID      LanguageName Glottocode LanguageCode isocodes level
    ## 1          248        BANDJALANG   band1339          bdy     <NA>  <NA>
    ## 2          488            NEPALI   east1436          nep     <NA>  <NA>
    ## 3         1208           Jiarong   jiar1240          jya     <NA>  <NA>
    ## 4         1398             Dinka   dink1262          din     <NA>  <NA>
    ## 5         2257 Caodeng rGyalrong   jiar1240          jya     <NA>  <NA>
    ## 6         2281    Modern Aramaic       <NA>         <NA>     <NA>  <NA>
    ## 7         2464         Mongolian   mong1331          mon     <NA>  <NA>
    ## 8         2544            Tamang   nucl1729         <NA>     <NA>  <NA>
    ## 9         2715        Minjungbal    wee1234          xjb     <NA>  <NA>
    ## 10        2716          Waalubal   band1339          bdy     <NA>  <NA>
    ## 11        2729         Djindewal       <NA>         <NA>     <NA>  <NA>
    ## 12        2778         Yari-Yari   yari1243         <NA>     <NA>  <NA>
    ## 13        2794          Ngintait   ngin1247         <NA>     <NA>  <NA>
    ## 14        2917            Uradhi   urad1238          amz     <NA>  <NA>
    ## 15        2961      Ngarlawangka       <NA>         <NA>     <NA>  <NA>

``` r
# Get parent language data to get ISO code
nrow(dialects)
```

    ## [1] 140

``` r
nrow(languoids %>% filter(id %in% dialects$Glottocode))
```

    ## [1] 132

``` r
head(languoids)
```

    ##         id family_id parent_id       name bookkeeping    level  status
    ## 1 aala1237  aust1307  ramo1244     Aalawa       False  dialect    safe
    ## 2 aant1238  nucl1709  nort2920 Aantantara       False  dialect    safe
    ## 3 aari1238  sout2845  ahkk1235 Aari-Gayil       False   family    safe
    ## 4 aari1239  sout2845  aari1238       Aari       False language    safe
    ## 5 aari1240  book1242  book1242     Aariya        True language    safe
    ## 6 aasa1238  afro1255  unun9872      Aasax       False language extinct
    ##   latitude longitude iso639P3code description markup_description
    ## 1       NA        NA                       NA                 NA
    ## 2       NA        NA                       NA                 NA
    ## 3       NA        NA          aiz          NA                 NA
    ## 4  5.95034   36.5721          aiw          NA                 NA
    ## 5       NA        NA          aay          NA                 NA
    ## 6 -4.00679   36.8648          aas          NA                 NA
    ##   child_family_count child_language_count child_dialect_count country_ids
    ## 1                  0                    0                   0            
    ## 2                  0                    0                   0            
    ## 3                  0                    2                   0            
    ## 4                  0                    0                   0          ET
    ## 5                  0                    0                   0          IN
    ## 6                  0                    0                   0          TZ
