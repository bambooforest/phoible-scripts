Extend the phoible index with Glottolog data
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
library(knitr)
```

``` r
# Get phoible index (TODO: update to online version when recent PRs are merged)
# index <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors=F)
index <- read.csv('../../phoible/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors = F)
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
index <- left_join(index, glottolog, by=c("Glottocode"="glottocode"))
```

    ## Warning: Column `Glottocode`/`glottocode` joining character vector and
    ## factor, coercing into character vector

``` r
expect_equal(nrow(index), 3020)
```

``` r
languoids <- read.csv('glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors=F)
languoids <- languoids %>% select(id, family_id, parent_id, status,  country_ids, iso639P3code)
```

``` r
index <- left_join(index, languoids, by=c("Glottocode"="id"))
expect_equal(nrow(index), 3020)
glimpse(index)
```

    ## Observations: 3,020
    ## Variables: 16
    ## $ InventoryID  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...
    ## $ LanguageCode <chr> "kor", "ket", "lbe", "kbd", "kat", "bsk", "kru", ...
    ## $ Glottocode   <chr> "kore1280", "kett1243", "lakk1252", "kaba1278", "...
    ## $ LanguageName <chr> "Korean", "Ket", "Lak", "Kabardian", "Georgian", ...
    ## $ Source       <chr> "spa", "spa", "spa", "spa", "spa", "spa", "spa", ...
    ## $ name         <fct> Korean, Ket, Lak, Kabardian, Georgian, Burushaski...
    ## $ isocodes     <fct> kor, ket, lbe, kbd, kat, bsk, kru, tel, kfe, unr,...
    ## $ level        <fct> language, language, language, language, language,...
    ## $ macroarea    <fct> Eurasia, Eurasia, Eurasia, Eurasia, Eurasia, Eura...
    ## $ latitude     <dbl> 37.50000, 63.75510, 42.13280, 43.50820, 41.85040,...
    ## $ longitude    <dbl> 128.00000, 87.54660, 47.08090, 43.39180, 43.78613...
    ## $ family_id    <chr> "kore1284", "yeni1252", "nakh1245", "abkh1242", "...
    ## $ parent_id    <chr> "kore1284", "nort2746", "dagh1238", "circ1239", "...
    ## $ status       <chr> "safe", "definitely endangered", "vulnerable", "v...
    ## $ country_ids  <chr> "CN KP KR RU", "RU", "AZ GE KZ RU UA", "RU TR", "...
    ## $ iso639P3code <chr> "kor", "ket", "lbe", "kbd", "kat", "bsk", "kru", ...
