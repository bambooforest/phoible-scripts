Aggregate Glottolog metadata into PHOIBLE stats
================
Steven Moran
17 May, 2020

``` r
library(dplyr)
library(knitr)
```

``` r
# See README.md
phoible <- read.csv('phoible-stats.csv')
```

``` r
# Glottolog languoids data (v 4.2)
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-18EC-5079-0173-0/glottolog_languoid.csv.zip
languoids <- read.csv('glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors=F)
languoids.cut <- languoids %>% select(id, family_id) %>% distinct()
# Top level language family ids are empty, so fill them with their ids
languoids.cut <- mutate(languoids.cut, family_id = ifelse(family_id == "", id, family_id))
languoids.families <- languoids %>% filter(family_id=="") %>% select(id, name) %>% rename(FamilyName=name)

# Glottolog geo data
geo <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-18EC-5079-0173-0/languages_and_dialects_geo.csv')
geo <- geo %>% rename(LanguageName=name) %>% select(-isocodes)
```

``` r
# Get WALS data for language genus field (note not all of PHOIBLE's data points are represented in WALS)
wals <- read.csv('https://github.com/cldf-datasets/wals/raw/master/cldf/languages.csv')
wals.cut <- wals %>% select(ISO639P3code, Genus)
```

``` r
# Joins
phoible <- left_join(phoible, geo, by=c("Glottocode"="glottocode"))
phoible <- left_join(phoible, wals.cut)
phoible <- left_join(phoible, languoids.cut, by=c("Glottocode"="id"))
phoible <- left_join(phoible, languoids.families, by=c("family_id"="id"))
```

``` r
# Note that NA in the tones column means that the inventory does not have information about tone (e.g. UPSID inventories).
phoible %>% head() %>% kable()
```

|  Inventory\_ID| ISO639P3code | Glottocode |  phonemes|  consonants|  vowels|  tones| LanguageName | level    | macroarea |  latitude|  longitude| Genus               | family\_id | FamilyName        |
|--------------:|:-------------|:-----------|---------:|-----------:|-------:|------:|:-------------|:---------|:----------|---------:|----------:|:--------------------|:-----------|:------------------|
|              1| kor          | kore1280   |        40|          22|      18|      0| Korean       | language | Eurasia   |   37.5000|  128.00000|                     | kore1284   | Koreanic          |
|              2| ket          | kett1243   |        32|          18|      14|      0| Ket          | language | Eurasia   |   63.7551|   87.54660| Yeniseian           | yeni1252   | Yeniseian         |
|              3| lbe          | lakk1252   |        69|          60|       9|      0| Lak          | language | Eurasia   |   42.1328|   47.08090| Lak-Dargwa          | nakh1245   | Nakh-Daghestanian |
|              4| kbd          | kaba1278   |        56|          49|       7|      0| Kabardian    | language | Eurasia   |   43.5082|   43.39180| Northwest Caucasian | abkh1242   | Abkhaz-Adyge      |
|              5| kat          | nucl1302   |        35|          29|       6|      0| Georgian     | language | Eurasia   |   41.8504|   43.78613| Kartvelian          | kart1248   | Kartvelian        |
|              6| bsk          | buru1296   |        53|          38|      12|      3| Burushaski   | language | Eurasia   |   36.2161|   74.82360|                     | buru1296   | Burushaski        |

``` r
write.csv(phoible, file="phoible-aggregated.csv", row.names = FALSE)
```
