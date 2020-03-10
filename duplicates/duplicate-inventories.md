Which languages in PHOIBLE have multiple inventories?
================
Steven Moran
10 March, 2020

``` r
library(testthat)
library(dplyr)
library(knitr)
```

PHOIBLE has both ISO 639-3 language name identifiers (aka language codes) and Glottocodes for most phonological inventories in its dataset:

<https://github.com/phoible/dev/blob/master/mappings/InventoryID-LanguageCodes.csv>

ISO codes identify language-level entries. Glottocodes identify all languoids, i.e. all families, languages, and dialects. In PHOIBLE, we assign the "lowest" level Glottocode, e.g. the dialect level when it's known for a given phonological inventory.

This means that there are two levels of multiple inventories for data points in PHOIBLE: at the ISO versus the Glottocode levels.

``` r
# Get the latest phoible dev data
load(url('https://github.com/phoible/dev/blob/master/data/phoible.RData?raw=true'))
expect_equal(nrow(phoible), 105467) # latest phoible number of data points
```

``` r
# Collapse inventories and get each inventory's phoneme count
counts <- phoible %>% group_by(InventoryID, Glottocode, ISO6393) %>% summarize(phonemes = n()) %>% arrange(ISO6393, Glottocode, phonemes)
expect_equal(nrow(counts), 3020) # phoible 2.0 has 3020 data points
```

``` r
# Get the distribution of multiple inventories at the ISO level
dup.iso <- counts %>% group_by(ISO6393) %>% summarize(inventories = n()) %>% arrange(desc(inventories))
expect_equal(nrow(dup.iso), 2099) # phoible 2.0 number of unique iso codes (should be less than dup glottocodes)

# What's the distribution?
kable(table(dup.iso$inventories))
```

| Var1 |  Freq|
|:-----|-----:|
| 1    |  1546|
| 2    |   359|
| 3    |   124|
| 4    |    35|
| 5    |    19|
| 6    |     7|
| 7    |     2|
| 8    |     3|
| 9    |     1|
| 10   |     1|
| 12   |     1|
| 38   |     1|

``` r
# Get the distribution of multiple inventories at the Glottocode level
dup.glottocodes <- counts %>% group_by(Glottocode) %>% summarize(inventories = n()) %>% arrange(desc(inventories))
expect_equal(nrow(dup.glottocodes), 2184) # phoible 2.0 number of unique glottocodes

# What's the distribution?
kable(table(dup.glottocodes$inventories))
```

| Var1 |  Freq|
|:-----|-----:|
| 1    |  1651|
| 2    |   352|
| 3    |   117|
| 4    |    35|
| 5    |    18|
| 6    |     4|
| 7    |     2|
| 8    |     2|
| 9    |     1|
| 10   |     1|
| 11   |     1|

``` r
# Show some ISO singletons
single.iso <- dup.iso %>% filter(inventories == 1)
temp <- left_join(counts, single.iso)
```

    ## Joining, by = "ISO6393"

``` r
kable(temp %>% arrange(InventoryID) %>% filter(!is.na(inventories)) %>% head())
```

|  InventoryID| Glottocode | ISO6393 |  phonemes|  inventories|
|------------:|:-----------|:--------|---------:|------------:|
|           35| sund1252   | sun     |        26|            1|
|           42| maor1246   | mri     |        20|            1|
|           47| mara1386   | zmr     |        24|            1|
|           55| tele1256   | tlf     |        27|            1|
|           57| awiy1238   | auy     |        20|            1|
|           68| squa1248   | squ     |        37|            1|

``` r
# Do the ranges, means, and stuff for ISO
temp1 <- counts %>% group_by(ISO6393) %>% summarize(num_inventories = n(), min_phonemes = min(phonemes), max_phonemes = max(phonemes), mean_phonemes = mean(phonemes)) %>% arrange(desc(num_inventories))
temp2 <- counts %>% group_by(ISO6393) %>% summarize(range = paste(phonemes, collapse=","))
temp <- left_join(temp1, temp2)
```

    ## Joining, by = "ISO6393"

Note range may not be ordered correctly.

``` r
kable(temp %>% head(n=20))
```

| ISO6393 |  num\_inventories|  min\_phonemes|  max\_phonemes|  mean\_phonemes| range                                                                                                             |
|:--------|-----------------:|--------------:|--------------:|---------------:|:------------------------------------------------------------------------------------------------------------------|
| NA      |                38|             17|             68|        28.44737| 36,32,22,21,21,36,38,18,19,28,28,23,58,23,22,29,23,29,23,26,20,53,25,17,25,29,30,28,26,21,21,26,23,27,23,68,19,45 |
| oss     |                12|             39|             47|        43.75000| 42,39,39,40,43,44,44,46,47,47,47,47                                                                               |
| bzr     |                10|             18|             18|        18.00000| 18,18,18,18,18,18,18,18,18,18                                                                                     |
| eng     |                 9|             39|             45|        41.22222| 39,39,39,40,40,41,44,44,45                                                                                        |
| eus     |                 8|             28|             43|        32.25000| 28,28,28,30,30,35,36,43                                                                                           |
| khg     |                 8|             51|            133|        77.75000| 51,61,62,64,77,78,96,133                                                                                          |
| nld     |                 8|             37|             58|        49.37500| 37,39,43,51,54,56,57,58                                                                                           |
| gup     |                 7|             24|             28|        26.71429| 27,27,27,28,24,27,27                                                                                              |
| mhr     |                 7|             27|             34|        30.42857| 27,27,30,31,31,33,34                                                                                              |
| gwn     |                 6|             46|             65|        53.83333| 46,48,48,52,64,65                                                                                                 |
| kca     |                 6|             25|             36|        30.33333| 25,26,31,32,32,36                                                                                                 |
| lit     |                 6|             40|             60|        52.50000| 40,47,52,57,59,60                                                                                                 |
| lzz     |                 6|             32|             38|        36.00000| 32,34,36,38,38,38                                                                                                 |
| nyf     |                 6|             47|             47|        47.00000| 47,47,47,47,47,47                                                                                                 |
| nys     |                 6|             22|             22|        22.00000| 22,22,22,22,22,22                                                                                                 |
| sgw     |                 6|             39|             45|        42.33333| 44,39,42,42,42,45                                                                                                 |
| ben     |                 5|             43|             72|        52.60000| 43,47,48,53,72                                                                                                    |
| bod     |                 5|             34|             56|        44.60000| 34,41,42,50,56                                                                                                    |
| bsk     |                 5|             43|             59|        50.20000| 43,48,48,53,59                                                                                                    |
| bym     |                 5|             17|             21|        19.00000| 17,18,19,20,21                                                                                                    |

``` r
# Do the ranges, means, and stuff for Glottocodes
temp1 <- counts %>% group_by(Glottocode) %>% summarize(num_inventories = n(), min_phonemes = min(phonemes), max_phonemes = max(phonemes), mean_phonemes = mean(phonemes)) %>% arrange(desc(num_inventories))
temp2 <- counts %>% group_by(Glottocode) %>% summarize(range = paste(phonemes, collapse=","))
temp <- left_join(temp1, temp2)
```

    ## Joining, by = "Glottocode"

Note range may not be ordered correctly.

``` r
kable(temp %>% head(n=20))
```

| Glottocode |  num\_inventories|  min\_phonemes|  max\_phonemes|  mean\_phonemes| range                            |
|:-----------|-----------------:|--------------:|--------------:|---------------:|:---------------------------------|
| osse1243   |                11|             39|             47|        43.90909| 39,39,40,43,44,44,46,47,47,47,47 |
| biri1256   |                10|             18|             18|        18.00000| 18,18,18,18,18,18,18,18,18,18    |
| stan1293   |                 9|             39|             45|        41.22222| 39,39,39,40,40,41,44,44,45       |
| dutc1256   |                 8|             37|             58|        49.37500| 37,39,43,51,54,56,57,58          |
| kham1282   |                 8|             51|            133|        77.75000| 51,61,62,64,77,78,96,133         |
| basq1248   |                 7|             28|             36|        30.71429| 28,28,28,30,30,35,36             |
| east2328   |                 7|             27|             34|        30.42857| 27,27,30,31,31,33,34             |
| gwan1268   |                 6|             46|             65|        53.83333| 46,48,48,52,64,65                |
| khan1273   |                 6|             25|             36|        30.33333| 25,26,31,32,32,36                |
| lazz1240   |                 6|             32|             38|        36.00000| 32,34,36,38,38,38                |
| lith1251   |                 6|             40|             60|        52.50000| 40,47,52,57,59,60                |
| beng1280   |                 5|             43|             72|        52.60000| 43,47,48,53,72                   |
| bidy1243   |                 5|             17|             21|        19.00000| 17,18,19,20,21                   |
| buru1296   |                 5|             43|             59|        50.20000| 43,48,48,53,59                   |
| chec1245   |                 5|             44|             70|        61.80000| 44,64,64,67,70                   |
| gali1262   |                 5|             15|             28|        19.40000| 15,15,17,22,28                   |
| gang1268   |                 5|             18|             22|        18.80000| 18,18,18,18,22                   |
| haus1257   |                 5|             31|             46|        40.60000| 31,38,43,45,46                   |
| hind1269   |                 5|             55|             94|        68.40000| 55,58,61,74,94                   |
| iris1253   |                 5|             49|             69|        57.60000| 49,50,52,68,69                   |

``` r
# Clean up
rm(list = ls())
```
