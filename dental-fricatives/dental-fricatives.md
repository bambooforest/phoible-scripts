PHOIBLE dental fricatives
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
```

``` r
# Load the PHOIBLE development data from the GitHub repository
phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv', stringsAsFactors = F)

# Merge in Glottolog 3.3 data that has macroarea data
geo <- read.csv(url("https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv"), stringsAsFactors = FALSE)

phoible <- left_join(phoible, geo, by=c("Glottocode"="glottocode"))
```

``` r
# Which rows in phoible have dental fricatives?
dental.fricatives <- phoible %>% filter(grepl("θ|ð", Phoneme))
```

``` r
# How many inventories have dental fricatives?
nrow(dental.fricatives %>% select(InventoryID) %>% distinct())
```

    ## [1] 237

``` r
# How many distinct Glottocodes (languiods, i.e. languages, dialects) have dental fricatives?
gcodes.dental.fricatives <- dental.fricatives %>% select(Glottocode, macroarea) %>% distinct()
```

``` r
# How are they distributed in phoible?
distribution.dental.fricatives <- dental.fricatives %>% group_by(Phoneme) %>% summarize(count=n()) %>% arrange(desc(count))
distribution.dental.fricatives
```

    ## # A tibble: 29 x 2
    ##    Phoneme count
    ##    <chr>   <int>
    ##  1 ð         160
    ##  2 θ         123
    ##  3 ð̞           7
    ##  4 ð̪̺           6
    ##  5 t̪θ          5
    ##  6 t̪θʼ         5
    ##  7 ðː          4
    ##  8 ðʲ          4
    ##  9 t̪θʰ         4
    ## 10 d̪ð          3
    ## # … with 19 more rows

``` r
# How are they distributed via macroarea
table(gcodes.dental.fricatives$macroarea)
```

    ## 
    ##                      Africa     Australia       Eurasia North America 
    ##             1            53            28            48            17 
    ##     Papunesia South America 
    ##             9            15
