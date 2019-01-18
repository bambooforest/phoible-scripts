Compare the old and new Australian datasets from Erich Round
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
```

``` r
# Erich's data
old <- read.table("../data/raw/Australian_phonemes_for_PHOIBLE_20170501.txt", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20190118.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
```

How many distinct things are there?
===================================

Glottocodes + variety names?
----------------------------

``` r
nrow(old %>% select(Glottolog_code, Variety_name) %>% group_by(Glottolog_code, Variety_name) %>% distinct()) # 388
```

    ## [1] 388

``` r
nrow(new %>% select(Glottolog_code, Variety_name) %>% group_by(Glottolog_code, Variety_name) %>% distinct()) # 392
```

    ## [1] 392

Glottocodes?
------------

``` r
nrow(old %>% select(Glottolog_code) %>% distinct()) # 388
```

    ## [1] 388

``` r
nrow(new %>% select(Glottolog_code) %>% distinct()) # 392
```

    ## [1] 392

Nearest Glottocodes
-------------------

``` r
nrow(old %>% select(Glottolog_nearest) %>% distinct()) # 45
```

    ## [1] 45

``` r
nrow(new %>% select(Glottolog_nearest) %>% distinct()) # 45
```

    ## [1] 45

``` r
# old %>% select(Glottolog_nearest) %>% distinct() %>% arrange(Glottolog_nearest)
# new %>% select(Glottolog_nearest) %>% distinct() %>% arrange(Glottolog_nearest)
```

Language names?
---------------

``` r
nrow(old %>% select(Variety_name) %>% group_by(Variety_name) %>% distinct()) # 388
```

    ## [1] 388

``` r
nrow(new %>% select(Variety_name) %>% group_by(Variety_name) %>% distinct()) # 392
```

    ## [1] 392

Sources
-------

``` r
nrow(old %>% select(Source) %>% group_by(Source) %>% distinct()) # 232
```

    ## [1] 232

``` r
nrow(new %>% select(Source) %>% group_by(Source) %>% distinct()) # 233
```

    ## [1] 233

Source refs
-----------

``` r
nrow(old %>% select(Source_ref) %>% group_by(Source_ref) %>% distinct()) # 235
```

    ## [1] 235

``` r
nrow(new %>% select(Source_ref) %>% group_by(Source_ref) %>% distinct()) # 232
```

    ## [1] 232

Comments
--------

``` r
nrow(old %>% select(Comments) %>% group_by(Comments) %>% distinct()) # 59
```

    ## [1] 59

``` r
nrow(new %>% select(Comments) %>% group_by(Comments) %>% distinct()) # 59
```

    ## [1] 59

``` r
# Get unique lists
# old.d <- old %>% select(Glottolog_code, Variety_name) %>% group_by(Glottolog_code, Variety_name) %>% distinct()

# old.d <- new %>% select(Glottolog_code, Variety_name) %>% group_by(Glottolog_code, Variety_name) %>% distinct()

# Write to disk
# write.csv(old.d, file='old.csv')
# write.csv(new.d, file='new.csv')
```
