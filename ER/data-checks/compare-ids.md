Compare phoible index with Australian inventories from Erich Round
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
```

Get phoible index for old inventories
=====================================

``` r
# Get phoible index for ER inventories
pi.index <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-LanguageCodes.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)

expect_equal(pi.index %>% filter(Source=="er") %>% nrow(), 392)
pi.index.er <- pi.index %>% filter(Source=="er")

# Identify any duplicate entries in phoible
expect_equal(pi.index.er %>% group_by(LanguageName) %>% filter(n()>1) %>% nrow(), 0)
```

``` r
# Get ER data
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20190118.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
head(new)
```

    ##   IPA_for_PHOIBLE Glottolog_code Glottolog_nearest    Variety_name
    ## 1               t̺͉       nang1259              <NA> Ngan'gityemerri
    ## 2               t̺͈       nang1259              <NA> Ngan'gityemerri
    ## 3          \u0236͈       nang1259              <NA> Ngan'gityemerri
    ## 4               k͈       nang1259              <NA> Ngan'gityemerri
    ## 5               p͉       nang1259              <NA> Ngan'gityemerri
    ## 6               p͈       nang1259              <NA> Ngan'gityemerri
    ##      Source                Source_ref
    ## 1 Reid 1990 reid_ngangityemerri:_1990
    ## 2 Reid 1990 reid_ngangityemerri:_1990
    ## 3 Reid 1990 reid_ngangityemerri:_1990
    ## 4 Reid 1990 reid_ngangityemerri:_1990
    ## 5 Reid 1990 reid_ngangityemerri:_1990
    ## 6 Reid 1990 reid_ngangityemerri:_1990
    ##                                                                   Comments
    ## 1 Please see also the general comparative notes on Australian inventories.
    ## 2 Please see also the general comparative notes on Australian inventories.
    ## 3 Please see also the general comparative notes on Australian inventories.
    ## 4 Please see also the general comparative notes on Australian inventories.
    ## 5 Please see also the general comparative notes on Australian inventories.
    ## 6 Please see also the general comparative notes on Australian inventories.

``` r
# Get distinct rows ER
## By multivalues
new.d <- new %>% select(Glottolog_code, Glottolog_nearest, Variety_name, Source_ref) %>% group_by(Glottolog_code, Glottolog_nearest, Variety_name, Source_ref) %>% distinct()

## By language variety name
new.d.langnames <- new %>% select(Variety_name) %>% distinct()

## Do they have the same contents?
expect_equal(nrow(new.d), 392)
expect_equal(nrow(new.d.langnames), 392)
expect_true(all(new.d$Variety_name %in% new.d.langnames$Variety_name))
expect_true(all(new.d.langnames$Variety_name %in% new.d$Variety_name))
```

What's different between phoible index and ER languages?
========================================================

``` r
# Are there phoible inventory IDs not in the new ER data?
expect_equal(nrow(pi.index.er[which(!(pi.index.er$LanguageName %in% new.d$Variety_name)),] %>% arrange(LanguageName)), 0)
pi.index.er[which(!(pi.index.er$LanguageName %in% new.d$Variety_name)),] %>% arrange(LanguageName)
```

    ## [1] InventoryID  LanguageCode Glottocode   LanguageName Source      
    ## <0 rows> (or 0-length row.names)

``` r
# Are there ER varieties not in the phoible index?
expect_equal(nrow(new.d[which(!(new.d$Variety_name %in% pi.index.er$LanguageName)),] %>% arrange(Variety_name)), 0)
new.d[which(!(new.d$Variety_name %in% pi.index.er$LanguageName)),] %>% arrange(Variety_name)
```

    ## # A tibble: 0 x 4
    ## # Groups:   Glottolog_code, Glottolog_nearest, Variety_name, Source_ref
    ## #   [0]
    ## # ... with 4 variables: Glottolog_code <chr>, Glottolog_nearest <chr>,
    ## #   Variety_name <chr>, Source_ref <chr>
