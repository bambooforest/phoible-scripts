Compare the old and new Australian datasets from Erich Round
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
```

``` r
# Erich's data
old <- read.table("../data/raw/Australian_phonemes_for_PHOIBLE_20170501.txt", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20180114.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
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

Get phoible index for old inventories
=====================================

``` r
# Get phoible index for old inventories
pi.index <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-LanguageCodes.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
pi.index %>% filter(Source=="er") %>% nrow() # 388
```

    ## [1] 388

``` r
pi.index.er <- pi.index %>% filter(Source=="er")
head(pi.index.er)
```

    ##   InventoryID LanguageCode Glottocode  LanguageName Source
    ## 1        2629          bvr   bura1267       Burarra     er
    ## 2        2630          bck   buna1275        Bunuba     er
    ## 3        2631          gni   goon1238    Gooniyandi     er
    ## 4        2632          zml   madn1237      Matngele     er
    ## 5        2633          mpb   mull1237   Malak-Malak     er
    ## 6        2634          mwf   murr1259 Murrinh-patha     er

``` r
nrow(pi.index.er)
```

    ## [1] 388

``` r
# Join in new ER data
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20180114.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
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
# Distinct rows
new.d <- new %>% select(Glottolog_code, Glottolog_nearest, Variety_name, Source_ref) %>% group_by(Glottolog_code, Glottolog_nearest, Variety_name, Source_ref) %>% distinct()
nrow(new.d)
```

    ## [1] 392

``` r
head(new.d)
```

    ## # A tibble: 6 x 4
    ## # Groups:   Glottolog_code, Glottolog_nearest, Variety_name, Source_ref
    ## #   [6]
    ##   Glottolog_code Glottolog_nearest Variety_name    Source_ref             
    ##   <chr>          <chr>             <chr>           <chr>                  
    ## 1 nang1259       <NA>              Ngan'gityemerri reid_ngangityemerri:_1…
    ## 2 mull1237       <NA>              Malak-Malak     birk_phonology_1975    
    ## 3 gara1269       <NA>              Garrwa          mushin_grammar_2012    
    ## 4 wany1247       <NA>              Waanyi          breen_barkly:_2003     
    ## 5 0000kunindirri <NA>              Kunindirri      breen_barkly:_2003     
    ## 6 wara1290       <NA>              Warray          harvey_ngoni_1986

``` r
# Get unique lists
# old.d <- old %>% select(Glottolog_code, Variety_name) %>% group_by(Glottolog_code, Variety_name) %>% distinct()

# Write to disk
# write.csv(old.d, file='old.csv')
# write.csv(new.d, file='new.csv')
# write.csv(x, file='pi.csv')
```

What's different?
=================

``` r
# These were in the old data but are not in the new
pi.index.er[which(!(pi.index.er$LanguageName %in% new.d$Variety_name)),] %>% arrange(LanguageName)
```

    ##   InventoryID LanguageCode Glottocode   LanguageName Source
    ## 1        2988          kda   wori1245        Birrpai     er
    ## 2        2782         <NA>       <NA>   Djadjawurung     er
    ## 3        2732          dbl   dyir1250        Djirbal     er
    ## 4        2728         <NA>       <NA>        Nunukul     er
    ## 5        2962          ntj   ngaa1240 Nyanganyatjara     er
    ## 6        2781          tbh   wadi1249      Wadi Wadi     er
    ## 7        2992          kda   wori1245       Warrimay     er

``` r
# There are in the new data and not in the old
new.d[which(!(new.d$Variety_name %in% pi.index.er$LanguageName)),] %>% arrange(Variety_name)
```

    ## # A tibble: 11 x 4
    ## # Groups:   Glottolog_code, Glottolog_nearest, Variety_name, Source_ref
    ## #   [11]
    ##    Glottolog_code     Glottolog_nearest Variety_name   Source_ref         
    ##    <chr>              <chr>             <chr>          <chr>              
    ##  1 djap1238           <NA>              Djapu          morphy_djapu_1983  
    ##  2 dyir1250           <NA>              Dyirbal        dixon_dyirbal_1972 
    ##  3 0000east_djadjawu… west2443          East Djadjawu… blake_dialects_2011
    ##  4 guwa1243           <NA>              Guwamu         austin_guwamu_1980 
    ##  5 0000nunukul        yaga1256          Jandai         jefferies_guwar_20…
    ##  6 ngaa1240           <NA>              Ngaanyatjarra  glass_pitjantjatja…
    ##  7 west2437           <NA>              Ngardily       nash_preliminary_1…
    ##  8 0000piangil        wadi1260          Piangil        blake_dialects_2011
    ##  9 0000tableland_lam… <NA>              Tableland Lam… verstraete_mbarrum…
    ## 10 wadi1260           <NA>              Wathi Wathi    blake_mathi_2011   
    ## 11 0000west_djadjawu… west2443          West Djadjawu… blake_dialects_2011
