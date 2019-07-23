Process EA inventory data for PHOIBLE
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;
23 July, 2019

``` r
library(dplyr)
library(data.table)
```

``` r
# Data
ea.raw <- read.csv("../data/raw/phono_dbase.csv", header=T,  stringsAsFactors=FALSE)
```

``` r
# Add PHOIBLE Inventory ID
DT <- data.table(ea.raw)
DT[, InventoryID := .GRP, by = list(LanguageCode, LanguageName)]
DT$InventoryID <- DT$InventoryID+2238
tail(DT)
```

    ##    LanguageCode         LanguageName Segment InventoryID
    ## 1:          nyw Nyaw (Sakhon Nakhon)       ɛ        2628
    ## 2:          nyw Nyaw (Sakhon Nakhon)       a        2628
    ## 3:          nyw Nyaw (Sakhon Nakhon)       ɔ        2628
    ## 4:          nyw Nyaw (Sakhon Nakhon)      ia        2628
    ## 5:          nyw Nyaw (Sakhon Nakhon)      ɯa        2628
    ## 6:          nyw Nyaw (Sakhon Nakhon)      ua        2628

``` r
# Reorder for phoible import
DT <- DT[, c(4, 1:3)]
head(DT)
```

    ##    InventoryID LanguageCode             LanguageName Segment
    ## 1:        2239          hnj Hmong Njua (Green Hmong)       p
    ## 2:        2239          hnj Hmong Njua (Green Hmong)       t
    ## 3:        2239          hnj Hmong Njua (Green Hmong)       c
    ## 4:        2239          hnj Hmong Njua (Green Hmong)       k
    ## 5:        2239          hnj Hmong Njua (Green Hmong)       q
    ## 6:        2239          hnj Hmong Njua (Green Hmong)       ʔ

``` r
# Trim white space
DT %>% filter(grepl(" ʑ", Segment))
```

    ##   InventoryID LanguageCode  LanguageName Segment
    ## 1        2343          rue Rusyn (Lemko)       ʑ

``` r
DT$Segment <- trimws(DT$Segment)
DT %>% filter(grepl(" ʑ", Segment))
```

    ## [1] InventoryID  LanguageCode LanguageName Segment     
    ## <0 rows> (or 0-length row.names)

``` r
# Check ISO codes
iso639.3 <- read.csv("http://www-01.sil.org/iso639-3/iso-639-3.tab", sep="\t")
u.iso <- unique(iso639.3$Id)
# EA's codes
ea.iso <- unique(DT$LanguageCode)
ea <- as.data.frame(ea.iso)
"drh" %in% ea
```

    ## [1] FALSE

``` r
ea$in.iso <- ea$ea.iso %in% u.iso
ea %>% filter(!in.iso)
```

    ##   ea.iso in.iso
    ## 1      0  FALSE
    ## 2      -  FALSE

``` r
# There are values 0 and "-". Make them NAs instead.
```

``` r
# Fix ISO codes
## Check how many inventories lack codes.
x <- DT[DT$LanguageCode=="0" | DT$LanguageCode=="-"]
x %>% select(InventoryID) %>% unique() # 39 rows (this is verified via manual inspection)
```

    ##     InventoryID
    ##  1:        2256
    ##  2:        2264
    ##  3:        2268
    ##  4:        2281
    ##  5:        2292
    ##  6:        2298
    ##  7:        2327
    ##  8:        2332
    ##  9:        2350
    ## 10:        2352
    ## 11:        2379
    ## 12:        2381
    ## 13:        2384
    ## 14:        2388
    ## 15:        2407
    ## 16:        2411
    ## 17:        2413
    ## 18:        2420
    ## 19:        2421
    ## 20:        2434
    ## 21:        2439
    ## 22:        2440
    ## 23:        2450
    ## 24:        2473
    ## 25:        2489
    ## 26:        2491
    ## 27:        2502
    ## 28:        2504
    ## 29:        2519
    ## 30:        2525
    ## 31:        2534
    ## 32:        2544
    ## 33:        2553
    ## 34:        2558
    ## 35:        2587
    ## 36:        2590
    ## 37:        2591
    ## 38:        2600
    ## 39:        2614
    ##     InventoryID

``` r
## Reassign them to NA.
DT$LanguageCode[DT$LanguageCode=="0"] <- NA
DT$LanguageCode[DT$LanguageCode=="-"] <- NA
DT[DT$LanguageCode=="0" | DT$LanguageCode=="-"] # 0 rows
```

    ## Empty data.table (0 rows and 4 cols): InventoryID,LanguageCode,LanguageName,Segment

Write inventories to PHOIBLE format
-----------------------------------

``` r
# Recheck whitespace
DT %>% filter(grepl(" ʑ", Segment))
```

    ## [1] InventoryID  LanguageCode LanguageName Segment     
    ## <0 rows> (or 0-length row.names)

``` r
write.table(DT, file="../data/formatted/EA_inventories.tsv", row.names=FALSE, sep="\t", quote=F)
```
