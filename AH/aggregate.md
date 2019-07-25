Aggregate HUPC phoible resources and run tests
================
Steven Moran
25 July, 2019

``` r
library(dplyr)
library(knitr)
library(testthat)
```

HUPC
----

``` r
inventories <- read.csv("raw-data/PHOIBLE inventories - inventories.csv")
metadata <- read.csv("raw-data/PHOIBLE inventories - metadata.csv")
```

Tests
-----

``` r
# How many unique pairings do we have? (TODO: may not be needed if we remove these values from `inventories`)
ids <- inventories %>% select(Glottocode, LanguageName, FileNames) %>% distinct()
nrow(ids)
```

    ## [1] 73

``` r
# Are there NAs in the input for Phoneme?
expect_false(any(is.na(inventories$Phoneme)))
```

``` r
# Check for duplicate phonemes in the input
inventories %>% group_by(InventoryID, Phoneme, Allophones) %>% filter(n()>1)
```

    ## Warning: Factor `Phoneme` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## Warning: Factor `Allophones` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

    ## # A tibble: 0 x 9
    ## # Groups:   InventoryID, Phoneme, Allophones [1]
    ## # … with 9 variables: InventoryID <int>, Glottocode <fct>,
    ## #   LanguageName <fct>, SpecificDialect <fct>, Phoneme <fct>, Notes <fct>,
    ## #   Allophones <fct>, ExplanationOfAllophones <fct>, FileNames <fct>

``` r
expect_equal(nrow(inventories %>% group_by(InventoryID, Phoneme, Allophones) %>% filter(n()>1)), 0)
```

``` r
# Spot check the number of segments per entry
inventories.segments <- inventories %>% group_by(InventoryID, LanguageName, FileNames) %>% select(InventoryID, LanguageName, FileNames) %>% summarize(segments = n())
kable(head(inventories.segments))
```

|  InventoryID| LanguageName | FileNames               |  segments|
|------------:|:-------------|:------------------------|---------:|
|         3022| Kalamang     | Visser2016.pdf          |        23|
|         3023| Belorussian  | mayo1993belorussian.pdf |        45|
|         3024| Cahuilla     | seiler1977cahuilla.pdf  |        39|
|         3025| Camling      | ebert1997camling.pdf    |        38|
|         3026| Coatlán Mixe | elson1992mixezoque.pdf  |        21|
|         3027| Coptic       | layton2000coptic.pdf    |        25|

``` r
# Inventories in the input range this much in size:
range(inventories.segments$segments)
```

    ## [1] 21 87

``` r
# Check if all segments in the input are already appear in phoible (i.e. cheap check for phoible conventions)
phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv', header = T, stringsAsFactors = F)
phoible.segments <- phoible %>% select(Phoneme) %>% distinct()

# Preprocess (drop the brackets for marginal sounds)
inventories.segments <- inventories %>% select(Phoneme) %>% transmute(Phonemes = gsub("<|>", "", Phoneme)) %>% distinct()

# Which segments in the input are *not* in phoible already?
inventories.segments[which(!(inventories.segments$Phoneme %in% phoible.segments$Phoneme)),]
```

    ##  [1] "ɗ̻"    "ɪʊ"   "ɪu̯"   "pˤʼ"  "ɡˤ"   "kˤʰ"  "kˤʼ"  "qχˤʰ" "qχʷʰ" "hˤ"  
    ## [11] "ɒɪ"   "ɒu"   "iəi"  "iəu"  "˦˧˦"  "u̹"    "eːi"  "ɑiː"  "uːi"  "yːi" 
    ## [21] "eːu"  "ʔˠ"   "ʕ̞"    "æ̠ː"   "ɻˤ"   "ă̟"    "a̟ʊ"   "yɥ"   "ø̯e"   "ɛːi" 
    ## [31] "ø̯ɛi"  "ɑːi"  "oːi"  "iːu"  "ɛːu"  "ø̯ɛu"  "k̠"    "ɣ̠"    "tˁ"   "sˁ"  
    ## [41] "ðˁ"   "zˁ"   "q̟"    "ʁ̟"    "χ̟"    "ʊu̯"   "e̞ːu̯"  "æːi̯"  "æːu̯"  "i̞ːi̯" 
    ## [51] "o̞ːi̯"  "ʊi̯"   "i̞ːu̯"  "u̯ɪ"   "i̯ɪ"   "a̟u"   "tˤʰ"  "ü̜"    "øɛ"   "ɛ̝ːi" 
    ## [61] "ɛːə"  "ɑə"   "k̠ʰ"   "rˠ"

``` r
# TODO: when these inventories are fully incorporated into phoible this test should pass
# expect_true(all(ah.segments$Phoneme %in% phoible.segments$Phoneme))
```

TODO
----

``` r
# TODO: dump the metadata so that it can be appended easily to phoible mappings
# TODO: dump the inventories data so that it can be aggregated into phoible
```

Old code
--------

``` r
## NOTE: This code was to generate the initial IDs, etc., for the shared Google sheet.

## Generate PHOIBLE Inventory IDs -- with dplyr group_indices this apparently recorders by 
#library(data.table)
#inventories.ids <- data.table(inventories)
#inventories.ids[, InventoryID := .GRP, by = list(Glottocode, LanguageName, FileNames)]
#inventories.ids$InventoryID <- inventories.ids$InventoryID+3021

## Rearrange the columns
#inventories.ids <- inventories.ids[, c(9, 1:8)] # reorder for phoible import
#write.csv(inventories.ids, file="inventory-ids.csv")

## How many IDs? (Seems to match above)
#nrow(inventories.ids %>% select(InventoryID) %>% distinct())
#idq <- inventories.ids %>% select(InventoryID, Glottocode, FileNames) %>% distinct()
#write.csv(idq, file="inventories-ids-unique.csv")
```

``` r
# Load in helper functions from phoible
# source(url('https://raw.githubusercontent.com/phoible/dev/master/scripts/aggregation-helper-functions.R'))
```
