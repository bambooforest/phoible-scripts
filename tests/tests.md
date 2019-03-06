PHOIBLE data tests
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
library(knitr)
library(testthat)
```

``` r
# Get the bibtex data
# path <- 'https://raw.githubusercontent.com/phoible/dev/master/data/phoible-references.bib'

path <- '../../phoible/data/phoible-references.bib'
bib <- bib2df(path)
```

``` r
# index.bibtex <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.csv', header=T, stringsAsFactors = F)
index.bibtex <- read.csv('../../phoible/mappings/InventoryID-Bibtex.csv', header=T, stringsAsFactors = F)
```

``` r
# Which bibtex keys are in the phoible bibtex index and NOT in the bibtex file?
## We expect one missing reference:
expect_equal(1, length(which(!(index.bibtex$BibtexKey %in% bib$BIBTEXKEY))))
index.bibtex[which(!(index.bibtex$BibtexKey %in% bib$BIBTEXKEY)), ]
```

    ##     InventoryID       BibtexKey Source
    ## 353         201 NO SOURCE GIVEN  upsid
    ##                                              Filename
    ## 353 http://web.phonetik.uni-frankfurt.de/L/L8362.html

``` r
# Which Glottolog codes are in valid input strings?
library(stringr)
index <- read.csv('../../phoible/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors = F)

# Mismatch in string length
expect_equal(length(which(str_length(index$Glottocode) != 8)), 0)
which(str_length(index$Glottocode) != 8)
```

    ## integer(0)

``` r
# Mismatch in makeup
glottocode <- "([a-z]{4})([0-9]{4})"
expect_equal(length(which(!(str_detect(index$Glottocode, glottocode)))), 0)
which(!(str_detect(index$Glottocode, glottocode)))
```

    ## integer(0)

``` r
# Need to read in the Glottolog data from GitHub (not the geo data) and check the Glottolog codes (and then update the update index page)
# glottolog <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv', header=T, stringsAsFactors = F)
glottolog <- read.csv('../glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors = F)

index[which(!(index$Glottocode %in% glottolog$id)), ]
```

    ##      InventoryID ISO6393 Glottocode                  LanguageName Source
    ## 2281        2281    <NA>       <NA> Modern Aramaic (Northeastern)     ea
    ## 2729        2729    <NA>       <NA>                     Djindewal     er

<!--
## load(url('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.RData'))
-->
