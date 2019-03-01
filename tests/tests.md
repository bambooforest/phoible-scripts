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

<!--
## load(url('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.RData'))
-->
