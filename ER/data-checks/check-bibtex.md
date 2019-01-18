Check ER bibliography
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
library(knitr)
library(testthat)
```

Prepare data
============

``` r
# Get ER data.
path <- '../data/raw2019/inventory_biblio.txt'
bib <- bib2df(path)
head(bib)
```

    ## # A tibble: 6 x 44
    ##   CATEGORY BIBTEXKEY ADDRESS ANNOTE AUTHOR BOOKTITLE CHAPTER CROSSREF
    ##   <chr>    <chr>     <chr>   <chr>  <list> <chr>     <chr>   <chr>   
    ## 1 THESIS   parish_a… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 2 BOOK     lissarra… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 3 BOOK     wangka_m… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 4 BOOK     crowley_… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 5 BOOK     lissarra… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 6 BOOK     donaldso… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## # ... with 36 more variables: EDITION <chr>, EDITOR <list>,
    ## #   HOWPUBLISHED <chr>, INSTITUTION <chr>, JOURNAL <chr>, KEY <chr>,
    ## #   MONTH <chr>, NOTE <chr>, NUMBER <chr>, ORGANIZATION <chr>,
    ## #   PAGES <chr>, PUBLISHER <chr>, SCHOOL <chr>, SERIES <chr>, TITLE <chr>,
    ## #   TYPE <chr>, VOLUME <chr>, YEAR <chr>, LOCATION <chr>, DATE <chr>,
    ## #   ISBN <chr>, PAGETOTAL <chr>, EDITORA <chr>, EDITORATYPE <chr>,
    ## #   KEYWORDS <chr>, VOLUMES <chr>, RIGHTS <chr>, URL <chr>,
    ## #   ABSTRACT <chr>, URLDATE <chr>, DOI <chr>, JOURNALTITLE <chr>,
    ## #   EPRINT <chr>, SHORTTITLE <chr>, BOOKAUTHOR <chr>, TITLEADDON <chr>

``` r
# Get ER inventories data. References are made distinct.
er.inv <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20190118.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
er.inv.d <- er.inv %>% select(Source_ref) %>% group_by(Source_ref) %>% distinct()
expect_equal(nrow(er.inv.d), 232)
kable(head(er.inv.d))
```

| Source\_ref                 |
|:----------------------------|
| reid\_ngangityemerri:\_1990 |
| birk\_phonology\_1975       |
| mushin\_grammar\_2012       |
| breen\_wanyi\_2003          |
| harvey\_ngoni\_1986         |
| parish\_aspects\_1983       |

Compare datasets
================

How many ER inventory Source\_ref's not in the bibtex file?
-----------------------------------------------------------

``` r
expect_equal(nrow(er.inv.d[which(!(er.inv.d$Source_ref %in% bib$BIBTEXKEY)), ]), 0)
# kable(er.inv.d[which(!(er.inv.d$Source_ref %in% bib$BIBTEXKEY)), ])
```

How many bibtex IDs not in the ER inventory data?
-------------------------------------------------

``` r
# None
expect_equal(nrow(bib[which(!(bib$BIBTEXKEY %in% er.inv.d$Source_ref)), ]), 0)
```

Check out PHOIBLE
=================

``` r
# How many unique er bibtex ids are there per language name?
expect_equal(nrow(er.inv %>% select(Variety_name, Source_ref) %>% distinct()), 392)
```

``` r
# phoible index data
ph.refs <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-Bibtex.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
# OK to filter on source
expect_equal(ph.refs %>% filter(Source=="er") %>% nrow(), 396)
# Filter
ph.refs.er <- ph.refs %>% filter(Source=="er")
expect_equal(nrow(ph.refs.er), 396)
```

``` r
# 236 unique bibtex keys in phoible index
expect_equal(nrow(ph.refs.er %>% select(BibtexKey) %>% distinct()), 236)

# 232 unique bibtex keys in ER distinct inventories
expect_equal(nrow(er.inv.d %>% select(Source_ref) %>% distinct()), 232)

x <- ph.refs.er %>% select(BibtexKey) %>% distinct()
y <- er.inv.d %>% select(Source_ref) %>% distinct()

# None of the bibtex IDs match match
table(x$BibtexKey %in% y$Source_ref)
```

    ## 
    ## FALSE 
    ##   236

``` r
# Names are diff
head(er.inv.d)
```

    ## # A tibble: 6 x 1
    ## # Groups:   Source_ref [6]
    ##   Source_ref               
    ##   <chr>                    
    ## 1 reid_ngangityemerri:_1990
    ## 2 birk_phonology_1975      
    ## 3 mushin_grammar_2012      
    ## 4 breen_wanyi_2003         
    ## 5 harvey_ngoni_1986        
    ## 6 parish_aspects_1983

``` r
head(ph.refs.er)
```

    ##   InventoryID BibtexKey Source
    ## 1        2629     25099     er
    ## 2        2630     62949     er
    ## 3        2631     98977     er
    ## 4        2632    126849     er
    ## 5        2633    161795     er
    ## 6        2634    413034     er
