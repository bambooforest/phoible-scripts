Check bibtex files
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
```

``` r
# ER bib
path <- '/Users/stiv/Dropbox/Github/phoible-scripts/ER/data/raw2019/inventory_biblio.txt'
bib <- bib2df(path)
```

``` r
# ER inventories
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20180114.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
new.ids <- new %>% select(Source_ref) %>% group_by(Source_ref) %>% distinct()
nrow(new.ids) # 232
```

    ## [1] 232

``` r
head(new.ids)
```

    ## # A tibble: 6 x 1
    ## # Groups:   Source_ref [6]
    ##   Source_ref               
    ##   <chr>                    
    ## 1 reid_ngangityemerri:_1990
    ## 2 birk_phonology_1975      
    ## 3 mushin_grammar_2012      
    ## 4 breen_barkly:_2003       
    ## 5 harvey_ngoni_1986        
    ## 6 parish_aspects_1983

``` r
# Bibtex IDs in inventories missing in bibtex file (7)
nrow(new.ids[which(!(new.ids$Source_ref %in% bib$BIBTEXKEY)), ]) # 7
```

    ## [1] 7

``` r
new.ids[which(!(new.ids$Source_ref %in% bib$BIBTEXKEY)), ]
```

    ## # A tibble: 7 x 1
    ## # Groups:   Source_ref [7]
    ##   Source_ref                                                   
    ##   <chr>                                                        
    ## 1 breen_barkly:_2003                                           
    ## 2 bowern_grammar_2012                                          
    ## 3 bowern_nhirrpi_2000                                          
    ## 4 tsunoda_djaru_1981                                           
    ## 5 wangka_maya_pilbara_aboriginal_language_centre_bayungu_2008  
    ## 6 wangka_maya_pilbara_aboriginal_language_centre_thalanyji_2008
    ## 7 kohn_morphological_2012

``` r
# No extra bibtex IDs in bib file (and not in the inventory file)
bib[which(!(bib$BIBTEXKEY %in% new.ids$Source_ref)), ] # 0
```

    ## # A tibble: 0 x 44
    ## # ... with 44 variables: CATEGORY <chr>, BIBTEXKEY <chr>, ADDRESS <chr>,
    ## #   ANNOTE <chr>, AUTHOR <list>, BOOKTITLE <chr>, CHAPTER <chr>,
    ## #   CROSSREF <chr>, EDITION <chr>, EDITOR <list>, HOWPUBLISHED <chr>,
    ## #   INSTITUTION <chr>, JOURNAL <chr>, KEY <chr>, MONTH <chr>, NOTE <chr>,
    ## #   NUMBER <chr>, ORGANIZATION <chr>, PAGES <chr>, PUBLISHER <chr>,
    ## #   SCHOOL <chr>, SERIES <chr>, TITLE <chr>, TYPE <chr>, VOLUME <chr>,
    ## #   YEAR <chr>, LOCATION <chr>, DATE <chr>, ISBN <chr>, PAGETOTAL <chr>,
    ## #   EDITORA <chr>, EDITORATYPE <chr>, KEYWORDS <chr>, VOLUMES <chr>,
    ## #   RIGHTS <chr>, URL <chr>, ABSTRACT <chr>, URLDATE <chr>, DOI <chr>,
    ## #   JOURNALTITLE <chr>, EPRINT <chr>, SHORTTITLE <chr>, BOOKAUTHOR <chr>,
    ## #   TITLEADDON <chr>

``` r
# Get phoible bib
pi.refs <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-Bibtex.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
pi.refs %>% filter(Source=="er") %>% nrow() # 396
```

    ## [1] 396

``` r
pi.refs %>% filter(Source=="er") %>% distinct() %>% nrow() # 396
```

    ## [1] 396

``` r
pi.refs.er <- pi.refs %>% filter(Source=="er")

# Meld phoible
# df <- left_join(pi.refs.er, x)
# head(df)
```

``` r
# Now add ER's old data
old <- read.table("../data/raw/Australian_phonemes_for_PHOIBLE_20170501.txt", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
head(old)
```

    ##   IPA_for_PHOIBLE Glottolog_code Glottolog_nearest Variety_name     Source
    ## 1               t͉       bura1267              <NA>      Burarra Green 1987
    ## 2               t͈       bura1267              <NA>      Burarra Green 1987
    ## 3               ʈ͉       bura1267              <NA>      Burarra Green 1987
    ## 4               ʈ͈       bura1267              <NA>      Burarra Green 1987
    ## 5          \u0236͉       bura1267              <NA>      Burarra Green 1987
    ## 6          \u0236͈       bura1267              <NA>      Burarra Green 1987
    ##                                         Source_ref
    ## 1 http://glottolog.org/resource/reference/id/25099
    ## 2 http://glottolog.org/resource/reference/id/25099
    ## 3 http://glottolog.org/resource/reference/id/25099
    ## 4 http://glottolog.org/resource/reference/id/25099
    ## 5 http://glottolog.org/resource/reference/id/25099
    ## 6 http://glottolog.org/resource/reference/id/25099
    ##                                                                   Comments
    ## 1 Please see also the general comparative notes on Australian inventories.
    ## 2 Please see also the general comparative notes on Australian inventories.
    ## 3 Please see also the general comparative notes on Australian inventories.
    ## 4 Please see also the general comparative notes on Australian inventories.
    ## 5 Please see also the general comparative notes on Australian inventories.
    ## 6 Please see also the general comparative notes on Australian inventories.
