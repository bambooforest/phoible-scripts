PHOIBLE data tests
================
Steven Moran
&lt;<a href="mailto:steven.moran@uzh.ch" class="email">steven.moran@uzh.ch</a>&gt;

    library(bib2df)
    library(dplyr)
    library(knitr)
    library(testthat)

    # Get the bibtex data
    path <- 'https://raw.githubusercontent.com/phoible/dev/master/data/phoible-references.bib'
    bib <- bib2df(path)

    index.bibtex <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.csv', header=T, stringsAsFactors = F)

    # Which bibtex keys are in the phoible bibtex index and NOT in the bibtex file?
    ## We expect one missing reference:
    expect_equal(1, length(which(!(index.bibtex$BibtexKey %in% bib$BIBTEXKEY))))
    index.bibtex[which(!(index.bibtex$BibtexKey %in% bib$BIBTEXKEY)), ]

    ##     InventoryID       BibtexKey Source Filename
    ## 353         201 NO SOURCE GIVEN  upsid     <NA>
    ##                                                   URI
    ## 353 http://web.phonetik.uni-frankfurt.de/L/L8362.html

    # phoible bibex file has more (legacy) entries than in the phoible index
    bib[which(!(bib$BIBTEXKEY %in% index.bibtex$BibtexKey)), ]

    ## # A tibble: 255 x 130
    ##    CATEGORY BIBTEXKEY ADDRESS ANNOTE AUTHOR BOOKTITLE CHAPTER CROSSREF EDITION
    ##    <chr>    <chr>     <chr>   <chr>  <list> <chr>     <chr>   <chr>    <chr>  
    ##  1 ARTICLE  Schadebe… <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  2 MISC     Round2019 <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  3 MISC     Nikolaev… <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  4 MISC     saphon    <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  5 MISC     spa1979   <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  6 BOOK     maddieso… Cambri… <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  7 INCOLLE… maddieso… <NA>    <NA>   <chr … UCLA Wor… <NA>    <NA>     <NA>   
    ##  8 BOOK     ramaswam… <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ##  9 PHDTHES… Moran2012 <NA>    <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ## 10 BOOK     Moran_et… Leipzig <NA>   <chr … <NA>      <NA>    <NA>     <NA>   
    ## # … with 245 more rows, and 121 more variables: EDITOR <list>,
    ## #   HOWPUBLISHED <chr>, INSTITUTION <chr>, JOURNAL <chr>, KEY <chr>,
    ## #   MONTH <chr>, NOTE <chr>, NUMBER <chr>, ORGANIZATION <chr>, PAGES <chr>,
    ## #   PUBLISHER <chr>, SCHOOL <chr>, SERIES <chr>, TITLE <chr>, TYPE <chr>,
    ## #   VOLUME <chr>, YEAR <chr>, DATE.ADDED <chr>, DATE.MODIFIED <chr>,
    ## #   BDSK.URL.1 <chr>, URL <chr>, CATALOGUE.URL <chr>, ISBN <chr>,
    ## #   AIATSIS_CALLNUMBER <chr>, AIATSIS_CODE <chr>,
    ## #   AIATSIS_REFERENCE_LANGUAGE <chr>, CLASS_LOC <chr>, DOCUMENT_TYPE <chr>,
    ## #   FN <chr>, HHTYPE <chr>, INLG <chr>, LGCODE <chr>, MACRO_AREA <chr>,
    ## #   MPI_EVA_LIBRARY_SHELF <chr>, MPIFN <chr>, OCLC <chr>, OZBIB_ID <chr>,
    ## #   OZBIBREFTYPE <chr>, SRC <chr>, SUBJECT_HEADINGS <chr>, DOI <chr>,
    ## #   OZBIBNOTE <chr>, BDSK.URL.2 <chr>, THESISTYPE <chr>, CALL_NUMBER <chr>,
    ## #   FNNOTE <chr>, ADDED <chr>, ASJP_NAME <chr>, ISO_CODE <chr>, KEYWORDS <chr>,
    ## #   MODIFIED <chr>, OLAC_FIELD <chr>, REFDB_ID <chr>, WALS_CODE <chr>,
    ## #   COUNTRY <chr>, SIL_ID <chr>, SUBJECT <chr>, LOCATION <chr>, OWNER <chr>,
    ## #   TIMESTAMP <chr>, SHORTTITLE <chr>, LANGNOTE <chr>, LGFAMILY <chr>,
    ## #   BWONOTE <chr>, ZURICHCODE <chr>, NOSHAREFN <chr>, LAST_CHANGED <chr>,
    ## #   GULDEMANN_LOCATION <chr>, MED <chr>, EXTRA_HASH <chr>, VARIETY <chr>,
    ## #   OTHER_EDITIONS <chr>, FILENAMES <chr>, ABSTRACT <chr>,
    ## #   SHELF_LOCATION <chr>, INLG_CODE <chr>, INTERNETARCHIVE_ID <chr>,
    ## #   SRCTRICKLE <chr>, LAPOLLANOTE <chr>, SEANOTE <chr>, TITLE_ENGLISH <chr>,
    ## #   ADVISER <chr>, DEGREE <chr>, DIGITAL_FORMATS <chr>, SOURCE <chr>,
    ## #   UMI_ID <chr>, AUTHOR_STATEMENT <chr>, REVIEW <chr>, PERMISSION <chr>,
    ## #   HAL_ID <chr>, HAL_VERSION <chr>, PDF <chr>, INVENTORYID <chr>,
    ## #   LANGUAGECODE <chr>, LANGUAGENAME <chr>, LANGUAGEVARIANTCODE <chr>,
    ## #   SQUIB <chr>, NOTES <chr>, CONFERENCE <chr>, GLOTTOLOG_REF_ID <chr>, …

    # Which Glottolog codes are in valid input strings?
    library(stringr)
    index <- read.csv('../../phoible/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors = F)

    # Mismatch in string length
    expect_equal(length(which(str_length(index$Glottocode) != 8)), 0)
    which(str_length(index$Glottocode) != 8)

    ## integer(0)

    # Mismatch in makeup
    glottocode <- "([a-z]{4})([0-9]{4})"
    expect_equal(length(which(!(str_detect(index$Glottocode, glottocode)))), 0)
    which(!(str_detect(index$Glottocode, glottocode)))

    ## integer(0)

    # Need to read in the Glottolog data from GitHub (not the geo data) and check the Glottolog codes (and then update the update index page)
    # glottolog <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv', header=T, stringsAsFactors = F)
    glottolog <- read.csv('../glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors = F)

    index[which(!(index$Glottocode %in% glottolog$id)), ]

    ##      InventoryID ISO6393 Glottocode LanguageName Source
    ## 2729        2729    <NA>       <NA>    Djindewal     er

<!--
## load(url('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.RData'))
-->

Are there any duplicate phoneme rows?
=====================================

    # phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv', header=T, stringsAsFactors = F)
    phoible <- read.csv('../../phoible/data/phoible.csv', header=T, stringsAsFactors = F)
    phoible %>% group_by(InventoryID, Phoneme) %>% filter(n()>1) %>% select(InventoryID, Glottocode, Phoneme, Source)

    ## # A tibble: 0 x 4
    ## # Groups:   InventoryID, Phoneme [0]
    ## # … with 4 variables: InventoryID <int>, Glottocode <chr>, Phoneme <chr>,
    ## #   Source <chr>
