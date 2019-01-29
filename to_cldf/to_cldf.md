PHOIBLE data dump to CLDF 1.0
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(stringi)
library(Unicode)
library(tidyverse)
library(testthat)
library(digest)
```

``` r
# PHOIBLE aggregated data
## TODO: update once the aggregation script is finished (and tag the version)
load(url('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.RData'))
phoible$InventoryID <- as.integer(phoible$InventoryID)
expect_equal(nrow(phoible), 95993)
```

``` r
# TODO: Problems to address! Submited to phoible/dev issues / aggregation PR.
# 1. spaces in LanguageName
# 2. PHOIBLE ISO != Glottolog ISO 

# Groups:   InventoryID, LanguageName, Glottocode, LanguageCode [3]
# InventoryID LanguageName Glottocode LanguageCode name  macroarea latitude longitude isocodes
# <int> <chr>        <chr>      <chr>        <fct> <fct>        <dbl>     <dbl> <fct>   
# 1         298 DAN          dann1241   lda          Dan   Africa        7.23     -8.25 daf     
# 2         691 "dan "       dann1241   dnj          Dan   Africa        7.23     -8.25 daf     
# 3        1393 Dan          dann1241   lda          Dan   Africa        7.23     -8.25 daf  

phoible %>% filter(InventoryID %in% c(691,298,1393)) %>% distinct(InventoryID, LanguageCode, LanguageName)
```

    ##   InventoryID LanguageCode LanguageName
    ## 1        1393          lda          Dan
    ## 2         298          lda          DAN
    ## 3         691          dnj         dan

Create one large table
======================

``` r
# Contributors data
contributors <- read.csv("contributors.csv")
phoible <- left_join(phoible, contributors, by=c("Source"="ID"))
```

    ## Warning: Column `Source`/`ID` joining character vector and factor, coercing
    ## into character vector

``` r
# Get bibtex keys in semi-colon delimited list
refs <- read.table('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.tsv', sep="\t", header=T, stringsAsFactors=F)
citations <- refs %>% group_by(InventoryID) %>% summarize(Contribution_ID=tolower(paste(BibtexKey, collapse=";")))
phoible <- left_join(phoible, citations)
```

    ## Joining, by = "InventoryID"

``` r
rm(refs, citations)

# Generate parameter ID (much quicker if we get a unique list of the phonemes first)
parameters <- phoible %>% select(Phoneme) %>% distinct(Phoneme) %>% arrange(Phoneme)
names <- sapply(parameters$Phoneme, function(x) u_char_name(as.u_char(utf8ToInt(x))))
parameters <- mutate(parameters, Phoneme_description = stri_join_list(names, sep = " - "))

# Get the segment name IDs
library(digest)
parameters$Parameter_ID <- sapply(parameters$Phoneme_description, digest, algo="md5")

# Merge in the phoneme parameters
phoible <- left_join(phoible, parameters)
```

    ## Joining, by = "Phoneme"

``` r
rm(names, parameters)

# But how to get the b16 encoding?
# https://github.com/clld/phoible/blob/73d140ae4c3377140fe3678b1997d0c13d42ad41/phoible/scripts/initializedb.py#L353-L357
# library(caTools)
# base64encode(x)
# library(hashids)
# hashids::encode_hex(x)

# Add in row "ID"s
phoible$ROWID <- seq.int(nrow(phoible))
```

Dump the individual tables for CLDF
===================================

values.csv
----------

``` r
# values.csv
## ID,Language_ID,Parameter_ID,Value,Code_ID,Comment,Source,Contribution_ID
## 1,kor,9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ Korean (SPA),,,cho1967;kim1972;martin1954;martinlee1969;martin1951;kim1968,1
## 14564,kor,9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ Korean (UPSID),,,kim1986;martin1951;martinlee1969;martin1954;cho1967;kim1972,423

# "ID","Language_ID","Parameter_ID","Value","Code_ID","Comment","Source","Contribution_ID"
glimpse(phoible)
```

    ## Observations: 95,993
    ## Variables: 21
    ## $ InventoryID         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ Glottocode          <chr> "kore1280", "kore1280", "kore1280", "kore1...
    ## $ LanguageCode        <chr> "kor", "kor", "kor", "kor", "kor", "kor", ...
    ## $ LanguageName        <chr> "Korean", "Korean", "Korean", "Korean", "K...
    ## $ SpecificDialect     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ GlyphID             <chr> "0061", "0061+02D0", "00E6", "00E6+02D0", ...
    ## $ Phoneme             <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "ɤː"...
    ## $ Allophones          <chr> "a", "aː", "ɛ æ", "æː", "e", "eː", "ɤ", "ɤ...
    ## $ Marginal            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ Source              <chr> "spa", "spa", "spa", "spa", "spa", "spa", ...
    ## $ Name                <fct> SPA, SPA, SPA, SPA, SPA, SPA, SPA, SPA, SP...
    ## $ Contributor         <fct> Stanford Phonology Archive, Stanford Phono...
    ## $ Description         <fct> The Stanford Phonology Archive (SPA) was t...
    ## $ Contents            <fct> Phonemes, allophones, tones., Phonemes, al...
    ## $ Citation            <fct> spa1979, spa1979, spa1979, spa1979, spa197...
    ## $ SourceURL           <fct> , , , , , , , , , , , , , , , , , , , , , ...
    ## $ URL                 <fct> https://github.com/phoible/dev/tree/master...
    ## $ Contribution_ID     <chr> "cho1967;martin1951;martin1954;martinlee19...
    ## $ Phoneme_description <chr> "LATIN SMALL LETTER A", "LATIN SMALL LETTE...
    ## $ Parameter_ID        <chr> "3bff843fa918065aa88e47217358c573", "eb735...
    ## $ ROWID               <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,...

``` r
values <- phoible %>% select(ROWID, LanguageCode, Glottocode, Parameter_ID, Phoneme, Marginal, Allophones, Source, InventoryID)
colnames(values) <- c("ID", "ISO639P3code", "Language_ID", "Parameter_ID", "Name", "Marginal", "Allophones", "Source", "Contribution_ID")
expect_equal(nrow(phoible), nrow(values))
glimpse(values)
```

    ## Observations: 95,993
    ## Variables: 9
    ## $ ID              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,...
    ## $ ISO639P3code    <chr> "kor", "kor", "kor", "kor", "kor", "kor", "kor...
    ## $ Language_ID     <chr> "kore1280", "kore1280", "kore1280", "kore1280"...
    ## $ Parameter_ID    <chr> "3bff843fa918065aa88e47217358c573", "eb735919b...
    ## $ Name            <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "ɤː", "h...
    ## $ Marginal        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ Allophones      <chr> "a", "aː", "ɛ æ", "æː", "e", "eː", "ɤ", "ɤː", ...
    ## $ Source          <chr> "spa", "spa", "spa", "spa", "spa", "spa", "spa...
    ## $ Contribution_ID <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...

``` r
write.csv(values, file="cldf/values.csv", row.names=FALSE)
rm(values)
```

parameters.csv
--------------

``` r
# parameters.csv
## ID,Name,Description
## 9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ,LATIN SMALL LETTER T - COMBINING MINUS SIGN BELOW - LATIN SMALL LETTER ESH - MODIFIER LETTER SMALL H
## 41D8827BB943E1FB9055A7487293BA79,pˀ,LATIN SMALL LETTER P - MODIFIER LETTER GLOTTAL STOP
parameters <- phoible %>% select(Parameter_ID, Phoneme, Phoneme_description) %>% distinct()
colnames(parameters) <- c("ID", "Name", "Description")
glimpse(parameters)
```

    ## Observations: 3,205
    ## Variables: 3
    ## $ ID          <chr> "3bff843fa918065aa88e47217358c573", "eb735919b725c...
    ## $ Name        <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "ɤː", "h", "...
    ## $ Description <chr> "LATIN SMALL LETTER A", "LATIN SMALL LETTER A - MO...

``` r
write.csv(parameters, file="cldf/parameters.csv", row.names=FALSE)
rm(parameters)
```

contributions.csv
-----------------

``` r
# contributions.csv:
## ID,Name,Description,Contributors
## 1,Korean (SPA),Korean,Stanford Phonology Archive

contributions <- phoible %>% select(InventoryID, Source, LanguageName, Contributor, Contribution_ID) %>% distinct()
colnames(contributions) <- c("ID", "Contributor_ID", "Name", "Contributors", "References")
glimpse(contributions)
```

    ## Observations: 2,628
    ## Variables: 5
    ## $ ID             <int> 1, 10, 100, 1000, 1001, 1002, 1003, 1004, 1005,...
    ## $ Contributor_ID <chr> "spa", "spa", "spa", "ph", "ph", "ph", "ph", "p...
    ## $ Name           <chr> "Korean", "Mundari", "Mixtec", "Secoya", "Sedik...
    ## $ Contributors   <fct> Stanford Phonology Archive, Stanford Phonology ...
    ## $ References     <chr> "cho1967;martin1951;martin1954;martinlee1969;ki...

``` r
write.csv(contributions, file="cldf/contributions.csv", row.names=FALSE)
rm(contributions)
```

contributors.csv
----------------

``` r
write.csv(contributors, file="cldf/contributors.csv", row.names=FALSE)
glimpse(contributors)
```

    ## Observations: 10
    ## Variables: 8
    ## $ ID          <fct> aa, gm, ph, ra, saphon, spa, upsid, uz, ea, er
    ## $ Name        <fct> AA, GM, PH, RA, SAPHON, SPA, UPSID, UZ, EA, ER
    ## $ Contributor <fct> Christian Chanard and Rhonda L. Hartell (AA), Chri...
    ## $ Description <fct> The inventories in Alphabets of Africa (AA) come f...
    ## $ Contents    <fct> Phonemes, tones., Phonemes, allophones, tones., Ph...
    ## $ Citation    <fct> Chanard2006; Hartell1993, Moran_etal2014, Moran_et...
    ## $ SourceURL   <fct> http://sumale.vjf.cnrs.fr/phono/, , , , http://lin...
    ## $ URL         <fct> https://github.com/phoible/dev/tree/master/raw-dat...

``` r
rm(contributors)
rm(phoible)
```
