PHOIBLE data dump to CLDF 1.0 specification
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;
06 May, 2019

``` r
library(stringi)
library(Unicode)
library(tidyverse)
library(testthat)
library(digest)
```

``` r
# PHOIBLE aggregated data (dev)
load(url('https://github.com/phoible/dev/blob/master/data/phoible.RData?raw=true'))
expect_equal(nrow(phoible), 105467)
```

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
# Get bibtex keys in semi-colon delimited list (TODO: update this link when merged)
refs <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.csv', header=T, stringsAsFactors=F)
citations <- refs %>% group_by(InventoryID, URI) %>% summarize(Contribution_ID=tolower(paste(BibtexKey, collapse=";")))
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

# Add in language names from the phoible index
index <- read.csv('../../phoible/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors=F)
index <- index %>% select(InventoryID, LanguageName)
colnames(index) <- c("InventoryID", "FullLanguageName")
phoible <- left_join(phoible, index)
```

    ## Joining, by = "InventoryID"

``` r
rm(index)

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
values <- phoible %>% select(ROWID, ISO6393, Glottocode, Parameter_ID, Phoneme, Marginal, Allophones, Source, InventoryID)
colnames(values) <- c("ID", "ISO639P3code", "Language_ID", "Parameter_ID", "Name", "Marginal", "Allophones", "Source", "Contribution_ID")
expect_equal(nrow(phoible), nrow(values))
glimpse(values)
```

    ## Observations: 105,467
    ## Variables: 9
    ## $ ID              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
    ## $ ISO639P3code    <chr> "kor", "kor", "kor", "kor", "kor", "kor", "kor",…
    ## $ Language_ID     <chr> "kore1280", "kore1280", "kore1280", "kore1280", …
    ## $ Parameter_ID    <chr> "3bff843fa918065aa88e47217358c573", "eb735919b72…
    ## $ Name            <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "ɤː", "h",…
    ## $ Marginal        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ Allophones      <chr> "a", "aː", "ɛ æ", "æː", "e", "eː", "ɤ", "ɤː", "ç…
    ## $ Source          <chr> "spa", "spa", "spa", "spa", "spa", "spa", "spa",…
    ## $ Contribution_ID <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

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
# parameters <- phoible %>% select(Parameter_ID, Phoneme, Phoneme_description, SegmentClass) %>% distinct()
# colnames(parameters) <- c("ID", "Name", "Description", "SegmentClass")

parameters <- phoible %>% select(Parameter_ID, Phoneme, Phoneme_description, SegmentClass, tone, stress, syllabic, short, long, consonantal, sonorant, continuant, delayedRelease, approximant, tap, trill, nasal, lateral, labial, round, labiodental, coronal, anterior, distributed, strident, dorsal, high, low, front, back, tense, retractedTongueRoot, advancedTongueRoot, periodicGlottalSource, epilaryngealSource, spreadGlottis, constrictedGlottis, fortis, raisedLarynxEjective, loweredLarynxImplosive, click) %>% distinct()

colnames(parameters) <- c("ID", "Name", "Description", "SegmentClass", "tone", "stress", "syllabic", "short", "long", "consonantal", "sonorant", "continuant", "delayedRelease", "approximant", "tap", "trill", "nasal", "lateral", "labial", "round", "labiodental", "coronal", "anterior", "distributed", "strident", "dorsal", "high", "low", "front", "back", "tense", "retractedTongueRoot", "advancedTongueRoot", "periodicGlottalSource", "epilaryngealSource", "spreadGlottis", "constrictedGlottis", "fortis", "raisedLarynxEjective", "loweredLarynxImplosive", "click")

glimpse(parameters)
```

    ## Observations: 3,183
    ## Variables: 41
    ## $ ID                     <chr> "3bff843fa918065aa88e47217358c573", "eb73…
    ## $ Name                   <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "ɤː…
    ## $ Description            <chr> "LATIN SMALL LETTER A", "LATIN SMALL LETT…
    ## $ SegmentClass           <chr> "vowel", "vowel", "vowel", "vowel", "vowe…
    ## $ tone                   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ stress                 <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ syllabic               <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ short                  <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ long                   <chr> "-", "+", "-", "+", "-", "+", "-", "+", "…
    ## $ consonantal            <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ sonorant               <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ continuant             <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ delayedRelease         <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ approximant            <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ tap                    <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ trill                  <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ nasal                  <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ lateral                <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ labial                 <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ round                  <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ labiodental            <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ coronal                <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ anterior               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ distributed            <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ strident               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ dorsal                 <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ high                   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ low                    <chr> "+", "+", "+", "+", "-", "-", "-", "-", "…
    ## $ front                  <chr> "-", "-", "+", "+", "+", "+", "-", "-", "…
    ## $ back                   <chr> "-", "-", "-", "-", "-", "-", "+", "+", "…
    ## $ tense                  <chr> "0", "0", "0", "0", "+", "+", "+", "+", "…
    ## $ retractedTongueRoot    <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ advancedTongueRoot     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ periodicGlottalSource  <chr> "+", "+", "+", "+", "+", "+", "+", "+", "…
    ## $ epilaryngealSource     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ spreadGlottis          <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ constrictedGlottis     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ fortis                 <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ raisedLarynxEjective   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ loweredLarynxImplosive <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ click                  <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…

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

contributions <- phoible %>% select(InventoryID, Source, FullLanguageName, Contributor, Contribution_ID, URI) %>% distinct()
colnames(contributions) <- c("ID", "Contributor_ID", "Name", "Contributors", "References", "URI")
glimpse(contributions)
```

    ## Observations: 3,020
    ## Variables: 6
    ## $ ID             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
    ## $ Contributor_ID <chr> "spa", "spa", "spa", "spa", "spa", "spa", "spa", …
    ## $ Name           <chr> "Korean", "Ket", "Lak", "Kabardian", "Georgian", …
    ## $ Contributors   <fct> Stanford Phonology Archive, Stanford Phonology Ar…
    ## $ References     <chr> "cho1967;martin1951;martin1954;martinlee1969;kim1…
    ## $ URI            <chr> "https://archive.org/details/kor_SPA1979_phon", "…

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
    ## Variables: 9
    ## $ ID          <fct> aa, gm, ph, ra, saphon, spa, upsid, uz, ea, er
    ## $ Name        <fct> AA, GM, PH, RA, SAPHON, SPA, UPSID, UZ, EA, ER
    ## $ Contributor <fct> "Christian Chanard and Rhonda L. Hartell (AA)", "Chr…
    ## $ Description <fct> "The inventories in Alphabets of Africa (AA) come fr…
    ## $ Contents    <fct> "Phonemes, tones.", "Phonemes, allophones, tones.", …
    ## $ Citation    <fct> Chanard2006; Hartell1993, Moran_etal2014, Moran_etal…
    ## $ SourceURL   <fct> http://sumale.vjf.cnrs.fr/phono/, , , , http://lingu…
    ## $ URL         <fct> https://github.com/phoible/dev/tree/master/raw-data/…
    ## $ with_tones  <int> 1, 1, 1, 1, 0, 1, 0, 1, 1, 1

``` r
rm(contributors)
rm(phoible)
```
