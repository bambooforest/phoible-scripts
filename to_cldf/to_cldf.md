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
## read.csv('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.csv')
load('phoible-by-phoneme.RData')
phoible$InventoryID <- as.integer(phoible$InventoryID)
```

``` r
# TODO: Problems to address! 
# 1. spaces in LanguageName
# 2. PHOIBLE ISO != Glottolog ISO 

# Groups:   InventoryID, LanguageName, Glottocode, LanguageCode [3]
# InventoryID LanguageName Glottocode LanguageCode name  macroarea latitude longitude isocodes
# <int> <chr>        <chr>      <chr>        <fct> <fct>        <dbl>     <dbl> <fct>   
# 1         298 DAN          dann1241   lda          Dan   Africa        7.23     -8.25 daf     
# 2         691 "dan "       dann1241   dnj          Dan   Africa        7.23     -8.25 daf     
# 3        1393 Dan          dann1241   lda          Dan   Africa        7.23     -8.25 daf  

# phoible %>% filter(InventoryID %in% c(691,298,1393))
```

languages.csv
=============

``` r
# languages.csv:
## ID,Name,Macroarea,Latitude,Longitude,Glottocode,ISO639P3code
## kor,Korean,,37.5,128.0,kore1280,kor
## ket,Ket,,63.7551,87.5466,kett1243,ket

languages <- phoible %>% group_by(InventoryID, LanguageName, Glottocode, LanguageCode) %>% select(InventoryID, LanguageName, Glottocode, LanguageCode) %>% distinct() %>% arrange(InventoryID)

# Merge in Glottolog data (v 3.3)
glottolog <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-F088-DE0E-0712-0/languages_and_dialects_geo.csv')
glottolog.cut <- glottolog %>% select(glottocode, name, macroarea, latitude, longitude, isocodes)
# TODO: Join seems to drop digits in dbl
languages <- left_join(languages, glottolog.cut, by=c("Glottocode"="glottocode"))
```

    ## Warning: Column `Glottocode`/`glottocode` joining character vector and
    ## factor, coercing into character vector

``` r
# Note here that the `Name` column is from the Glottolog data and not from PHOIBLE
languages[which(!(languages$LanguageName == languages$name)),]
```

    ## # A tibble: 1,526 x 9
    ## # Groups:   InventoryID, LanguageName, Glottocode, LanguageCode [1,526]
    ##    InventoryID LanguageName Glottocode LanguageCode name  macroarea
    ##          <int> <chr>        <chr>      <chr>        <fct> <fct>    
    ##  1           9 Kota         kota1263   kfe          Kota… Eurasia  
    ##  2          14 Cambodian    cent1989   khm          Cent… Eurasia  
    ##  3          17 Wu           wuch1236   wuu          Wu C… Eurasia  
    ##  4          18 Hakka        hakk1236   hak          Hakk… Eurasia  
    ##  5          19 Cantonese    yuec1235   yue          Yue … Eurasia  
    ##  6          20 Yao          iumi1238   ium          Iu M… Eurasia  
    ##  7          21 Dafla        nyis1236   njz          Nyis… Eurasia  
    ##  8          25 Karen        sgaw1245   ksw          S'ga… Eurasia  
    ##  9          28 Yay          bouy1240   pcc          Bouy… Eurasia  
    ## 10          29 Cham         west2650   cja          West… Eurasia  
    ## # ... with 1,516 more rows, and 3 more variables: latitude <dbl>,
    ## #   longitude <dbl>, isocodes <fct>

``` r
# TODO: Here the PHOIBLE and Glottocode ISO codes don't match up
languages[which(!(languages$LanguageCode == languages$isocodes)),]
```

    ## # A tibble: 49 x 9
    ## # Groups:   InventoryID, LanguageName, Glottocode, LanguageCode [49]
    ##    InventoryID LanguageName Glottocode LanguageCode name  macroarea
    ##          <int> <chr>        <chr>      <chr>        <fct> <fct>    
    ##  1          40 Kaliai       kali1299   khl          Kali… Papunesia
    ##  2         136 Katcha       katc1250   xtc          Katc… Africa   
    ##  3         155 Nama         nama1265   naq          Nama  Africa   
    ##  4         298 DAN          dann1241   lda          Dan   Africa   
    ##  5         613 WINTU        wint1259   wnw          Wintu North Am…
    ##  6         691 "dan "       dann1241   dnj          Dan   Africa   
    ##  7         871 Korafe       kora1295   kpr          Kora… Papunesia
    ##  8         875 Endo         endo1242   enb          Endo  Africa   
    ##  9         885 Huron        huro1249   wya          Huron North Am…
    ## 10         916 Kuay         kuay1244   kdt          Kuay  Eurasia  
    ## # ... with 39 more rows, and 3 more variables: latitude <dbl>,
    ## #   longitude <dbl>, isocodes <fct>

``` r
languages[which(!(languages$LanguageCode == languages$isocodes)),] %>% filter(isocodes!="")
```

    ## # A tibble: 9 x 9
    ## # Groups:   InventoryID, LanguageName, Glottocode, LanguageCode [9]
    ##   InventoryID LanguageName Glottocode LanguageCode name  macroarea latitude
    ##         <int> <chr>        <chr>      <chr>        <fct> <fct>        <dbl>
    ## 1         298 DAN          dann1241   lda          Dan   Africa        7.23
    ## 2         613 WINTU        wint1259   wnw          Wintu North Am…    39.7 
    ## 3         691 "dan "       dann1241   dnj          Dan   Africa        7.23
    ## 4        1393 Dan          dann1241   lda          Dan   Africa        7.23
    ## 5        1772 Nepali       nepa1254   nep          Nepa… Eurasia      28   
    ## 6        1775 Oriya        oriy1255   ori          Odia  Eurasia      21   
    ## 7        2181 Estonian     esto1258   est          Esto… Eurasia      58.6 
    ## 8        2204 Nepali       nepa1254   nep          Nepa… Eurasia      28   
    ## 9        2256 Darkhat      dark1243   NA           Dark… Eurasia      50.6 
    ## # ... with 2 more variables: longitude <dbl>, isocodes <fct>

``` r
# Create languages.csv
languages.csv <- ungroup(languages)
languages.csv <- languages.csv %>% select(isocodes, name, macroarea, latitude, longitude, Glottocode)
languages.csv$ISO639P3code <- languages.csv$isocodes
colnames(languages.csv) <- c("ID","Name","Macroarea","Latitude","Longitude","Glottocode","ISO639P3code")
write.csv(languages.csv, file="cldf/languages.csv", row.names=FALSE)
```

parameters.csv
==============

``` r
# parameters.csv
## ID,Name,Description
## 9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ,LATIN SMALL LETTER T - COMBINING MINUS SIGN BELOW - LATIN SMALL LETTER ESH - MODIFIER LETTER SMALL H
## 41D8827BB943E1FB9055A7487293BA79,pˀ,LATIN SMALL LETTER P - MODIFIER LETTER GLOTTAL STOP

parameters <- phoible %>% select(Phoneme) %>% distinct(Phoneme) %>% arrange(Phoneme)
names <- sapply(parameters$Phoneme, function(x) u_char_name(as.u_char(utf8ToInt(x))))
parameters <- mutate(parameters, name = stri_join_list(names, sep = " - "))
rm(names)

# Get the segment name IDs
library(digest)
parameters$ID <- sapply(parameters$name, digest, algo="md5")

# But how to get the b16 encoding?
# https://github.com/clld/phoible/blob/73d140ae4c3377140fe3678b1997d0c13d42ad41/phoible/scripts/initializedb.py#L353-L357
# library(caTools)
# base64encode(x)
# library(hashids)
# hashids::encode_hex(x)

# Create parameters.csv
parameters.csv <- parameters %>% select(ID, Phoneme, name)
colnames(parameters.csv) <- c("ID","Name","Description")
write.csv(parameters.csv, file="cldf/parameters.csv", row.names=FALSE)
```

contributions.csv
=================

``` r
# contributions.csv:
## ID,Name,Description,Contributors
## 1,Korean (SPA),Korean,Stanford Phonology Archive

# The `Name` column contents is a conglomeraton of name in phoible and the uppercased Source field (contributions if this tuff spelled out)
contributions <- phoible %>% group_by(InventoryID, LanguageName, Source) %>% select(InventoryID, LanguageName, Source) %>% distinct() %>% arrange(InventoryID)
contributions$Name <- paste0(contributions$LanguageName, ' (', toupper(contributions$Source), ')')
contributions$Description <- contributions$LanguageName

glimpse(contributions)
```

    ## Observations: 2,628
    ## Variables: 5
    ## $ InventoryID  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...
    ## $ LanguageName <chr> "Korean", "Ket", "Lak", "Kabardian", "Georgian", ...
    ## $ Source       <chr> "spa", "spa", "spa", "spa", "spa", "spa", "spa", ...
    ## $ Name         <chr> "Korean (SPA)", "Ket (SPA)", "Lak (SPA)", "Kabard...
    ## $ Description  <chr> "Korean", "Ket", "Lak", "Kabardian", "Georgian", ...

``` r
# Add the ontributors
contributions$Source[contributions$Source=="spa"] <- "Stanford Phonology Archive"
contributions$Source[contributions$Source=="saphon"] <- "UCLA Phonological Segment Inventory Database"
contributions$Source[contributions$Source=="aa"] <- "Christian Chanard and Rhonda L. Hartell"
contributions$Source[contributions$Source=="ph"] <- "PHOIBLE"
contributions$Source[contributions$Source=="ra"] <- "Ramaswami N."
contributions$Source[contributions$Source=="gm"] <- "Christopher Green and Steven Moran"
contributions$Source[contributions$Source=="uz"] <- "Steven Moran"
contributions$Source[contributions$Source=="saphon"] <- "South American Phonological Inventory Database"
contributions$Source[contributions$Source=="ea"] <- "Dmitry Nikolaev"
contributions$Source[contributions$Source=="er"] <- "Erich Round"

# Create contributions.csv
contributions <- ungroup(contributions)
contributions.csv <- contributions %>% select(InventoryID, Name, Description, Source)
colnames(contributions.csv) <- c("ID","Name","Description","Contributors")
write.csv(contributions.csv, file="cldf/contributions.csv", row.names=FALSE)
```

values.csv
==========

``` r
# values.csv
## ID,Language_ID,Parameter_ID,Value,Code_ID,Comment,Source,Contribution_ID
## 1,kor,9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ Korean (SPA),,,cho1967;kim1972;martin1954;martinlee1969;martin1951;kim1968,1
## 14564,kor,9C6117430968F42700ACD02E4E7442F7,t̠ʃʰ Korean (UPSID),,,kim1986;martin1951;martinlee1969;martin1954;cho1967;kim1972,423

# Note the fields Code_ID, Comment are always empty
# temp <- read.csv('/Users/stiv/Downloads/phoible_dataset.cldf/values.csv', header=T, stringsAsFactors=F)
# expect_true(all(is.na(temp$Code_ID)))
# expect_true(all(is.na(temp$Comment)))
# rm(temp)
```

``` r
# Get values by phoneme per row -- note the ID is dependent on the order of phoible input, so it doesn't match previous CLDF 1.0 values.csv file
values <- phoible %>% select(InventoryID, Phoneme)
values$ROWID <- rownames(values)
head(values)
```

    ##   InventoryID Phoneme ROWID
    ## 1           1       a     1
    ## 2           1      aː     2
    ## 3           1       æ     3
    ## 4           1      æː     4
    ## 5           1       e     5
    ## 6           1      eː     6

``` r
# Get parameter ID
parameter_id <- parameters.csv %>% select(Name, ID)
values <- left_join(values, parameter_id, by=c("Phoneme"="Name"))
head(values)
```

    ##   InventoryID Phoneme ROWID                               ID
    ## 1           1       a     1 3bff843fa918065aa88e47217358c573
    ## 2           1      aː     2 eb735919b725c5b318e42da4ec5f0b56
    ## 3           1       æ     3 135e8fb54aece0d5717d474be163d1db
    ## 4           1      æː     4 ae860bae855ddd8da79f63e45c1f143f
    ## 5           1       e     5 fa1f0cd01c7cb09df4101d6e679ecbba
    ## 6           1      eː     6 a12b4703feda121294abc424ef81fa79

``` r
# Get bibtex keys in semi-colon delimited list
refs <- read.table('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.tsv', sep="\t", header=T, stringsAsFactors=F)
citations <- refs %>% group_by(InventoryID) %>% summarize(Contribution_ID=tolower(paste(BibtexKey, collapse=";")))
head(citations)
```

    ## # A tibble: 6 x 2
    ##   InventoryID Contribution_ID                                             
    ##         <int> <chr>                                                       
    ## 1           1 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972 
    ## 2           2 dulzon1968;krejnovich1968                                   
    ## 3           3 murkelinskij1967;zhirkov1955;khajdakov1966                  
    ## 4           4 kuipers1960                                                 
    ## 5           5 robinswaterson1952;selmer1935;tschenkeli1958;vogt1938;vogt1…
    ## 6           6 morgenstierne1945

``` r
values <- left_join(values, citations)
```

    ## Joining, by = "InventoryID"

``` r
head(values)
```

    ##   InventoryID Phoneme ROWID                               ID
    ## 1           1       a     1 3bff843fa918065aa88e47217358c573
    ## 2           1      aː     2 eb735919b725c5b318e42da4ec5f0b56
    ## 3           1       æ     3 135e8fb54aece0d5717d474be163d1db
    ## 4           1      æː     4 ae860bae855ddd8da79f63e45c1f143f
    ## 5           1       e     5 fa1f0cd01c7cb09df4101d6e679ecbba
    ## 6           1      eː     6 a12b4703feda121294abc424ef81fa79
    ##                                               Contribution_ID
    ## 1 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972
    ## 2 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972
    ## 3 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972
    ## 4 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972
    ## 5 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972
    ## 6 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972

``` r
# Add in the languages.csv IDs
languages <- ungroup(languages)
language_id <- languages %>% select(InventoryID, isocodes)
head(language_id)
```

    ## # A tibble: 6 x 2
    ##   InventoryID isocodes
    ##         <int> <fct>   
    ## 1           1 kor     
    ## 2           2 ket     
    ## 3           3 lbe     
    ## 4           4 kbd     
    ## 5           5 kat     
    ## 6           6 bsk

``` r
values <- left_join(values, language_id)
```

    ## Joining, by = "InventoryID"

``` r
head(values)
```

    ##   InventoryID Phoneme ROWID                               ID
    ## 1           1       a     1 3bff843fa918065aa88e47217358c573
    ## 2           1      aː     2 eb735919b725c5b318e42da4ec5f0b56
    ## 3           1       æ     3 135e8fb54aece0d5717d474be163d1db
    ## 4           1      æː     4 ae860bae855ddd8da79f63e45c1f143f
    ## 5           1       e     5 fa1f0cd01c7cb09df4101d6e679ecbba
    ## 6           1      eː     6 a12b4703feda121294abc424ef81fa79
    ##                                               Contribution_ID isocodes
    ## 1 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 2 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 3 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 4 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 5 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 6 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor

``` r
contributions_id <- contributions %>% select(InventoryID, Name)
head(contributions_id)
```

    ## # A tibble: 6 x 2
    ##   InventoryID Name            
    ##         <int> <chr>           
    ## 1           1 Korean (SPA)    
    ## 2           2 Ket (SPA)       
    ## 3           3 Lak (SPA)       
    ## 4           4 Kabardian (SPA) 
    ## 5           5 Georgian (SPA)  
    ## 6           6 Burushaski (SPA)

``` r
values <- left_join(values, contributions_id)
```

    ## Joining, by = "InventoryID"

``` r
head(values)
```

    ##   InventoryID Phoneme ROWID                               ID
    ## 1           1       a     1 3bff843fa918065aa88e47217358c573
    ## 2           1      aː     2 eb735919b725c5b318e42da4ec5f0b56
    ## 3           1       æ     3 135e8fb54aece0d5717d474be163d1db
    ## 4           1      æː     4 ae860bae855ddd8da79f63e45c1f143f
    ## 5           1       e     5 fa1f0cd01c7cb09df4101d6e679ecbba
    ## 6           1      eː     6 a12b4703feda121294abc424ef81fa79
    ##                                               Contribution_ID isocodes
    ## 1 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 2 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 3 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 4 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 5 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ## 6 cho1967;martin1951;martin1954;martinlee1969;kim1968;kim1972      kor
    ##           Name
    ## 1 Korean (SPA)
    ## 2 Korean (SPA)
    ## 3 Korean (SPA)
    ## 4 Korean (SPA)
    ## 5 Korean (SPA)
    ## 6 Korean (SPA)

``` r
# Create value.csv
values$Value <- paste0(values$Phoneme, " ", values$Name)
# These are blank for the time being (TODO revist)
values$Code_ID <- NA
values$Comment <- NA
# Rename and write to disk
values.csv <- values %>% select(ROWID, isocodes, ID, Value, Code_ID, Comment, Contribution_ID, InventoryID)
colnames(values.csv) <- c("ID","Language_ID","Parameter_ID","Value","Code_ID","Comment","Source","Contribution_ID")
write.csv(values.csv, file="cldf/values.csv", row.names=FALSE)
```
