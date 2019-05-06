PHOIBLE phoneme class counts
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
```

``` r
# Get data
phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv', stringsAsFactors = F)
```

``` r
# Make sure there aren't any NAs in SegmentClass
table(phoible$SegmentClass, exclude=F)
```

    ## 
    ## consonant      tone     vowel 
    ##     72257      2147     31063

``` r
expect_false(any(is.na(phoible$SegmentClass)))
```

``` r
# Get counts by segment class
phonemes <- phoible %>% group_by(InventoryID, ISO6393, Glottocode, Source) %>% summarize(phonemes=n())
vowels <- phoible %>% group_by(InventoryID, ISO6393, Glottocode, Source) %>% filter(SegmentClass=="vowel") %>% summarize(vowels=n())
consonants <- phoible %>% group_by(InventoryID, ISO6393, Glottocode, Source) %>% filter(SegmentClass=="consonant") %>% summarize(consonants=n())
tones <- phoible %>% group_by(InventoryID, ISO6393, Glottocode, Source) %>% filter(SegmentClass=="tone") %>% summarize(tones=n())
```

``` r
# Merge into new df
phonemes <- left_join(phonemes, consonants)
```

    ## Joining, by = c("InventoryID", "ISO6393", "Glottocode", "Source")

``` r
phonemes <- left_join(phonemes, vowels)
```

    ## Joining, by = c("InventoryID", "ISO6393", "Glottocode", "Source")

``` r
phonemes <- left_join(phonemes, tones)
```

    ## Joining, by = c("InventoryID", "ISO6393", "Glottocode", "Source")

``` r
head(phonemes)
```

    ## # A tibble: 6 x 8
    ## # Groups:   InventoryID, ISO6393, Glottocode [6]
    ##   InventoryID ISO6393 Glottocode Source phonemes consonants vowels tones
    ##         <int> <chr>   <chr>      <chr>     <int>      <int>  <int> <int>
    ## 1           1 kor     kore1280   spa          40         22     18    NA
    ## 2           2 ket     kett1243   spa          32         18     14    NA
    ## 3           3 lbe     lakk1252   spa          69         60      9    NA
    ## 4           4 kbd     kaba1278   spa          56         49      7    NA
    ## 5           5 kat     nucl1302   spa          35         29      6    NA
    ## 6           6 bsk     buru1296   spa          53         38     12     3

``` r
# There shouldn't be any NAs for consonants or vowels (tones OK)
expect_false(any(is.na(phonemes$consonants)))
expect_false(any(is.na(phonemes$vowels)))

# Reassign NAs in tones to zero -- TODO: this ignores the fact that we know that UPSID does not contain tone!!
phonemes$tones[is.na(phonemes$tones)] <- 0
head(phonemes)
```

    ## # A tibble: 6 x 8
    ## # Groups:   InventoryID, ISO6393, Glottocode [6]
    ##   InventoryID ISO6393 Glottocode Source phonemes consonants vowels tones
    ##         <int> <chr>   <chr>      <chr>     <int>      <int>  <int> <dbl>
    ## 1           1 kor     kore1280   spa          40         22     18     0
    ## 2           2 ket     kett1243   spa          32         18     14     0
    ## 3           3 lbe     lakk1252   spa          69         60      9     0
    ## 4           4 kbd     kaba1278   spa          56         49      7     0
    ## 5           5 kat     nucl1302   spa          35         29      6     0
    ## 6           6 bsk     buru1296   spa          53         38     12     3

``` r
range(phonemes$tones)
```

    ## [1]  0 10

``` r
# Check the counts
phonemes$counts.match <- phonemes$consonants + phonemes$vowels + phonemes$tones == phonemes$phonemes
expect_true(all(phonemes$counts.match))
```

``` r
# Write the CSV
df <- phonemes %>% select(-counts.match)
head(df)
```

    ## # A tibble: 6 x 8
    ## # Groups:   InventoryID, ISO6393, Glottocode [6]
    ##   InventoryID ISO6393 Glottocode Source phonemes consonants vowels tones
    ##         <int> <chr>   <chr>      <chr>     <int>      <int>  <int> <dbl>
    ## 1           1 kor     kore1280   spa          40         22     18     0
    ## 2           2 ket     kett1243   spa          32         18     14     0
    ## 3           3 lbe     lakk1252   spa          69         60      9     0
    ## 4           4 kbd     kaba1278   spa          56         49      7     0
    ## 5           5 kat     nucl1302   spa          35         29      6     0
    ## 6           6 bsk     buru1296   spa          53         38     12     3

``` r
write.csv(df, "phoible-counts.csv", row.names = F)
```

``` r
# Total counts
counts <- table(phonemes$phonemes)
barplot(counts)
```

![](segment-counts_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# Total consonants
counts <- table(phonemes$consonants)
barplot(counts)
```

![](segment-counts_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# Total vowels
counts <- table(phonemes$vowels)
barplot(counts)
```

![](segment-counts_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
# Total counts
counts <- table(phonemes$tones)
barplot(counts)
```

![](segment-counts_files/figure-markdown_github/unnamed-chunk-12-1.png)