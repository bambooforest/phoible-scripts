Check for errors in PHOIBLE segment conventions
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(testthat)
library(dplyr)
library(knitr)
```

``` r
load(url('https://github.com/phoible/dev/blob/master/data/phoible.RData?raw=true'))
expect_equal(nrow(phoible), 105467)
```

``` r
# Get a list of unique segments and which sources they appear in
phoneme.source <- phoible %>% select(Phoneme, Source) %>% group_by(Phoneme, Source) %>% distinct()
distinct.segments <- phoneme.source %>% group_by(Phoneme) %>% summarize(Sources=tolower(paste(Source, collapse=";"))) %>% arrange(Phoneme)
expect_equal(nrow(distinct.segments), 3183)
head(distinct.segments)
```

    ## # A tibble: 6 x 2
    ##   Phoneme Sources           
    ##   <chr>   <chr>             
    ## 1 ˥       spa;aa;ph;gm;ra;uz
    ## 2 ˥̰       spa               
    ## 3 ˥˦      spa;ra            
    ## 4 ˥˧      ph;gm             
    ## 5 ˥˧̰      ph                
    ## 6 ˥˧˥     spa

``` r
write.csv(distinct.segments, file="distinct-segments.csv", row.names = F)
```

``` r
# Check for ambigously placed segments, e.g. ring above and below (awful hack here because copy and pasting diacritics in R studio is problematic)
rings.below <- distinct.segments %>% filter(grepl("̥", Phoneme))
rings.above <- distinct.segments %>% filter(grepl("̊", Phoneme))
rings <- rbind(rings.above, rings.below)
rings <- rings %>% arrange(Phoneme)
write.csv(rings, file="rings.csv", row.names = F)

kable(rings)
```

| Phoneme | Sources                            |
|:--------|:-----------------------------------|
| å       | gm                                 |
| ḁ       | upsid                              |
| b̤̥       | spa                                |
| b̥       | uz;ea                              |
| b̥ː      | ea                                 |
| b̥ʰ      | uz                                 |
| ɓ̥       | spa;upsid;aa;gm;saphon             |
| ɓ̥ː      | gm                                 |
| d̪̤̥       | spa                                |
| d̪̥       | ea                                 |
| d̺̥       | ea                                 |
| d̤̥       | spa                                |
| d̥       | uz;ea                              |
| d̥ː      | ea                                 |
| d̥ʲ      | ea                                 |
| d̺̥z̺̥      | ea                                 |
| d̥z̥      | uz;ea                              |
| d̥z̥ʲ     | ea                                 |
| d̥ʑ̥      | ea                                 |
| d̥ʒ̊      | ea                                 |
| d̥ʒ̊      | ea                                 |
| d̥ʒ̥      | ea                                 |
| ɖ̥ʐ̥      | ea                                 |
| ɗ̥       | spa;upsid;aa;gm;saphon             |
| e̞̥       | upsid                              |
| e̥       | gm                                 |
| e̥ː      | gm                                 |
| ə̥       | spa                                |
| ɡ̊       | uz;ea                              |
| ɡ̤̥       | spa                                |
| ɡ̊ː      | ea                                 |
| ɡ̊ʰ      | uz                                 |
| ɡ̊ʷ      | ea                                 |
| ɢ̥       | ea                                 |
| ɠ̥       | aa                                 |
| ɠ̥ʲ      | aa                                 |
| ɠ̥ʷ      | aa                                 |
| ʛ̥       | upsid                              |
| ɣ̊       | ea                                 |
| ɣ̥       | ph                                 |
| ʰl̥      | ea                                 |
| i̥       | spa;upsid;ph                       |
| ɨ̥       | ph                                 |
| j̊       | ea                                 |
| j̥       | spa;upsid;ph;gm                    |
| j̥ʷ      | gm                                 |
| ɟ̥ʝ̥      | ea                                 |
| ʄ̥       | aa;gm                              |
| kʟ͓̥ʼ     | spa                                |
| kʟ̥ʼ     | upsid                              |
| l̪̊       | ea                                 |
| l̪̥       | upsid                              |
| l̥       | spa;upsid;aa;ph;gm;ra;saphon;uz;ea |
| l̪̥|l̥     | upsid                              |
| l̥ː      | gm;ea                              |
| l̥ˠ      | spa;upsid                          |
| l̥ʲ      | spa;upsid;ea                       |
| l̥ʲː     | ea                                 |
| ʟ͓̥       | upsid                              |
| ɫ̥       | ea                                 |
| ɬʟ͓̥      | upsid                              |
| ɭ̥       | upsid;ra;ea                        |
| ʎ̟̥       | uz                                 |
| ʎ̥       | ph                                 |
| m̥       | spa;upsid;ph;gm;saphon;uz;ea       |
| m̥ː      | ea                                 |
| m̥ʰ      | gm                                 |
| m̥ʲ      | ea                                 |
| m̥ʲː     | ea                                 |
| m̥m      | uz                                 |
| m̥p      | gm                                 |
| m̥ʷ      | spa                                |
| ɱ̥f      | ph                                 |
| n̪̊       | ea                                 |
| n̪̥       | upsid;ea                           |
| n̠̥       | upsid                              |
| n̥       | spa;upsid;ph;gm;saphon;uz;ea       |
| n̪̥|n̥     | upsid                              |
| n̥ː      | ea                                 |
| n̥ˠ      | spa;upsid                          |
| n̥ʲ      | spa;upsid;ea                       |
| n̥ʲː     | ea                                 |
| n̥n      | uz                                 |
| n̥ʃ      | ph                                 |
| n̠̥t̠ʃ     | ph                                 |
| ɲ̊       | uz;ea                              |
| ɲ̟̥       | uz                                 |
| ɲ̥       | spa;upsid;ph;gm;saphon;ea          |
| ɲ̥ɲ̥      | uz                                 |
| ɳ̥       | upsid                              |
| ɳʈr̠̥     | spa                                |
| ŋ̊       | uz;ea                              |
| ŋ̥       | spa;upsid;ph;gm;saphon;uz;ea       |
| ŋ̥ʲ      | spa;upsid                          |
| ŋ̥m̥      | upsid                              |
| ŋ̊ŋ      | uz                                 |
| ŋ̥ʷ      | upsid                              |
| ŋ̊ǀ      | gm                                 |
| ŋ̥ǀ͓ʰ     | upsid                              |
| ŋ̥ǀ͓xˀ    | upsid                              |
| ŋ̥ǀ͓ˀ     | upsid                              |
| ŋ̊ǁ      | gm                                 |
| ŋ̥ǁ͓ʰ     | upsid                              |
| ŋ̥ǁ͓ˀ     | upsid                              |
| ŋ̊ǂ      | gm                                 |
| ŋ̥ǂʰ     | upsid                              |
| ŋ̥ǂ͓ˡʰ    | upsid                              |
| ŋ̥ǂ͓ˡxˀ   | upsid                              |
| ŋ̥ǂ͓ˡˀ    | upsid                              |
| ŋ̥ǂxˀ    | upsid                              |
| ŋ̥ǂˀ     | upsid                              |
| ŋ̊ǃ      | gm                                 |
| ŋ̥ǃ      | upsid                              |
| ŋ̥ǃˠˀ    | upsid                              |
| ŋ̥ǃ̠ʰ     | upsid                              |
| ŋ̥ǃˀ     | upsid                              |
| ŋ̥ǃ̠ˀ     | upsid                              |
| ŋ̊ʘ      | gm                                 |
| o̞̥       | upsid                              |
| r̺̥       | ea                                 |
| r̥       | spa;upsid;ph;ra;saphon;ea          |
| r̪̥|r̥     | upsid                              |
| R̪̥|R̥     | upsid                              |
| r̥ː      | ea                                 |
| r̥ʲ      | ea                                 |
| r̥ʲː     | ea                                 |
| ɹ̥       | ea                                 |
| ɹ̪̥|ɹ̥     | upsid                              |
| ɺ̥       | saphon                             |
| ɻ̊       | ea                                 |
| ɽ̊       | ea                                 |
| ɾ̪̊       | ra                                 |
| ɾ̥       | ph;ea                              |
| ɾ̥ˠ      | spa;upsid                          |
| ɾ̪̊ʰ      | ra                                 |
| ɾ̥ʲ      | spa;upsid;ea                       |
| tz̤̥      | spa                                |
| ʈr̠̥      | spa                                |
| ʈɹ̠̥      | upsid                              |
| u̥       | spa;upsid;ph                       |
| ɥ̊       | ea                                 |
| ɥ̥       | ph;gm                              |
| v̥       | ea                                 |
| w̥       | ph;ea                              |
| x̥       | ph                                 |
| xʀ̥      | uz                                 |
| z̥       | ph;ea                              |
| ʒ̊       | ea                                 |
| ˀj̥      | ph                                 |
| ˀl̪̥      | ph                                 |
| ˀm̥      | ph                                 |
| ˀn̪̥      | ph                                 |
| ˀw̥      | ph                                 |

``` r
# Check for ambigously placed segments, e.g. ring above and below (awful hack here because copy and pasting diacritics in R studio is problematic)
syllabic.below <- distinct.segments %>% filter(grepl("̩", Phoneme))
syllabic.above <- distinct.segments %>% filter(grepl("̍", Phoneme))
syllabic <- rbind(syllabic.above, syllabic.below)
syllabic <- syllabic %>% arrange(Phoneme)
write.csv(syllabic, file="syllabic.csv", row.names = F)

kable(syllabic)
```

| Phoneme | Sources            |
|:--------|:-------------------|
| b̩       | spa                |
| d̩       | spa                |
| f̩       | ea                 |
| ɡ̩       | spa                |
| ɡb̩      | spa                |
| ɡ̩ʷ      | spa                |
| ɣ̩       | spa                |
| i̩ː      | uz                 |
| ɟ̰̩       | ph                 |
| l̩       | spa;ea             |
| l̪̩       | spa                |
| ɭ̩       | spa                |
| m̩       | spa;ph;gm;ra;uz;ea |
| n̩       | spa;ph;gm;ra;uz;ea |
| n̪̩       | ea                 |
| n̠̩d̠ʒ     | gm                 |
| ɲ̩       | spa                |
| ŋ̩       | spa;ph;gm;ra;uz;ea |
| ŋ̩ʷ      | spa                |
| r̩       | spa;ea             |
| ɹ̩       | spa;uz             |
| ɹ̪̩       | ea                 |
| ɹ̪̰̩       | ea                 |
| ɹ̪̹̩       | ea                 |
| ɹ̪̩ˠ      | ea                 |
| ɻ̩       | ea                 |
| ɻ̹̩       | ea                 |
| ɽ̩       | ra                 |
| s̩       | ea                 |
| v̩       | spa;ea             |
| ʋ̩       | ea                 |
| z̩       | spa;ea             |
| z̞̩       | spa                |
| z̞̩ˠ      | spa                |
| z̞̩̃ˠ      | spa                |
| ʒ̩       | ea                 |