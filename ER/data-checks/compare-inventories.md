Compare ER inventory sizes
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(knitr)
```

``` r
# Erich's data
old <- read.table("../data/raw/Australian_phonemes_for_PHOIBLE_20170501.txt", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
new <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20180114.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
```

``` r
# Which inventories have changed in size?
old.inv <- old %>% select(Variety_name, IPA_for_PHOIBLE) %>% group_by(Variety_name) %>% summarize(phoneme.count = n())
new.inv <- new %>% select(Variety_name, IPA_for_PHOIBLE) %>% group_by(Variety_name) %>% summarize(phoneme.count = n())

x <- full_join(old.inv, new.inv) # 421
```

    ## Joining, by = c("Variety_name", "phoneme.count")

``` r
d <- x %>% group_by(Variety_name) %>% 
  filter(n()>1) %>% arrange(Variety_name)
dim(d)
```

    ## [1] 44  2

``` r
kable(d)
```

| Variety\_name         |  phoneme.count|
|:----------------------|--------------:|
| Alngith               |             25|
| Alngith               |             28|
| Alyawarr              |             24|
| Alyawarr              |             43|
| Anindilyakwa          |             24|
| Anindilyakwa          |             26|
| Antekerrepenhe        |             25|
| Antekerrepenhe        |             44|
| Ayerrerenge           |             24|
| Ayerrerenge           |             43|
| Boonwurrung           |             20|
| Boonwurrung           |             23|
| Central Arrernte      |             25|
| Central Arrernte      |             44|
| Daungwurrung          |             20|
| Daungwurrung          |             23|
| Eastern Anmatyerre    |             25|
| Eastern Anmatyerre    |             44|
| Eastern Arrernte      |             25|
| Eastern Arrernte      |             44|
| Gambera               |             22|
| Gambera               |             25|
| Kaytetye              |             24|
| Kaytetye              |             43|
| Ladji-Ladji           |             19|
| Ladji-Ladji           |             21|
| Lower Southern Aranda |             25|
| Lower Southern Aranda |             44|
| Nyiyaparli            |             22|
| Nyiyaparli            |             25|
| Umiida                |             22|
| Umiida                |             25|
| Wathawurrung          |             20|
| Wathawurrung          |             23|
| Western Anmatyerre    |             25|
| Western Anmatyerre    |             44|
| Western Arrernte      |             25|
| Western Arrernte      |             44|
| Woiwurrung            |             20|
| Woiwurrung            |             23|
| Wurrugu               |             24|
| Wurrugu               |             23|
| Yawijibaya            |             22|
| Yawijibaya            |             25|

``` r
# Take random example
old %>% filter(Variety_name=="Lower Southern Aranda") %>% select(IPA_for_PHOIBLE)
```

    ##    IPA_for_PHOIBLE
    ## 1                t
    ## 2                ʈ
    ## 3                t̪
    ## 4           \u0236
    ## 5                k
    ## 6                p
    ## 7                n
    ## 8                ɳ
    ## 9                n̪
    ## 10          \u0235
    ## 11               ŋ
    ## 12               m
    ## 13               l
    ## 14               ɭ
    ## 15               l̪
    ## 16          \u0234
    ## 17               r
    ## 18               ɻ
    ## 19               j
    ## 20               w
    ## 21               i
    ## 22               u
    ## 23               a
    ## 24               ɰ
    ## 25               ə

``` r
new %>% filter(Variety_name=="Lower Southern Aranda") %>% select(IPA_for_PHOIBLE)
```

    ##    IPA_for_PHOIBLE
    ## 1                t
    ## 2                ʈ
    ## 3                t̪
    ## 4           \u0236
    ## 5                k
    ## 6                p
    ## 7                n
    ## 8                ɳ
    ## 9                n̪
    ## 10          \u0235
    ## 11               ŋ
    ## 12               m
    ## 13               l
    ## 14               ɭ
    ## 15               l̪
    ## 16          \u0234
    ## 17               r
    ## 18               ɻ
    ## 19               j
    ## 20               w
    ## 21               i
    ## 22               u
    ## 23               a
    ## 24               ɰ
    ## 25               ə
    ## 26              tʷ
    ## 27              ʈʷ
    ## 28              t̪ʷ
    ## 29         \u0236ʷ
    ## 30              kʷ
    ## 31              pʷ
    ## 32              nʷ
    ## 33              ɳʷ
    ## 34              n̪ʷ
    ## 35         \u0235ʷ
    ## 36              ŋʷ
    ## 37              mʷ
    ## 38              lʷ
    ## 39              ɭʷ
    ## 40              l̪ʷ
    ## 41         \u0234ʷ
    ## 42              rʷ
    ## 43              ɻʷ
    ## 44              jʷ

``` r
# Could do some more specific testing here, e.g. which segment are / aren't in each old / new inventory.
```
