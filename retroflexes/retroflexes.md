PHOIBLE retroflexes in South Asia
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
```

``` r
# Get PHOIBLE data and merge in Glottolog metadata
phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv', stringsAsFactors = F)

# Merge in Glottolog 3.3 data
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/glottolog_languoid.csv.zip
languoids <- read.csv('../data/glottolog_languoid.csv/languoid.csv', stringsAsFactors = FALSE) 
geo <- read.csv(url("https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv"), stringsAsFactors = FALSE)

phoible <- left_join(phoible, languoids, by=c("Glottocode"="id"))
phoible <- left_join(phoible, geo)
```

    ## Joining, by = c("name", "level", "latitude", "longitude")

``` r
# What do we consider retroflexes in PHOIBLE by phonological features?
retros.by.features <- phoible %>% filter(coronal %in% "+") %>% filter(anterior %in% "-") %>% select(Phoneme) %>% distinct()
```

``` r
# Retroflexes from Yoav
retros <- c("ʈ", "ɖ", "ɳ", "ɽ", "ʐ", "ʂ", "ɻ", "ɭ")
```

``` r
# table(phoible$country_ids)
se.asia <- phoible %>% filter(grepl("BT|IN|BD|MV|NP|PK|LK|TJ", country_ids))

# Number of data points
nrow(se.asia %>% select(InventoryID, Glottocode, LanguageName) %>% distinct())
```

    ## [1] 286

``` r
# Number of unique Glottocodes
nrow(se.asia %>% select(Glottocode) %>% distinct())
```

    ## [1] 173

``` r
# Does the language contain "pure" retroflexes?
r1 <- se.asia %>% group_by(InventoryID, Glottocode, LanguageName) %>% summarize(has.retroflex = any(Phoneme %in% c("ʈ", "ɖ", "ɳ", "ɽ", "ʐ", "ʂ", "ɻ", "ɭ")))

# Does the language contain regrex matched retroflexes?
r2 <- se.asia %>% group_by(InventoryID, Glottocode, LanguageName) %>% summarize(has.retroflex = any(grepl("ʈ|ɖ|ɳ|ɽ|ʐ|ʂ|ɻ|ɭ", Phoneme)))

# Numbers differ
table(r1$has.retroflex)
```

    ## 
    ## FALSE  TRUE 
    ##    70   216

``` r
table(r2$has.retroflex)
```

    ## 
    ## FALSE  TRUE 
    ##    67   219

``` r
# Which data points differ?
x <- left_join(r1, r2, by=c("InventoryID"="InventoryID"))
x$diff <- x$has.retroflex.x != x$has.retroflex.y
x[which(x$diff),]
```

    ## # A tibble: 3 x 8
    ## # Groups:   InventoryID, Glottocode.x [3]
    ##   InventoryID Glottocode.x LanguageName.x has.retroflex.x Glottocode.y
    ##         <int> <chr>        <chr>          <lgl>           <chr>       
    ## 1         497 aona1235     AO             FALSE           aona1235    
    ## 2        2525 kham1282     Kami Tibetan   FALSE           kham1282    
    ## 3        2578 lisu1250     Lisu           FALSE           lisu1250    
    ## # ... with 3 more variables: LanguageName.y <chr>, has.retroflex.y <lgl>,
    ## #   diff <lgl>

``` r
# How do they differ?
phoible %>% filter(InventoryID %in% c(497, 2525, 2578)) %>% select(InventoryID, Glottocode, LanguageName, Phoneme, Allophones, country_ids)
```

    ##     InventoryID Glottocode LanguageName Phoneme Allophones country_ids
    ## 1           497   aona1235           AO       a       <NA>          IN
    ## 2           497   aona1235           AO      cç       <NA>          IN
    ## 3           497   aona1235           AO       e       <NA>          IN
    ## 4           497   aona1235           AO       i       <NA>          IN
    ## 5           497   aona1235           AO       j       <NA>          IN
    ## 6           497   aona1235           AO       k       <NA>          IN
    ## 7           497   aona1235           AO       l       <NA>          IN
    ## 8           497   aona1235           AO ɭ\u0353       <NA>          IN
    ## 9           497   aona1235           AO       m       <NA>          IN
    ## 10          497   aona1235           AO       n       <NA>          IN
    ## 11          497   aona1235           AO       ŋ       <NA>          IN
    ## 12          497   aona1235           AO       o       <NA>          IN
    ## 13          497   aona1235           AO       p       <NA>          IN
    ## 14          497   aona1235           AO       s       <NA>          IN
    ## 15          497   aona1235           AO       t       <NA>          IN
    ## 16          497   aona1235           AO       u       <NA>          IN
    ## 17          497   aona1235           AO       ɯ       <NA>          IN
    ## 18          497   aona1235           AO       w       <NA>          IN
    ## 19          497   aona1235           AO       z       <NA>          IN
    ## 20          497   aona1235           AO       ʔ       <NA>          IN
    ## 21         2525   kham1282 Kami Tibetan       a       <NA>    CN IN MM
    ## 22         2525   kham1282 Kami Tibetan       ɑ̃       <NA>    CN IN MM
    ## 23         2525   kham1282 Kami Tibetan       b       <NA>    CN IN MM
    ## 24         2525   kham1282 Kami Tibetan       ɕ       <NA>    CN IN MM
    ## 25         2525   kham1282 Kami Tibetan      ɕʰ       <NA>    CN IN MM
    ## 26         2525   kham1282 Kami Tibetan       d       <NA>    CN IN MM
    ## 27         2525   kham1282 Kami Tibetan      dz       <NA>    CN IN MM
    ## 28         2525   kham1282 Kami Tibetan      dʑ       <NA>    CN IN MM
    ## 29         2525   kham1282 Kami Tibetan      d̠ʒ       <NA>    CN IN MM
    ## 30         2525   kham1282 Kami Tibetan      ɖʐ       <NA>    CN IN MM
    ## 31         2525   kham1282 Kami Tibetan       e       <NA>    CN IN MM
    ## 32         2525   kham1282 Kami Tibetan       ẽ       <NA>    CN IN MM
    ## 33         2525   kham1282 Kami Tibetan      ei       <NA>    CN IN MM
    ## 34         2525   kham1282 Kami Tibetan       ə       <NA>    CN IN MM
    ## 35         2525   kham1282 Kami Tibetan       ɛ       <NA>    CN IN MM
    ## 36         2525   kham1282 Kami Tibetan       ɤ       <NA>    CN IN MM
    ## 37         2525   kham1282 Kami Tibetan       ɡ       <NA>    CN IN MM
    ## 38         2525   kham1282 Kami Tibetan       ɣ       <NA>    CN IN MM
    ## 39         2525   kham1282 Kami Tibetan       h       <NA>    CN IN MM
    ## 40         2525   kham1282 Kami Tibetan       ɦ       <NA>    CN IN MM
    ## 41         2525   kham1282 Kami Tibetan       i       <NA>    CN IN MM
    ## 42         2525   kham1282 Kami Tibetan       ĩ       <NA>    CN IN MM
    ## 43         2525   kham1282 Kami Tibetan       j       <NA>    CN IN MM
    ## 44         2525   kham1282 Kami Tibetan       j̊       <NA>    CN IN MM
    ## 45         2525   kham1282 Kami Tibetan       k       <NA>    CN IN MM
    ## 46         2525   kham1282 Kami Tibetan      kʰ       <NA>    CN IN MM
    ## 47         2525   kham1282 Kami Tibetan       l       <NA>    CN IN MM
    ## 48         2525   kham1282 Kami Tibetan       l̥       <NA>    CN IN MM
    ## 49         2525   kham1282 Kami Tibetan       m       <NA>    CN IN MM
    ## 50         2525   kham1282 Kami Tibetan       m̥       <NA>    CN IN MM
    ## 51         2525   kham1282 Kami Tibetan       n       <NA>    CN IN MM
    ## 52         2525   kham1282 Kami Tibetan       n̥       <NA>    CN IN MM
    ## 53         2525   kham1282 Kami Tibetan      ⁿb       <NA>    CN IN MM
    ## 54         2525   kham1282 Kami Tibetan      ⁿd       <NA>    CN IN MM
    ## 55         2525   kham1282 Kami Tibetan     ⁿdz       <NA>    CN IN MM
    ## 56         2525   kham1282 Kami Tibetan     ⁿdʑ       <NA>    CN IN MM
    ## 57         2525   kham1282 Kami Tibetan     ⁿd̠ʒ       <NA>    CN IN MM
    ## 58         2525   kham1282 Kami Tibetan     ⁿɖʐ       <NA>    CN IN MM
    ## 59         2525   kham1282 Kami Tibetan      ⁿɡ       <NA>    CN IN MM
    ## 60         2525   kham1282 Kami Tibetan       ɲ       <NA>    CN IN MM
    ## 61         2525   kham1282 Kami Tibetan       ɲ̊       <NA>    CN IN MM
    ## 62         2525   kham1282 Kami Tibetan       ŋ       <NA>    CN IN MM
    ## 63         2525   kham1282 Kami Tibetan       ŋ̊       <NA>    CN IN MM
    ## 64         2525   kham1282 Kami Tibetan      ou       <NA>    CN IN MM
    ## 65         2525   kham1282 Kami Tibetan       ɔ       <NA>    CN IN MM
    ## 66         2525   kham1282 Kami Tibetan       ɔ̃       <NA>    CN IN MM
    ## 67         2525   kham1282 Kami Tibetan       p       <NA>    CN IN MM
    ## 68         2525   kham1282 Kami Tibetan      pʰ       <NA>    CN IN MM
    ## 69         2525   kham1282 Kami Tibetan       ɹ       <NA>    CN IN MM
    ## 70         2525   kham1282 Kami Tibetan       ɹ̥       <NA>    CN IN MM
    ## 71         2525   kham1282 Kami Tibetan       s       <NA>    CN IN MM
    ## 72         2525   kham1282 Kami Tibetan      sʰ       <NA>    CN IN MM
    ## 73         2525   kham1282 Kami Tibetan       ʃ       <NA>    CN IN MM
    ## 74         2525   kham1282 Kami Tibetan      ʃʰ       <NA>    CN IN MM
    ## 75         2525   kham1282 Kami Tibetan       t       <NA>    CN IN MM
    ## 76         2525   kham1282 Kami Tibetan      tɕ       <NA>    CN IN MM
    ## 77         2525   kham1282 Kami Tibetan     tɕʰ       <NA>    CN IN MM
    ## 78         2525   kham1282 Kami Tibetan      tʰ       <NA>    CN IN MM
    ## 79         2525   kham1282 Kami Tibetan      ts       <NA>    CN IN MM
    ## 80         2525   kham1282 Kami Tibetan     tsʰ       <NA>    CN IN MM
    ## 81         2525   kham1282 Kami Tibetan      t̠ʃ       <NA>    CN IN MM
    ## 82         2525   kham1282 Kami Tibetan     t̠ʃʰ       <NA>    CN IN MM
    ## 83         2525   kham1282 Kami Tibetan      ʈʂ       <NA>    CN IN MM
    ## 84         2525   kham1282 Kami Tibetan     ʈʂʰ       <NA>    CN IN MM
    ## 85         2525   kham1282 Kami Tibetan       u       <NA>    CN IN MM
    ## 86         2525   kham1282 Kami Tibetan       ũ       <NA>    CN IN MM
    ## 87         2525   kham1282 Kami Tibetan      ua       <NA>    CN IN MM
    ## 88         2525   kham1282 Kami Tibetan      ue       <NA>    CN IN MM
    ## 89         2525   kham1282 Kami Tibetan      uẽ       <NA>    CN IN MM
    ## 90         2525   kham1282 Kami Tibetan      ui       <NA>    CN IN MM
    ## 91         2525   kham1282 Kami Tibetan       ɯ       <NA>    CN IN MM
    ## 92         2525   kham1282 Kami Tibetan       ɯ̃       <NA>    CN IN MM
    ## 93         2525   kham1282 Kami Tibetan       w       <NA>    CN IN MM
    ## 94         2525   kham1282 Kami Tibetan       x       <NA>    CN IN MM
    ## 95         2525   kham1282 Kami Tibetan       z       <NA>    CN IN MM
    ## 96         2525   kham1282 Kami Tibetan       ʑ       <NA>    CN IN MM
    ## 97         2525   kham1282 Kami Tibetan       ʒ       <NA>    CN IN MM
    ## 98         2578   lisu1250         Lisu       ɑ       <NA> CN IN MM TH
    ## 99         2578   lisu1250         Lisu       b       <NA> CN IN MM TH
    ## 100        2578   lisu1250         Lisu       ɕ       <NA> CN IN MM TH
    ## 101        2578   lisu1250         Lisu       d       <NA> CN IN MM TH
    ## 102        2578   lisu1250         Lisu      dz       <NA> CN IN MM TH
    ## 103        2578   lisu1250         Lisu      dʑ       <NA> CN IN MM TH
    ## 104        2578   lisu1250         Lisu       e       <NA> CN IN MM TH
    ## 105        2578   lisu1250         Lisu       ɛ       <NA> CN IN MM TH
    ## 106        2578   lisu1250         Lisu       ɤ       <NA> CN IN MM TH
    ## 107        2578   lisu1250         Lisu       f       <NA> CN IN MM TH
    ## 108        2578   lisu1250         Lisu       ɡ       <NA> CN IN MM TH
    ## 109        2578   lisu1250         Lisu       ɣ       <NA> CN IN MM TH
    ## 110        2578   lisu1250         Lisu       h       <NA> CN IN MM TH
    ## 111        2578   lisu1250         Lisu       h̃       <NA> CN IN MM TH
    ## 112        2578   lisu1250         Lisu       i       <NA> CN IN MM TH
    ## 113        2578   lisu1250         Lisu       j       <NA> CN IN MM TH
    ## 114        2578   lisu1250         Lisu       k       <NA> CN IN MM TH
    ## 115        2578   lisu1250         Lisu      kʰ       <NA> CN IN MM TH
    ## 116        2578   lisu1250         Lisu       l       <NA> CN IN MM TH
    ## 117        2578   lisu1250         Lisu       m       <NA> CN IN MM TH
    ## 118        2578   lisu1250         Lisu       n       <NA> CN IN MM TH
    ## 119        2578   lisu1250         Lisu       ɲ       <NA> CN IN MM TH
    ## 120        2578   lisu1250         Lisu       ŋ       <NA> CN IN MM TH
    ## 121        2578   lisu1250         Lisu       o       <NA> CN IN MM TH
    ## 122        2578   lisu1250         Lisu       ø       <NA> CN IN MM TH
    ## 123        2578   lisu1250         Lisu       p       <NA> CN IN MM TH
    ## 124        2578   lisu1250         Lisu      pʰ       <NA> CN IN MM TH
    ## 125        2578   lisu1250         Lisu       s       <NA> CN IN MM TH
    ## 126        2578   lisu1250         Lisu       t       <NA> CN IN MM TH
    ## 127        2578   lisu1250         Lisu      tɕ       <NA> CN IN MM TH
    ## 128        2578   lisu1250         Lisu     tɕʰ       <NA> CN IN MM TH
    ## 129        2578   lisu1250         Lisu      tʰ       <NA> CN IN MM TH
    ## 130        2578   lisu1250         Lisu      ts       <NA> CN IN MM TH
    ## 131        2578   lisu1250         Lisu     tsʰ       <NA> CN IN MM TH
    ## 132        2578   lisu1250         Lisu       u       <NA> CN IN MM TH
    ## 133        2578   lisu1250         Lisu       ɯ       <NA> CN IN MM TH
    ## 134        2578   lisu1250         Lisu       w       <NA> CN IN MM TH
    ## 135        2578   lisu1250         Lisu       x       <NA> CN IN MM TH
    ## 136        2578   lisu1250         Lisu       y       <NA> CN IN MM TH
    ## 137        2578   lisu1250         Lisu       z       <NA> CN IN MM TH
    ## 138        2578   lisu1250         Lisu       ʐ̩       <NA> CN IN MM TH
    ## 139        2578   lisu1250         Lisu       ʔ       <NA> CN IN MM TH

``` r
# Which languages don't have retroflexes?
r2 %>% filter(!has.retroflex)
```

    ## # A tibble: 67 x 4
    ## # Groups:   InventoryID, Glottocode [67]
    ##    InventoryID Glottocode LanguageName has.retroflex
    ##          <int> <chr>      <chr>        <lgl>        
    ##  1          21 nyis1236   Dafla        FALSE        
    ##  2          22 nucl1310   Burmese      FALSE        
    ##  3          24 garo1247   Garo         FALSE        
    ##  4         188 kirg1245   Kirghiz      FALSE        
    ##  5         227 apuc1241   ANDAMANESE   FALSE        
    ##  6         261 brah1256   BRAHUI       FALSE        
    ##  7         263 bodo1269   BODO         FALSE        
    ##  8         271 carn1240   NICOBARESE   FALSE        
    ##  9         287 haka1240   LAI          FALSE        
    ## 10         293 tedi1235   TIDDIM CHIN  FALSE        
    ## # ... with 57 more rows

``` r
no.retroflexes <- r2 %>% filter(!has.retroflex)
no.retroflexes
```

    ## # A tibble: 67 x 4
    ## # Groups:   InventoryID, Glottocode [67]
    ##    InventoryID Glottocode LanguageName has.retroflex
    ##          <int> <chr>      <chr>        <lgl>        
    ##  1          21 nyis1236   Dafla        FALSE        
    ##  2          22 nucl1310   Burmese      FALSE        
    ##  3          24 garo1247   Garo         FALSE        
    ##  4         188 kirg1245   Kirghiz      FALSE        
    ##  5         227 apuc1241   ANDAMANESE   FALSE        
    ##  6         261 brah1256   BRAHUI       FALSE        
    ##  7         263 bodo1269   BODO         FALSE        
    ##  8         271 carn1240   NICOBARESE   FALSE        
    ##  9         287 haka1240   LAI          FALSE        
    ## 10         293 tedi1235   TIDDIM CHIN  FALSE        
    ## # ... with 57 more rows

``` r
# Here's an example
phoible %>% filter(InventoryID==21) %>% select(InventoryID, Glottocode, LanguageName, Phoneme, country_ids)
```

    ##    InventoryID Glottocode LanguageName Phoneme country_ids
    ## 1           21   nyis1236        Dafla      ˥˦          IN
    ## 2           21   nyis1236        Dafla      ˩˨          IN
    ## 3           21   nyis1236        Dafla       ɑ          IN
    ## 4           21   nyis1236        Dafla       b          IN
    ## 5           21   nyis1236        Dafla       d          IN
    ## 6           21   nyis1236        Dafla       ɛ          IN
    ## 7           21   nyis1236        Dafla       ɡ          IN
    ## 8           21   nyis1236        Dafla       ɦ          IN
    ## 9           21   nyis1236        Dafla       i          IN
    ## 10          21   nyis1236        Dafla       i̥          IN
    ## 11          21   nyis1236        Dafla       j          IN
    ## 12          21   nyis1236        Dafla       k          IN
    ## 13          21   nyis1236        Dafla       l̪          IN
    ## 14          21   nyis1236        Dafla       m          IN
    ## 15          21   nyis1236        Dafla       n          IN
    ## 16          21   nyis1236        Dafla       ŋ          IN
    ## 17          21   nyis1236        Dafla       ɔ          IN
    ## 18          21   nyis1236        Dafla       p          IN
    ## 19          21   nyis1236        Dafla       r          IN
    ## 20          21   nyis1236        Dafla       s          IN
    ## 21          21   nyis1236        Dafla       t          IN
    ## 22          21   nyis1236        Dafla       u          IN
    ## 23          21   nyis1236        Dafla       u̥          IN
    ## 24          21   nyis1236        Dafla       ɯ          IN
    ## 25          21   nyis1236        Dafla       ʌ          IN
    ## 26          21   nyis1236        Dafla       x          IN
    ## 27          21   nyis1236        Dafla       ʔ          IN

``` r
# Write all the language data to CSV
df <- phoible %>% filter(InventoryID %in% no.retroflexes$InventoryID) %>% select(InventoryID, Glottocode, LanguageName, Phoneme, country_ids)
write.csv(df, file="phoible-langs-no-retroflexes-seasia.csv", row.names = F)
```

``` r
# Specifically, I am looking for the languages in South Asia (Indian sub-continent) which do not have any retroflex sounds. I have already collected the PHOIBLE data for the languages which do have retroflex sounds in that area. The area of interest includes India, Pakistan, Nepal, Bangladesh, Bhutan, and the Tibetan plateau.
```
