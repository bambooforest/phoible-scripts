Test PHOIBLE features
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(testthat)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     matches

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
load('/Users/stiv/Github/phoible/data/phoible.RData')
```

How many distinct feature vectors are there?
============================================

``` r
distinct.segments <- phoible %>% select(-InventoryID, -Glottocode, -ISO6393, -LanguageName, -SpecificDialect, -GlyphID, -Allophones, -Marginal, -SegmentClass, -Source) %>% distinct()
head(distinct.segments)
```

    ##   Phoneme tone stress syllabic short long consonantal sonorant continuant
    ## 1       a    0      -        +     -    -           -        +          +
    ## 2      aː    0      -        +     -    +           -        +          +
    ## 3       æ    0      -        +     -    -           -        +          +
    ## 4      æː    0      -        +     -    +           -        +          +
    ## 5       e    0      -        +     -    -           -        +          +
    ## 6      eː    0      -        +     -    +           -        +          +
    ##   delayedRelease approximant tap trill nasal lateral labial round
    ## 1              0           +   -     -     -       -      -     0
    ## 2              0           +   -     -     -       -      -     0
    ## 3              0           +   -     -     -       -      -     0
    ## 4              0           +   -     -     -       -      -     0
    ## 5              0           +   -     -     -       -      -     0
    ## 6              0           +   -     -     -       -      -     0
    ##   labiodental coronal anterior distributed strident dorsal high low front
    ## 1           0       -        0           0        0      +    -   +     -
    ## 2           0       -        0           0        0      +    -   +     -
    ## 3           0       -        0           0        0      +    -   +     +
    ## 4           0       -        0           0        0      +    -   +     +
    ## 5           0       -        0           0        0      +    -   -     +
    ## 6           0       -        0           0        0      +    -   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   -                  -                     +
    ## 2    -     0                   -                  -                     +
    ## 3    -     0                   -                  -                     +
    ## 4    -     0                   -                  -                     +
    ## 5    -     +                   -                  -                     +
    ## 6    -     +                   -                  -                     +
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -             -                  -      0
    ## 2                  -             -                  -      0
    ## 3                  -             -                  -      0
    ## 4                  -             -                  -      0
    ## 5                  -             -                  -      0
    ## 6                  -             -                  -      0
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     0
    ## 2                    -                      -     0
    ## 3                    -                      -     0
    ## 4                    -                      -     0
    ## 5                    -                      -     0
    ## 6                    -                      -     0

Look at the different features' compoents
=========================================

``` r
table(phoible$tone)
```

    ## 
    ##      +      0 
    ##   2151 103330

``` r
table(phoible$stress)
```

    ## 
    ##      -      0 
    ## 103330   2151

``` r
table(phoible$syllabic)
```

    ## 
    ##     -   -,+ -,+,- -,+,+     +   +,- +,+,-     0 
    ## 72225   125    12    12 30711   243     2  2151

``` r
table(phoible$short)
```

    ## 
    ##      -    -,+      +      0 
    ## 103123      5    204   2149

``` r
table(phoible$long)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 
    ## 94846     1    63  8382    40  2149

``` r
table(phoible$consonantal)
```

    ## 
    ##     -   -,+     +   +,- +,+,-     0 
    ## 39049     2 64245    29     5  2151

``` r
table(phoible$sonorant)
```

    ## 
    ##       -     -,+   -,+,-       +     +,-   +,-,-   +,-,+ +,-,+,-       0 
    ##   45474      82       5   55850    1876      16      22       1    2151 
    ##   0,-,+   0,+,- 
    ##       2       2

``` r
phoible %>% filter(sonorant=="+,-,+,-")
```

    ##   InventoryID Glottocode ISO6393 LanguageName SpecificDialect
    ## 1        1576   nucl1683     dbq         Daba            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 014B+0261+006D+0062    ŋɡmb       ŋɡmb    FALSE    consonant     gm    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +  +,-,+,-          -
    ##   delayedRelease approximant tap trill   nasal lateral  labial round
    ## 1              -           -   -     - +,-,+,-       - -,-,+,+     -
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       -        0           0        0 +,+,-,-    +   -     -
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     +
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -             -                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$continuant)
```

    ## 
    ##       -   -,-,+     -,+   -,+,+       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   44556      79     694       4   57964       9    2151      23       1

``` r
phoible %>% filter(continuant=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B0+02B7+0074+0255    ʰʷtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - +,0,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       0,+,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$delayedRelease)
```

    ## 
    ##       -   -,-,+     -,+   -,+,+       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   27439      39     481       7   19487       6   57987      34       1

``` r
phoible %>% filter(delayedRelease=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B0+02B7+0074+0255    ʰʷtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - +,0,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       0,+,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$approximant)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 0,-,+ 
    ## 58959    22    69 44272     6  2151     2

``` r
table(phoible$tap)
```

    ## 
    ##      -  -,-,+    -,+      +      0  0,-,+ 
    ## 102020     12     23   1220   2204      2

``` r
table(phoible$trill)
```

    ## 
    ##      -  -,-,+    -,+      +    +,-      0 
    ## 101422      8     26   1820      2   2203

``` r
table(phoible$nasal)
```

    ## 
    ##       -     -,+   -,+,-       +     +,-   +,-,- +,-,+,-       0   0,+,- 
    ##   85266      84       6   15873    2041      57       1    2151       2

``` r
table(phoible$lateral)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,-     0 0,-,+ 
    ## 98964     5   177    15  4165     3  2151     1

``` r
table(phoible$labial)
```

    ## 
    ##       -   -,-,+ -,-,+,+     -,+   -,+,-   -,+,+       +     +,-   +,-,- 
    ##   71954      11       1     595      10       4   30185     524      21 
    ##   +,-,+   +,+,-   +,0,- +,0,-,-       0 
    ##       1       5       8       1    2161

``` r
table(phoible$round)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 14081    21 17229     1 74149

``` r
table(phoible$labiodental)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 28790     1  2574     1 74115

``` r
table(phoible$coronal)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,- +,-,+     0 0,-,+ 
    ## 66245    11   254    57 36713    36     1  2162     2

``` r
table(phoible$anterior)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 
    ## 11319     2    12 25757     7 68384

``` r
table(phoible$distributed)
```

    ## 
    ##       -   -,-,+     -,+       +     +,-   +,-,+       0   0,-,+   0,+,- 
    ##   22317       8     309   13164       8       2   69651      20       1 
    ## 0,0,-,+ 
    ##       1

``` r
table(phoible$strident)
```

    ## 
    ##       -   -,-,+     -,+   -,+,-       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   25359       9     397       2   11264       5   68424      20       1

``` r
table(phoible$dorsal)
```

    ## 
    ##       -   -,-,+     -,+   -,+,-       +     +,-   +,-,-   +,-,+   +,+,- 
    ##   47012       6     346       6   55501     359      10      54       2 
    ## +,+,-,-       0   0,-,+   0,+,- 0,0,-,+ 
    ##       1    2161      20       2       1

``` r
# Testing a weird looking feature vector -- the segment is the same at source
phoible %>% filter(dorsal=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B0+02B7+0074+0255    ʰʷtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - +,0,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       0,+,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$high)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,+ +,-,0 +,+,-     0 
    ## 19159     2   823     1     2 35616   627    38     1     6 49206

``` r
table(phoible$low)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,- +,-,-     0 
    ## 49973     3   269    21  5599   412     1 49203

``` r
table(phoible$front)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 34242    12   544     2    10 21024   350    14     6     2 49275

``` r
table(phoible$back)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 39812     8   368     5     5 15551   482    19     1     1 49229

``` r
table(phoible$tense)
```

    ## 
    ##     -   -,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ##  6390   179 23420   269     3     6     1 75213

``` r
table(phoible$retractedTongueRoot)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 30956    11   286     2 74226

``` r
table(phoible$advancedTongueRoot)
```

    ## 
    ##     -     +     0 
    ## 31244    11 74226

``` r
table(phoible$periodicGlottalSource)
```

    ## 
    ##     -   -,+ -,+,-     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 31249    80     7 71542   424    33     5     1  2140

``` r
table(phoible$epilaryngealSource)
```

    ## 
    ##      -      +      0 
    ## 103299     31   2151

``` r
table(phoible$spreadGlottis)
```

    ## 
    ##       -   -,-,+     -,+       +     +,-   +,-,-   +,-,+       0   0,-,+ 
    ##   96854      17     212    6115     130       3       1    2139       4 
    ##   0,+,- 0,+,-,- 
    ##       5       1

``` r
table(phoible$constrictedGlottis)
```

    ## 
    ##     - -,-,+   -,+     +   +,- +,-,-     0 
    ## 99730    17    99  3358   126    12  2139

``` r
table(phoible$fortis)
```

    ## 
    ##     -     +     0 
    ## 71844   415 33222

``` r
table(phoible$raisedLarynxEjective)
```

    ## 
    ##      -  -,-,+    -,+      +    +,-  +,-,-      0 
    ## 101660      9     80   1558     21      2   2151

``` r
table(phoible$loweredLarynxImplosive)
```

    ## 
    ##      -    -,+      +    +,-      0 
    ## 102605      7    716      2   2151

``` r
table(phoible$click)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +     0 0,-,+ 
    ## 71948     6   231    62    10 33222     2

Compare features to phoible 2104
--------------------------------

``` r
old <- read.delim("phoible-segments-features.tsv", header=T)
glimpse(old)
```

    ## Observations: 2,182
    ## Variables: 38
    ## $ segment                <fct> m, k, i, a, j, p, u, w, n, s, t, b, o, ...
    ## $ tone                   <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ stress                 <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ syllabic               <fct> -, -, +, +, -, -, +, -, -, -, -, -, +, ...
    ## $ short                  <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ long                   <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ consonantal            <fct> +, +, -, -, -, +, -, -, +, +, +, +, -, ...
    ## $ sonorant               <fct> +, -, +, +, +, -, +, +, +, -, -, -, +, ...
    ## $ continuant             <fct> -, -, +, +, +, -, +, +, -, +, -, -, +, ...
    ## $ delayedRelease         <fct> 0, -, 0, 0, 0, -, 0, 0, 0, +, -, -, 0, ...
    ## $ approximant            <fct> -, -, +, +, +, -, +, +, -, -, -, -, +, ...
    ## $ tap                    <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ trill                  <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ nasal                  <fct> +, -, -, -, -, -, -, -, +, -, -, -, -, ...
    ## $ lateral                <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ labial                 <fct> +, -, -, -, -, +, +, +, -, -, -, +, +, ...
    ## $ round                  <fct> -, 0, 0, 0, 0, -, +, +, 0, 0, 0, -, +, ...
    ## $ labiodental            <fct> -, 0, 0, 0, 0, -, -, -, 0, 0, 0, -, -, ...
    ## $ coronal                <fct> -, -, -, -, -, -, -, -, +, +, +, -, -, ...
    ## $ anterior               <fct> 0, 0, 0, 0, 0, 0, 0, 0, +, +, +, 0, 0, ...
    ## $ distributed            <fct> 0, 0, 0, 0, 0, 0, 0, 0, -, -, -, 0, 0, ...
    ## $ strident               <fct> 0, 0, 0, 0, 0, 0, 0, 0, -, +, -, 0, 0, ...
    ## $ dorsal                 <fct> -, +, +, +, +, -, +, +, -, -, -, -, +, ...
    ## $ high                   <fct> 0, +, +, -, +, 0, +, +, 0, 0, 0, 0, -, ...
    ## $ low                    <fct> 0, -, -, +, -, 0, -, -, 0, 0, 0, 0, -, ...
    ## $ front                  <fct> 0, -, +, -, +, 0, -, -, 0, 0, 0, 0, -, ...
    ## $ back                   <fct> 0, -, -, -, -, 0, +, +, 0, 0, 0, 0, +, ...
    ## $ tense                  <fct> 0, 0, +, 0, +, 0, +, +, 0, 0, 0, 0, +, ...
    ## $ retractedTongueRoot    <fct> 0, 0, -, -, 0, 0, -, 0, 0, 0, 0, 0, -, ...
    ## $ advancedTongueRoot     <fct> 0, 0, -, -, 0, 0, -, 0, 0, 0, 0, 0, -, ...
    ## $ periodicGlottalSource  <fct> +, -, +, +, +, -, +, +, +, -, -, +, +, ...
    ## $ epilaryngealSource     <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ spreadGlottis          <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ constrictedGlottis     <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ fortis                 <fct> -, -, 0, 0, -, -, 0, -, -, -, -, -, 0, ...
    ## $ raisedLarynxEjective   <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ loweredLarynxImplosive <fct> -, -, -, -, -, -, -, -, -, -, -, -, -, ...
    ## $ click                  <fct> -, -, 0, 0, -, -, 0, -, -, -, -, -, 0, ...

``` r
x <- phoible %>% filter(Phoneme %in% old$segment) %>% select(-InventoryID, -Glottocode, -ISO6393, -LanguageName, -SpecificDialect, -GlyphID, -Allophones, -Marginal, -SegmentClass, -Source) %>% distinct()
glimpse(x)
```

    ## Observations: 1,841
    ## Variables: 38
    ## $ Phoneme                <chr> "a", "aː", "æ", "æː", "e", "eː", "ɤ", "...
    ## $ tone                   <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ stress                 <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ syllabic               <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ short                  <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ long                   <chr> "-", "+", "-", "+", "-", "+", "-", "+",...
    ## $ consonantal            <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ sonorant               <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ continuant             <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ delayedRelease         <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ approximant            <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ tap                    <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ trill                  <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ nasal                  <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ lateral                <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ labial                 <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ round                  <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ labiodental            <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ coronal                <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ anterior               <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ distributed            <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ strident               <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ dorsal                 <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ high                   <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ low                    <chr> "+", "+", "+", "+", "-", "-", "-", "-",...
    ## $ front                  <chr> "-", "-", "+", "+", "+", "+", "-", "-",...
    ## $ back                   <chr> "-", "-", "-", "-", "-", "-", "+", "+",...
    ## $ tense                  <chr> "0", "0", "0", "0", "+", "+", "+", "+",...
    ## $ retractedTongueRoot    <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ advancedTongueRoot     <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ periodicGlottalSource  <chr> "+", "+", "+", "+", "+", "+", "+", "+",...
    ## $ epilaryngealSource     <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ spreadGlottis          <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ constrictedGlottis     <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ fortis                 <chr> "0", "0", "0", "0", "0", "0", "0", "0",...
    ## $ raisedLarynxEjective   <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ loweredLarynxImplosive <chr> "-", "-", "-", "-", "-", "-", "-", "-",...
    ## $ click                  <chr> "0", "0", "0", "0", "0", "0", "0", "0",...

Individual spacing modifier letters and where they occur
========================================================

``` r
# epilaryngeal source (only occurs in 1379, !Xoo)
phoible %>% filter(grepl("ᴱ", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ##   InventoryID LanguageName Phoneme
    ## 1        1379         !Xóõ a\u1d31
    ## 2        1379         !Xóõ o\u1d31
    ## 3        1379         !Xóõ u\u1d31

``` r
# unaspirated
phoible %>% filter(grepl("˭", Allophones)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ## [1] InventoryID  LanguageName Phoneme     
    ## <0 rows> (or 0-length row.names)

``` r
# labial-palatalized  # TODO: should use labial+palatal?
phoible %>% filter(grepl("ᶣ", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ##    InventoryID      LanguageName  Phoneme
    ## 1         1245              Akan  ɕ\u1da3
    ## 2         1245              Akan dʑ\u1da3
    ## 3         1245              Akan  ɲ\u1da3
    ## 4         1245              Akan  s\u1da3
    ## 5         1245              Akan tɕ\u1da3
    ## 6         1246             anufɔ  ɕ\u1da3
    ## 7         1246             anufɔ  l\u1da3
    ## 8         1246             anufɔ  ʑ\u1da3
    ## 9         1257           songhoy  ɡ\u1da3
    ## 10        1302           Gechode  s\u1da3
    ## 11        1357 Gwandara (Nimbia)  ɡ\u1da3
    ## 12        1357 Gwandara (Nimbia)  k\u1da3
    ## 13        1357 Gwandara (Nimbia)  r\u1da3
    ## 14        1523             Efutu  d\u1da3
    ## 15        1523             Efutu dʑ\u1da3
    ## 16        1523             Efutu  ɲ\u1da3
    ## 17        1523             Efutu tɕ\u1da3
    ## 18        1601            Mfumte  h\u1da3
    ## 19        1601            Mfumte ŋɣ\u1da3
    ## 20        1601            Mfumte ŋh\u1da3
    ## 21        1632            Pinyin  ɡ\u1da3
    ## 22        1632            Pinyin  k\u1da3

``` r
# schwa-like release # TODO: check what this really is
phoible %>% filter(grepl("ᵊ", Allophones)) %>% select(InventoryID, LanguageName, Phoneme, Allophones)
```

    ##   InventoryID LanguageName Phoneme Allophones
    ## 1        1407      Mbodomo       e  e ɛ\u1d4a
    ## 2        1407      Mbodomo       i  i i\u1d4a
