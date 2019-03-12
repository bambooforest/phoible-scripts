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

``` r
dim(distinct.segments)
```

    ## [1] 3175   38

Look at the different features' components and check out weird stuff
====================================================================

``` r
table(phoible$tone)
```

    ## 
    ##      +      0 
    ##   2147 103330

``` r
table(phoible$stress)
```

    ## 
    ##      -      0 
    ## 103330   2147

``` r
table(phoible$syllabic)
```

    ## 
    ##     -   -,+ -,+,- -,+,+     +   +,- +,+,-     0 
    ## 72225   125    12    12 30711   243     2  2147

``` r
table(phoible$short)
```

    ## 
    ##      -    -,+      +      0 
    ## 103121      5    204   2147

``` r
table(phoible$long)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 
    ## 94846     1    63  8380    40  2147

``` r
table(phoible$consonantal)
```

    ## 
    ##     -   -,+     +   +,- +,+,-     0 
    ## 39049     2 64245    29     5  2147

``` r
table(phoible$sonorant)
```

    ## 
    ##       -     -,+   -,+,-       +     +,-   +,-,-   +,-,+ +,-,+,-       0 
    ##   45474      82       5   55850    1876      16      22       1    2147 
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
    ##   44556      79     694       4   57964       9    2147      23       1

``` r
phoible %>% filter(continuant=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B7+02B0+0074+0255    ʷʰtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - 0,+,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       +,0,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$delayedRelease)
```

    ## 
    ##       -   -,-,+     -,+   -,+,+       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   27439      39     481       7   19487       6   57983      34       1

``` r
phoible %>% filter(delayedRelease=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B7+02B0+0074+0255    ʷʰtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - 0,+,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       +,0,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$approximant)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 0,-,+ 
    ## 58959    22    69 44272     6  2147     2

``` r
table(phoible$tap)
```

    ## 
    ##      -  -,-,+    -,+      +      0  0,-,+ 
    ## 102020     12     23   1220   2200      2

``` r
table(phoible$trill)
```

    ## 
    ##      -  -,-,+    -,+      +    +,-      0 
    ## 101422      8     26   1820      2   2199

``` r
table(phoible$nasal)
```

    ## 
    ##       -     -,+   -,+,-       +     +,-   +,-,- +,-,+,-       0   0,+,- 
    ##   85266      84       6   15873    2041      57       1    2147       2

``` r
table(phoible$lateral)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,-     0 0,-,+ 
    ## 98964     5   177    15  4165     3  2147     1

``` r
table(phoible$labial)
```

    ## 
    ##       -   -,-,+ -,-,+,+     -,+   -,+,-   -,+,+       +     +,-   +,-,- 
    ##   71954      11       1     595      10       4   30185     524      21 
    ##   +,-,+   +,+,-       0   0,+,- 0,+,-,- 
    ##       1       5    2157       8       1

``` r
table(phoible$round)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 14081    21 17229     1 74145

``` r
table(phoible$labiodental)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 28790     1  2574     1 74111

``` r
table(phoible$coronal)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,- +,-,+     0 0,-,+ 
    ## 66245    11   254    57 36713    36     1  2158     2

``` r
table(phoible$anterior)
```

    ## 
    ##     - -,-,+   -,+     +   +,-     0 
    ## 11319     2    12 25757     7 68380

``` r
table(phoible$distributed)
```

    ## 
    ##       -   -,-,+     -,+       +     +,-   +,-,+       0   0,-,+   0,+,- 
    ##   22317       8     309   13164       8       2   69647      20       1 
    ## 0,0,-,+ 
    ##       1

``` r
table(phoible$strident)
```

    ## 
    ##       -   -,-,+     -,+   -,+,-       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   25359       9     397       2   11264       5   68420      20       1

``` r
table(phoible$dorsal)
```

    ## 
    ##       -   -,-,+     -,+   -,+,-       +     +,-   +,-,-   +,-,+   +,+,- 
    ##   47012       6     346       6   55501     359      10      54       2 
    ## +,+,-,-       0   0,-,+   0,+,- 0,0,-,+ 
    ##       1    2157      20       2       1

``` r
# Testing a weird looking feature vector -- the segment is the same at source
phoible %>% filter(dorsal=="0,0,-,+")
```

    ##   InventoryID Glottocode ISO6393   LanguageName SpecificDialect
    ## 1        2519   kham1282     khg Soghpo Tibetan            <NA>
    ##               GlyphID Phoneme Allophones Marginal SegmentClass Source tone
    ## 1 02B7+02B0+0074+0255    ʷʰtɕ       <NA>    FALSE    consonant     ea    0
    ##   stress syllabic short long consonantal sonorant continuant
    ## 1      -        -     -    -           +        -    0,0,-,+
    ##   delayedRelease approximant tap trill nasal lateral  labial round
    ## 1        0,0,-,+           -   -     -     -       - 0,+,-,-     +
    ##   labiodental coronal anterior distributed strident  dorsal high low front
    ## 1           -       +        +     0,0,-,+  0,0,-,+ 0,0,-,+    +   -     +
    ##   back tense retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1    -     0                   0                  0                     -
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -       +,0,-,-                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click
    ## 1                    -                      -     -

``` r
table(phoible$high)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,+ +,-,0 +,+,-     0 
    ## 19159     2   823     1     2 35616   627    38     1     6 49202

``` r
table(phoible$low)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +   +,- +,-,-     0 
    ## 49973     3   269    21  5599   412     1 49199

``` r
table(phoible$front)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 34242    12   544     2    10 21024   350    14     6     2 49271

``` r
table(phoible$back)
```

    ## 
    ##     - -,-,+   -,+ -,+,- -,+,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 39812     8   368     5     5 15551   482    19     1     1 49225

``` r
table(phoible$tense)
```

    ## 
    ##     -   -,+     +   +,- +,-,- +,-,+ +,+,-     0 
    ##  6390   179 23420   269     3     6     1 75209

``` r
table(phoible$retractedTongueRoot)
```

    ## 
    ##     -   -,+     +   +,-     0 
    ## 30956    11   286     2 74222

``` r
table(phoible$advancedTongueRoot)
```

    ## 
    ##     -     +     0 
    ## 31244    11 74222

``` r
table(phoible$periodicGlottalSource)
```

    ## 
    ##     -   -,+ -,+,-     +   +,- +,-,- +,-,+ +,+,-     0 
    ## 31249    80     7 71542   424    33     5     1  2136

``` r
table(phoible$epilaryngealSource)
```

    ## 
    ##      -      +      0 
    ## 103299     31   2147

``` r
table(phoible$spreadGlottis)
```

    ## 
    ##       -   -,-,+     -,+       +     +,-   +,-,-   +,-,+   +,0,- +,0,-,- 
    ##   96854      17     212    6115     130       3       1       5       1 
    ##       0   0,-,+ 
    ##    2135       4

``` r
table(phoible$constrictedGlottis)
```

    ## 
    ##     - -,-,+   -,+     +   +,- +,-,-     0 
    ## 99730    17    99  3358   126    12  2135

``` r
table(phoible$fortis)
```

    ## 
    ##     -     +     0 
    ## 71844   415 33218

``` r
table(phoible$raisedLarynxEjective)
```

    ## 
    ##      -  -,-,+    -,+      +    +,-  +,-,-      0 
    ## 101660      9     80   1558     21      2   2147

``` r
table(phoible$loweredLarynxImplosive)
```

    ## 
    ##      -    -,+      +    +,-      0 
    ## 102605      7    716      2   2147

``` r
table(phoible$click)
```

    ## 
    ##     - -,-,+   -,+ -,+,-     +     0 0,-,+ 
    ## 71948     6   231    62    10 33218     2

Compare features to phoible 2104 (WIP)
--------------------------------------

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

    ## Observations: 1,968
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
phoible %>% filter(grepl("˭", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
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

``` r
# unreleased (combining left angle above)
phoible %>% filter(grepl("̚", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ##   InventoryID LanguageName Phoneme
    ## 1        1411       Ngomba       q̚
    ## 2        1567      Tangale       d̪̚

Combining diacritics and where they occur
=========================================

``` r
# velarized/pharyngealized (combining tilde overlay)
phoible %>% filter(grepl("̴", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ## [1] Phoneme
    ## <0 rows> (or 0-length row.names)

``` r
phoible %>% filter(grepl("̴", Allophones)) %>% select(Allophones) %>% distinct()
```

    ##   Allophones
    ## 1        ɑ̴ a
    ## 2      ɑ̴ː aː
    ## 3    i̥ ə̴ i e
    ## 4      ɪː ə̴ː
    ## 5    u u̥ ɔ̴ o
    ## 6      ʊː ɔ̴ː

``` r
# nasalized (combining tilde)
phoible %>% filter(grepl("̃", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##     Phoneme
    ## 1        ãː
    ## 2        ũː
    ## 3         ã
    ## 4         ẽ̞
    ## 5        ẽ̞ː
    ## 6         ĩ
    ## 7        ĩː
    ## 8         õ̞
    ## 9        õ̞ː
    ## 10        ũ
    ## 11        ɛ̃
    ## 12        õ
    ## 13       ɑ̃˞
    ## 14       ə̃˞
    ## 15        æ̃
    ## 16       eĩ
    ## 17        ɪ̃
    ## 18       oũ
    ## 19        ʊ̃
    ## 20        ẵ
    ## 21        ɛ̆̃
    ## 22       iẽ
    ## 23        ŏ̃
    ## 24        ɔ̃
    ## 25        ŭ̃
    ## 26       ɛ̃ː
    ## 27       õː
    ## 28       æ̃ː
    ## 29       ə̃ː
    ## 30       ɔ̃ː
    ## 31        ə̃
    ## 32        ɯ̃
    ## 33       ɯ̃ː
    ## 34       ɪ̃ː
    ## 35       ʊ̃ː
    ## 36        ɤ̞̃
    ## 37        h̃
    ## 38        j̃
    ## 39        w̃
    ## 40        ɑ̃
    ## 41        ẽ
    ## 42        ã̟
    ## 43       ã̟ː
    ## 44        ɨ̃
    ## 45       z̞̩̃ˠ
    ## 46        ʌ̃
    ## 47       ʌ̃ː
    ## 48       ɑ̃ː
    ## 49       ẽː
    ## 50        ɸ̃
    ## 51        ɒ̃
    ## 52        œ̃
    ## 53        ɐ̃
    ## 54        ɤ̃
    ## 55       ɨ̃ː
    ## 56       æẽ̯
    ## 57        ɜ̃
    ## 58       ɔõ̯
    ## 59        ɽ̃
    ## 60       ɑ̃ɔ̃
    ## 61       ɛ̃ɔ̃
    ## 62        ɘ̃
    ## 63        ỹ
    ## 64        l̃
    ## 65       ãĩ
    ## 66       ɨ̃ĩ
    ## 67       ɔ̃ĩ
    ## 68       ɘ̃ː
    ## 69       ã̟ĩ
    ## 70       õ̞ĩ
    ## 71       ũĩ
    ## 72      ãẽ̞ˤ
    ## 73      ãõ̞ˤ
    ## 74       ãˤ
    ## 75      ãˤː
    ## 76       ẽ̞ĩ
    ## 77       ẽ̞ũ
    ## 78       õ̞ã
    ## 79      õ̞ãˤ
    ## 80      õ̞ĩˤ
    ## 81       õ̞ˤ
    ## 82      ɔ̃ˤː
    ## 83       ĩẽ̞
    ## 84       õ̞ũ
    ## 85        ã̰
    ## 86        ẽ̞̰
    ## 87        ḭ̃
    ## 88        õ̞̰
    ## 89        ɨ̞̃
    ## 90       ĩˠ
    ## 91        ɵ̃
    ## 92        ɤ̟̞̃
    ## 93        ʉ̃
    ## 94       iã
    ## 95       ɪã
    ## 96       uẽ
    ## 97        r̃
    ## 98        ṵ̃
    ## 99       ãi
    ## 100      ɛ̃ə
    ## 101      ĩə
    ## 102      ũə
    ## 103      ɯ̃ə
    ## 104       ə̰̃
    ## 105       ɛ̰̃
    ## 106       õ̰
    ## 107      ɑ̃ĩ
    ## 108      ɑ̃õ
    ## 109      ẽĩ
    ## 110      õũ
    ## 111       ṽ
    ## 112       ɥ̃
    ## 113      ɤ̃ː
    ## 114       ɰ̃
    ## 115      õˤ
    ## 116      ũˤ
    ## 117      ɒ̃ː
    ## 118       β̃
    ## 119     i̯ãː
    ## 120      uã
    ## 121     uãː
    ## 122       æ̞̃
    ## 123      ãʲ
    ## 124      õʲ
    ## 125       ḛ̃
    ## 126       ɾ̃
    ## 127      aɪ̃
    ## 128      aʊ̃
    ## 129      eɪ̃
    ## 130      oʊ̃
    ## 131      œ̃ː
    ## 132       ə̠̃
    ## 133      ɐ̃i
    ## 134      ɐ̃u̜
    ## 135      õi
    ## 136      ũi
    ## 137      ɐ̃ɪ̯̃
    ## 138      ɐ̃ʊ̯̃
    ## 139      ẽɪ̯̃
    ## 140      õɪ̯̃
    ## 141      ũɪ̯̃
    ## 142      ʊ̯ɐ̃
    ## 143     iẽː
    ## 144      u̯õ
    ## 145     u̯õː
    ## 146      i̯ẽ
    ## 147      ãi̯
    ## 148      ãõ
    ## 149      ãu̯
    ## 150      ẽi̯
    ## 151      õi̯
    ## 152       ø̃
    ## 153      ø̃ː
    ## 154      ỹː
    ## 155      ɔ̜̃ː
    ## 156      əĩ
    ## 157      əũ
    ## 158      æ̃ɪ̯̃
    ## 159     ĩɛ̃ː
    ## 160      ɔ̃ʊ̯̃
    ## 161     ũɔ̃ː
    ## 162      uɑ̃
    ## 163      ãũ
    ## 164       ã̈
    ## 165       ĩ̯
    ## 166       ĩ̞
    ## 167      ã̈ː
    ## 168      ã̤̈ː
    ## 169      ə̤̃ː
    ## 170      ɛ̤̃ː
    ## 171      ĩ̤ː
    ## 172      ɔ̤̃ː
    ## 173      ṳ̃ː
    ## 174      ə̃ĩ
    ## 175      ə̃ũ
    ## 176       ʔ̃
    ## 177     ãɪ̯ː
    ## 178     i̯ũo
    ## 179      ɪ̃ɛ
    ## 180     ɪ̃ɛː
    ## 181       ɔ̞̃
    ## 182      ɔ̞̃ː
    ## 183      ũo
    ## 184     ũoː
    ## 185      ỹœ
    ## 186     ỹœː
    ## 187       ʏ̃
    ## 188      ʏ̃ː
    ## 189      aɯ̃
    ## 190      əɯ̃
    ## 191      iæ̃
    ## 192      iɛ̃
    ## 193      uɛ̃
    ## 194      yĩ
    ## 195      ãɛ̃
    ## 196     ɛ̃ːi̯
    ## 197      ɞ̜̃ː
    ## 198      ħ̃ʲ
    ## 199      ɨ̞̃ː
    ## 200      œ̃ɛ̃
    ## 201      ɔ̟̜̃ː
    ## 202      ʉ̃ː
    ## 203      ĩẽ
    ## 204      õã
    ## 205      i̯ɛ̃
    ## 206      u̯ã
    ## 207      u̯ə̃
    ## 208      u̯ɛ̃
    ## 209      u̯ɔ̃
    ## 210      ĩã
    ## 211      ĩõ
    ## 212      ãũ̯
    ## 213      ẽɪ̃
    ## 214      ɛ̃ũ
    ## 215      ɪ̃ə̃
    ## 216      ũã
    ## 217       ũ̯
    ## 218       ã̤

``` r
# denasalized (combining not tilde above)
phoible %>% filter(grepl("͊", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ## [1] Phoneme
    ## <0 rows> (or 0-length row.names)

``` r
phoible %>% filter(grepl("͊", Allophones)) %>% select(Allophones) %>% distinct()
```

    ## [1] Allophones
    ## <0 rows> (or 0-length row.names)

``` r
# nasal emission (combining homothetic)
phoible %>% filter(grepl("͋", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ## [1] Phoneme
    ## <0 rows> (or 0-length row.names)

``` r
phoible %>% filter(grepl("͋", Allophones)) %>% select(Allophones) %>% distinct()
```

    ## [1] Allophones
    ## <0 rows> (or 0-length row.names)

``` r
# derhoticized (combining breve below)
phoible %>% filter(grepl("̮", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ## [1] Phoneme
    ## <0 rows> (or 0-length row.names)

``` r
phoible %>% filter(grepl("̮", Allophones)) %>% select(Allophones) %>% distinct()
```

    ## [1] Allophones
    ## <0 rows> (or 0-length row.names)

``` r
# breathy (combining diaresis below)
phoible %>% filter(grepl("̤", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##       Phoneme
    ## 1           b̤
    ## 2           d̪̤
    ## 3          d̠ʒ̤
    ## 4           ɖ̤
    ## 5           ɡ̤
    ## 6           d̤
    ## 7          ˨˩̤
    ## 8           b̤̥
    ## 9           d̤̥
    ## 10          ɡ̤̥
    ## 11         tz̤̥
    ## 12         ˧˨̤
    ## 13          d̪̤̥
    ## 14          l̤
    ## 15         b̤ʲ
    ## 16         ɡ̤ʷ
    ## 17         b̤ː
    ## 18         d̪̤ː
    ## 19        d̠ʒ̤ː
    ## 20         ɖ̤ː
    ## 21         ɡ̤ː
    ## 22          m̤
    ## 23          n̪̤
    ## 24          ɔ̤
    ## 25          n̤
    ## 26          ɽ̤
    ## 27          ɾ̤
    ## 28          a̤
    ## 29          ɒ̤
    ## 30          e̤
    ## 31          ɛ̤
    ## 32          ɤ̤
    ## 33          i̤
    ## 34          o̤
    ## 35          ṳ
    ## 36          ɯ̤
    ## 37          ʌ̤
    ## 38         i̤ə̤
    ## 39         ṳə̤
    ## 40         ɯ̤ə̤
    ## 41        l̪̤|l̤
    ## 42        d̪̤|d̤
    ## 43          v̤
    ## 44         dz̤
    ## 45   ɡ̤ǀ\u0353
    ## 46         ɡ̤ǂ
    ## 47  ɡ̤ǂ\u0353ˡ
    ## 48         ɡ̤ǃ
    ## 49   ŋ̤ǀ\u0353
    ## 50         ŋ̤ǂ
    ## 51  ŋ̤ǂ\u0353ˡ
    ## 52         ŋ̤ǃ
    ## 53      d̪z̪̤|dz̤
    ## 54        n̪̤|n̤
    ## 55         a̤i̤
    ## 56         a̤ṳ
    ## 57         a̤ɯ̤
    ## 58         ɤ̤i̤
    ## 59         i̤a̤
    ## 60         i̤ɛ̤
    ## 61         i̤ṳ
    ## 62          l̠̤
    ## 63          n̠̤
    ## 64          ŋ̤
    ## 65         o̤i̤
    ## 66         ɔ̤i̤
    ## 67         ṳa̤
    ## 68         ṳi̤
    ## 69         ɯ̤i̤
    ## 70          ʒ̤
    ## 71         a̤ː
    ## 72          æ̤
    ## 73         æ̤ː
    ## 74         ɛ̤ː
    ## 75         i̤ː
    ## 76         o̤ː
    ## 77         ɔ̤ː
    ## 78          ɵ̤
    ## 79         ɵ̤ː
    ## 80         ṳː
    ## 81          ʊ̤
    ## 82         ʊ̤ː
    ## 83          ɑ̤
    ## 84         ɑ̤ː
    ## 85         e̤ː
    ## 86         ɤ̤ː
    ## 87         i̤a
    ## 88         ṳa
    ## 89         ɯ̤ː
    ## 90         ɯ̤a
    ## 91         ʌ̤ː
    ## 92         ˧˩̤
    ## 93         d̤z̤
    ## 94          w̤
    ## 95         a̤ˑ
    ## 96         e̤ˑ
    ## 97         ɛ̤ˑ
    ## 98         i̤ˑ
    ## 99         o̤ˑ
    ## 100        ɔ̤ˑ
    ## 101        ṳˑ
    ## 102        b̤v̤
    ## 103        b̤z̤
    ## 104        d̤ɮ̤
    ## 105        d̤ʒ̤
    ## 106         ɦ̤
    ## 107        m̤b̤
    ## 108        n̤d̤
    ## 109       n̤d̤ɮ̤
    ## 110        n̤ɡ̤
    ## 111       ŋ̤ɡǃ
    ## 112         r̤
    ## 113        d̠̤ʒ̤
    ## 114        ɡ̤ǀ
    ## 115        ɡ̤ǁ
    ## 116         ɣ̤
    ## 117         ɟ̤
    ## 118         ɮ̤
    ## 119       m̤b̤v̤
    ## 120       n̤d̤z̤
    ## 121       n̠̤d̠̤ʒ
    ## 122         ɲ̤
    ## 123        ɲ̤ɟ̤
    ## 124        ŋ̤ɡ̤
    ## 125        ŋ̤ǀ
    ## 126        ŋ̤ǁ
    ## 127         z̤
    ## 128         ɪ̤
    ## 129         œ̤
    ## 130         y̤
    ## 131        n̤ʷ
    ## 132        m̤ʱ
    ## 133       m̤pʰ
    ## 134        ɱ̤f
    ## 135        n̤ʱ
    ## 136        n̤s
    ## 137       n̤tʰ
    ## 138       ɲ̤cʰ
    ## 139        ɲ̤ʱ
    ## 140        ŋ̤ʱ
    ## 141       ŋ̤kʰ
    ## 142         ə̤
    ## 143         j̤
    ## 144        ɟ̤ʝ
    ## 145         ɭ̤
    ## 146         c̤
    ## 147        d̤z
    ## 148         ɳ̤
    ## 149       d̤ʒ̤ː
    ## 150       d̤z̤ː
    ## 151         ä̤
    ## 152        ã̤̈ː
    ## 153        b̤ʱ
    ## 154        d̪̤ʱ
    ## 155       d̤ʒ̤ʱ
    ## 156        ɖ̤ʱ
    ## 157        ə̤̃ː
    ## 158        ɛ̤̃ː
    ## 159        ɡ̤ʱ
    ## 160        ĩ̤ː
    ## 161        ɔ̤̃ː
    ## 162        ṳ̃ː
    ## 163        d̻̤ʱ
    ## 164        ɽ̤ʱ
    ## 165        ɛ̤i̤
    ## 166         ɨ̤
    ## 167         ø̤
    ## 168         ã̤

``` r
# creaky (combining tilde below)
phoible %>% filter(grepl("̰", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##        Phoneme
    ## 1            a̰
    ## 2            ḛ
    ## 3            ɛ̰
    ## 4            ḭ
    ## 5            o̰
    ## 6            ɔ̰
    ## 7            ṵ
    ## 8           ˦˥̰
    ## 9            ˩̰
    ## 10          ˧˩̰
    ## 11           ˥̰
    ## 12           j̰
    ## 13           l̻̰
    ## 14           m̰
    ## 15           n̰
    ## 16           w̰
    ## 17           b̰
    ## 18           d̰
    ## 19           l̰
    ## 20         l̪̰|l̰
    ## 21         n̪̰|n̰
    ## 22           ŋ̰
    ## 23           ɾ̰
    ## 24      \u1d05̪̰
    ## 25           n̪̰
    ## 26   ɡ̰ǀ\u0353x
    ## 27  ɡ̰ǂ\u0353ˡx
    ## 28         ɡ̰ǂx
    ## 29         ɡ̰ǃx
    ## 30           d̪̰
    ## 31           ɲ̰
    ## 32           ã̰
    ## 33           e̞̰
    ## 34           ẽ̞̰
    ## 35           ḭ̃
    ## 36           o̞̰
    ## 37           õ̞̰
    ## 38           ɺ̠̰
    ## 39           ɟ̰
    ## 40         d̪̰|d̰
    ## 41           d̠̰
    ## 42         r̪̰|r̰
    ## 43          ʁ̞̰ʷ
    ## 44           ɰ̰
    ## 45           ɖ̰
    ## 46         R̪̰|R̰
    ## 47           ɣ̰
    ## 48           ʐ̰
    ## 49           k̰
    ## 50           q̰
    ## 51           p̰
    ## 52           t̰
    ## 53          ts̰
    ## 54          t̠ʃ̰
    ## 55          ɹ̰ˤ
    ## 56          k̰ʷ
    ## 57          q̰ʷ
    ## 58           s̰
    ## 59           c̰
    ## 60           r̰
    ## 61           ʕ̰
    ## 62          ʕ̰ʷ
    ## 63           ɡ̰
    ## 64           ɟ̰̩
    ## 65           f̰
    ## 66           θ̰
    ## 67          t̰s̰
    ## 68           ṵ̃
    ## 69          ˀj̰
    ## 70          ˀw̰
    ## 71           ɯ̰
    ## 72          tɬ̰
    ## 73           ə̰
    ## 74           ə̰̃
    ## 75           ɛ̰̃
    ## 76           õ̰
    ## 77          ˥˧̰
    ## 78          a̰ḭ
    ## 79          a̰ṵ
    ## 80          a̰ː
    ## 81          a̰ˑ
    ## 82          ḛː
    ## 83          ḛˑ
    ## 84          ɛ̰ː
    ## 85          ɛ̰ˑ
    ## 86          ḭː
    ## 87          ḭˑ
    ## 88          o̰ː
    ## 89          o̰ˑ
    ## 90          ɔ̰ː
    ## 91          ɔ̰ˑ
    ## 92           t̪̰
    ## 93           ɨ̰
    ## 94           ḛ̃
    ## 95           ˦̰
    ## 96          ḭu
    ## 97           æ̰
    ## 98           ɞ̰
    ## 99          a̰ɪ̰
    ## 100         a̰ɯ̰
    ## 101         ḛɪ̰
    ## 102         ə̰ɯ̰
    ## 103          ɪ̰
    ## 104          ʊ̰
    ## 105          ɞ̜̰
    ## 106          ɨ̞̰
    ## 107          ɔ̟̜̰
    ## 108          ʉ̰
    ## 109          z̰̩
    ## 110          ɵ̞̰
    ## 111         ɑ̰ː
    ## 112         œ̰ː
    ## 113         ṵː
    ## 114         ɯ̰̽ː
    ## 115         y̰ː
    ## 116          ø̰
    ## 117          y̰

``` r
# stiff (combining caron below)
phoible %>% filter(grepl("̬", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##   Phoneme
    ## 1       ʔ̬

``` r
phoible %>% filter(grepl("̬", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ##   InventoryID LanguageName Phoneme
    ## 1         638       NENETS       ʔ̬

``` r
# frictionalized (combining x below)
phoible %>% filter(grepl("͓", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##            Phoneme
    ## 1        kʟ\u0353̥ʼ
    ## 2          ɯ\u0353
    ## 3         ɬʟ\u0353̥
    ## 4         kǀ\u0353
    ## 5         ŋǀ\u0353
    ## 6          i\u0353
    ## 7          u\u0353
    ## 8  r̪\u0353|r\u0353
    ## 9        kǀ\u0353ʰ
    ## 10        kǁ\u0353
    ## 11       kǁ\u0353ʰ
    ## 12       ŋ̥ǀ\u0353ˀ
    ## 13        ŋǁ\u0353
    ## 14       ŋ̥ǁ\u0353ˀ
    ## 15        ɡǀ\u0353
    ## 16        ɡ̤ǀ\u0353
    ## 17       ɡǀ\u0353x
    ## 18       ɡ̰ǀ\u0353x
    ## 19       ɡǂ\u0353ˡ
    ## 20       ɡ̤ǂ\u0353ˡ
    ## 21      ɡǂ\u0353ˡx
    ## 22      ɡ̰ǂ\u0353ˡx
    ## 23       kǀ\u0353x
    ## 24       kǂ\u0353ˡ
    ## 25      kǂ\u0353ˡʰ
    ## 26      kǂ\u0353ˡx
    ## 27        ŋ̤ǀ\u0353
    ## 28       ŋ̥ǀ\u0353ʰ
    ## 29      ŋ̥ǀ\u0353xˀ
    ## 30       ŋǂ\u0353ˡ
    ## 31       ŋ̤ǂ\u0353ˡ
    ## 32      ŋ̥ǂ\u0353ˡʰ
    ## 33     ŋ̥ǂ\u0353ˡxˀ
    ## 34      ŋ̥ǂ\u0353ˡˀ
    ## 35      kǀ\u0353ˠʰ
    ## 36      kǁ\u0353xʰ
    ## 37       ŋ̥ǁ\u0353ʰ
    ## 38         ɭ\u0353
    ## 39        ɖr̠\u0353
    ## 40        ɡǁ\u0353
    ## 41       kǀ\u0353ˀ
    ## 42       kǁ\u0353ˀ
    ## 43         ɾ\u0353
    ## 44         ʟ\u0353̥
    ## 45        kǃ\u0353

``` r
# linguolabial (combining seagull below)
phoible %>% filter(grepl("̼", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##   Phoneme
    ## 1       m̼
    ## 2       p̼
    ## 3       v̼

``` r
# dental (combining bridge below)
phoible %>% filter(grepl("̪", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##             Phoneme
    ## 1                 d̪
    ## 2                 d̪̤
    ## 3                d̪ː
    ## 4                 n̪
    ## 5                 s̪
    ## 6                s̪ː
    ## 7                 t̪
    ## 8                t̪ː
    ## 9                t̪ʰ
    ## 10                l̪
    ## 11                d̪̤̥
    ## 12               t̪θ
    ## 13              t̪θʰ
    ## 14              t̪θʼ
    ## 15               t̪ʼ
    ## 16               l̪ː
    ## 17                z̪
    ## 18               z̪ː
    ## 19               ˀd̪
    ## 20               d̪ð
    ## 21               l̪ˠ
    ## 22                l̪̩
    ## 23               d̪̤ː
    ## 24                n̪̤
    ## 25               n̪ː
    ## 26              t̪ʰː
    ## 27              n̪|n
    ## 28              R̪|R
    ## 29              d̪|d
    ## 30            d̪z̪|dz
    ## 31              r̪|r
    ## 32              s̪|s
    ## 33            t̪ʰ|tʰ
    ## 34          t̪s̪ʰ|tsʰ
    ## 35               d̪ⁿ
    ## 36              t̪|t
    ## 37              z̪|z
    ## 38              l̪|l
    ## 39              ɬ̪|ɬ
    ## 40              n̪̥|n̥
    ## 41            t̪s̪|ts
    ## 42              ɾ̪|ɾ
    ## 43            s̪ʷ|sʷ
    ## 44            t̪ʼ|tʼ
    ## 45                r̪
    ## 46              ɹ̪|ɹ
    ## 47            d̪ʷ|dʷ
    ## 48            ɬ̪ː|ɬː
    ## 49            ɬ̪ʷ|ɬʷ
    ## 50          ɬ̪ʷː|ɬʷː
    ## 51              ɮ̪|ɮ
    ## 52            s̪ː|sː
    ## 53          s̪ʷː|sʷː
    ## 54            t̪ː|tː
    ## 55          t̪ɬ̪ʰ|tɬʰ
    ## 56        t̪ɬ̪ʷʰ|tɬʷʰ
    ## 57        t̪ɬ̪ʷʼ|tɬʷʼ
    ## 58          t̪ɬ̪ʼ|tɬʼ
    ## 59        t̪s̪ʷʰ|tsʷʰ
    ## 60        t̪s̪ʷʼ|tsʷʼ
    ## 61          t̪s̪ʼ|tsʼ
    ## 62        t̪s̪ʼː|tsʼː
    ## 63          t̪ʷʰ|tʷʰ
    ## 64            z̪ʷ|zʷ
    ## 65               d̪ˤ
    ## 66               s̪ˤ
    ## 67               t̪ˤ
    ## 68               z̪ˤ
    ## 69          t̪ɬ̪ː|tɬː
    ## 70        t̪ɬ̪ʼː|tɬʼː
    ## 71          t̪s̪ː|tsː
    ## 72               d̪z̪
    ## 73              t̪s̪ʰ
    ## 74              t̪s̪ʼ
    ## 75            n̪s̪|ns
    ## 76            n̪t̪|nt
    ## 77               d̪ʲ
    ## 78               l̪ʲ
    ## 79               s̪ʲ
    ## 80               t̪ʲ
    ## 81               t̪s̪
    ## 82              t̪s̪ʲ
    ## 83               z̪ʲ
    ## 84          t̪s̪ˀ|tsˀ
    ## 85            t̪ˀ|tˀ
    ## 86              l̪̥|l̥
    ## 87              R̪̥|R̥
    ## 88          t̪ˠʰ|tˠʰ
    ## 89            t̪ɬ̪|tɬ
    ## 90              z̪͇|z͇
    ## 91            s̪ʲ|sʲ
    ## 92            l̪ʲ|lʲ
    ## 93            ɬ̪ʲ|ɬʲ
    ## 94            n̪d̪|nd
    ## 95          n̪d̪ʲ|ndʲ
    ## 96            t̪ʲ|tʲ
    ## 97          t̪ʲʰ|tʲʰ
    ## 98                n̪̥
    ## 99               n̪t̪
    ## 100             n̪t̪ʰ
    ## 101             n̪t̪s̪
    ## 102            n̪t̪s̪ʰ
    ## 103             ɺ̪|ɺ
    ## 104             ɗ̪|ɗ
    ## 105           d̪ɮ̪|dɮ
    ## 106              n̪d̪
    ## 107               ɮ̪
    ## 108             ɹ̪̥|ɹ̥
    ## 109         n̪t̪s̪|nts
    ## 110 r̪\u0353|r\u0353
    ## 111             l̪̰|l̰
    ## 112             n̪̰|n̰
    ## 113             l̪̤|l̤
    ## 114             d̪̤|d̤
    ## 115           ɾ̪ʲ|ɾʲ
    ## 116               ɗ̪
    ## 117           t̪ʷ|tʷ
    ## 118           n̪ʲ|nʲ
    ## 119              t̪ɦ
    ## 120              ɬ̪ː
    ## 121           l̪ˠ|lˠ
    ## 122              ɬ̪ʲ
    ## 123             ɬ̪ʲʼ
    ## 124              ɮ̪ʲ
    ## 125           d̪ʲ|dʲ
    ## 126              d̪l̪
    ## 127          \u1d05̪
    ## 128          \u1d05̪̰
    ## 129               n̪̰
    ## 130              s̪ʼ
    ## 131              s̪ˀ
    ## 132              t̪ˀ
    ## 133           s̪ʰ|sʰ
    ## 134       t̪s̪ʷː|tsʷː
    ## 135               d̪̰
    ## 136         t̪s̪ʲ|tsʲ
    ## 137           s̪ʼ|sʼ
    ## 138               b̪
    ## 139             n̪d̪z̪
    ## 140           d̪z̪̤|dz̤
    ## 141             n̪̤|n̤
    ## 142               R̪
    ## 143             d̪̰|d̰
    ## 144           n̪ʷ|nʷ
    ## 145           ʰs̪|ʰs
    ## 146               ɾ̪
    ## 147              n̪ʲ
    ## 148              r̪ʲ
    ## 149             r̪̥|r̥
    ## 150             r̪̰|r̰
    ## 151           z̪ʲ|zʲ
    ## 152           d̪ˤ|dˤ
    ## 153           l̪ˤ|lˤ
    ## 154           r̪ˤ|rˤ
    ## 155           s̪ˤ|sˤ
    ## 156           t̪ˤ|tˤ
    ## 157           z̪ˤ|zˤ
    ## 158         t̪ɬ̪ˀ|tɬˀ
    ## 159         d̪z̪ʲ|dzʲ
    ## 160           r̪ʲ|rʲ
    ## 161             R̪̰|R̰
    ## 162         n̪d̪ʷ|ndʷ
    ## 163               l̪̥
    ## 164           ɬ̪ʼ|ɬʼ
    ## 165               ɬ̪
    ## 166           n̪ː|nː
    ## 167           n̪z̪|nz
    ## 168           t̪ɦ|tɦ
    ## 169         t̪s̪ɦ|tsɦ
    ## 170           r̪ˠ|rˠ
    ## 171              n̪z̪
    ## 172              kǀ̪
    ## 173              kǃ̪
    ## 174              d̪ʱ
    ## 175              ts̪
    ## 176              ʱn̪
    ## 177              ʱɾ̪
    ## 178              ɾ̪ˠ
    ## 179              t̪s
    ## 180              ˀl̪̥
    ## 181              ˀn̪̥
    ## 182              ˀt̪
    ## 183             ˀt̪ɬ
    ## 184             ˀt̪s
    ## 185              d̪ɦ
    ## 186              dz̪
    ## 187              t̪n̪
    ## 188              nd̪
    ## 189              t̪ʷ
    ## 190              t̪ɬ̪
    ## 191             t̪ɬ̪ʰ
    ## 192             t̪ɬ̪ʼ
    ## 193              l̪ʷ
    ## 194               m̪
    ## 195               p̪
    ## 196              d̪x
    ## 197           d̪ʼkxʼ
    ## 198              t̪x
    ## 199           t̪ʼkxʼ
    ## 200           t̪ʃ|t̠ʃ
    ## 201              r̪ː
    ## 202             t̪ʼː
    ## 203               d̪̚
    ## 204               t̪̰
    ## 205               ɾ̪̊
    ## 206              ɾ̪̊ʰ
    ## 207              t̪ʙ
    ## 208               ð̪
    ## 209              d̪ʒ
    ## 210              ð̪̙ˤ
    ## 211              t̪̙ˤ
    ## 212               θ̪
    ## 213              d̪n̪
    ## 214               s̪̻
    ## 215              t̪̻s̪̻
    ## 216             d̪ʱː
    ## 217              d̪ð̪
    ## 218              t̪θ̪
    ## 219              d̪ˠ
    ## 220              n̪ˠ
    ## 221             t̪ˠʰ
    ## 222             d̪z̪ː
    ## 223             t̪s̪ː
    ## 224             d̪ˤː
    ## 225              l̪ˤ
    ## 226             l̪ˤː
    ## 227              n̪ˤ
    ## 228              r̪ˤ
    ## 229             r̪ˤː
    ## 230             s̪ˤː
    ## 231             t̪ˤː
    ## 232             z̪ˤː
    ## 233               ð̪̺
    ## 234               t̪̺
    ## 235              n̪ʱ
    ## 236               ɫ̪
    ## 237             t̪ˤʰ
    ## 238             t̪ˤʼ
    ## 239              d̪z
    ## 240              t̪ˠ
    ## 241              d̪ʰ
    ## 242               d̪̺
    ## 243               n̪̩
    ## 244              ⁿd̪
    ## 245              t̪ɬ
    ## 246             t̪ɬʼ
    ## 247              t̻s̪̻
    ## 248               ɬ̪̺
    ## 249               n̪̺
    ## 250              l̪ʱ
    ## 251              d̪̤ʱ
    ## 252               d̪̥
    ## 253               d̪̻
    ## 254               t̪̻
    ## 255               z̪̻
    ## 256             d̪z̪ʲ
    ## 257              ɾ̪ʲ
    ## 258             t̪ʲʰ
    ## 259              s̪ʷ
    ## 260             s̪ʷʲ
    ## 261             t̪ʲʼ
    ## 262            t̪s̪ʲʰ
    ## 263            t̪s̪ʲʼ
    ## 264             t̪s̪ʷ
    ## 265            t̪s̪ʷʰ
    ## 266            t̪s̪ʷʲ
    ## 267           t̪s̪ʷʲʰ
    ## 268           t̪s̪ʷʲʼ
    ## 269            t̪s̪ʷʼ
    ## 270             t̪ʷʰ
    ## 271             t̪ʷʲ
    ## 272            t̪ʷʲʰ
    ## 273            t̪ʷʲʼ
    ## 274             t̪ʷʼ
    ## 275              z̪ʷ
    ## 276             ⁿd̪z̪
    ## 277            ⁿt̪s̪ʰ
    ## 278               l̪̊
    ## 279               n̪̊
    ## 280               j̪
    ## 281               t̪͈
    ## 282               t̪͉
    ## 283              n̪ʷ

``` r
# apical (combining inverted bridge below)
phoible %>% filter(grepl("̺", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        n̺
    ## 2        r̺
    ## 3        s̺
    ## 4       t̺s̺
    ## 5        ʃ̺
    ## 6      t̠ʃ̺ʰ
    ## 7        t̺
    ## 8        d̺
    ## 9       d̺ː
    ## 10      t̺ː
    ## 11       z̺
    ## 12       ɭ̺
    ## 13       ɳ̺
    ## 14       ɻ̺
    ## 15       ʈ̺
    ## 16       ð̪̺
    ## 17       l̺
    ## 18       t̪̺
    ## 19      d̺ʱ
    ## 20      t̺ʰ
    ## 21       r̺̥
    ## 22       ɾ̺
    ## 23       d̺̥
    ## 24      t̺ʼ
    ## 25       d̪̺
    ## 26       q̺
    ## 27      t̺ɕ̺
    ## 28     t̺ɕ̺ʼ
    ## 29       s̺̠
    ## 30      r̺ʲ
    ## 31      d̺̥z̺̥
    ## 32     t̺s̺ʰ
    ## 33     t̺s̺ʼ
    ## 34      ts̺
    ## 35     t̺s̺ː
    ## 36       ɬ̪̺
    ## 37       n̪̺
    ## 38       ɮ̺
    ## 39      dz̺
    ## 40       ɫ̺
    ## 41      d̺ʒ̺
    ## 42      t̺ʃ̺
    ## 43       ʒ̺
    ## 44       ɹ̺
    ## 45      t̠ʃ̺
    ## 46      d̺z̺
    ## 47     d̺z̺ː
    ## 48      s̺ː
    ## 49     d̺ʒ̺ʷ
    ## 50     t̺ʃ̺ʷ
    ## 51       ð̺̞
    ## 52       t̺͈
    ## 53       t̺͉
    ## 54       ɺ̺̠
    ## 55       r̺͈

``` r
# laminal (combining square below)
phoible %>% filter(grepl("̻", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        d̻
    ## 2       d̻z̻
    ## 3        n̻
    ## 4        s̻
    ## 5       t̻ʰ
    ## 6       t̻s̻
    ## 7      t̻s̻ʼ
    ## 8       t̻ʼ
    ## 9        z̻
    ## 10       l̻
    ## 11       t̻
    ## 12      s̻ː
    ## 13     t̻s̻ʰ
    ## 14       l̻̰
    ## 15      s̻θ
    ## 16       ɬ̻
    ## 17      d̻ʲ
    ## 18      n̻ʲ
    ## 19      t̻ʲ
    ## 20      d̻ː
    ## 21      l̻ː
    ## 22      n̻ː
    ## 23      t̻ː
    ## 24     t̻ʰː
    ## 25       b̻
    ## 26       k̻
    ## 27       p̻
    ## 28       c̻
    ## 29       j̻
    ## 30       ʎ̻
    ## 31       ɲ̻
    ## 32       s̪̻
    ## 33      t̪̻s̪̻
    ## 34     d̻z̻ː
    ## 35     t̻s̻ː
    ## 36      z̻ː
    ## 37       c̻̟
    ## 38       ʎ̻̟
    ## 39       ɲ̻̟
    ## 40      ts̻
    ## 41      t̻s̪̻
    ## 42       s̻̠
    ## 43       ɫ̻
    ## 44       ɾ̻
    ## 45      d̻ʑ̻
    ## 46     d̻ʑ̻ʱ
    ## 47      t̻ɕ̻
    ## 48     t̻ɕ̻ʰ
    ## 49      d̻̤ʱ
    ## 50       d̪̻
    ## 51      d̻ʒ̻
    ## 52       t̪̻
    ## 53      t̻ʃ̻
    ## 54       z̪̻
    ## 55       θ̻
    ## 56      dz̻
    ## 57      ɖ̻ʐ̻
    ## 58       ʂ̻
    ## 59      ʈ̻ʂ̻
    ## 60       ʐ̻

``` r
# non-sibilant (combining equals sign below)
phoible %>% filter(grepl("͇", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        ʐ͇
    ## 2        ʂ͇
    ## 3        z͇
    ## 4       ʈʂ͇
    ## 5      z̪͇|z͇
    ## 6        ʃ͇
    ## 7        ʒ͇
    ## 8       ts͇
    ## 9      ts͇ʰ
    ## 10      d͇z͇
    ## 11     ⁿd͇z͇
    ## 12    ⁿt͇s͇ʰ
    ## 13       s͇
    ## 14      t͇s͇
    ## 15     t͇s͇ʰ
    ## 16      ð͇ˠ

``` r
# fortis (combining double vertical line below)
phoible %>% filter(grepl("͈", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        k͈
    ## 2       k͈ʷ
    ## 3        p͈
    ## 4        q͈
    ## 5       q͈ʷ
    ## 6        s͈
    ## 7        ʃ͈
    ## 8       ʃ͈ʷ
    ## 9        t͈
    ## 10      t͈s
    ## 11     t͈sʷ
    ## 12      t̠͈ʃ
    ## 13     t̠͈ʃʷ
    ## 14       x͈
    ## 15      x͈ʷ
    ## 16       χ͈
    ## 17      χ͈ʷ
    ## 18       b͈
    ## 19       d͈
    ## 20      d̠͈ʒ
    ## 21      k͈ː
    ## 22      p͈ː
    ## 23      s͈ː
    ## 24      ʃ͈ː
    ## 25      t͈ː
    ## 26     t̠͈ʃː
    ## 27      b͈ː
    ## 28      d͈ː
    ## 29      f͈ː
    ## 30      ɡ͈ː
    ## 31      ɣ͈ː
    ## 32      ħ͈ː
    ## 33      l͈ː
    ## 34      m͈ː
    ## 35      n͈ː
    ## 36      r͈ː
    ## 37      x͈ː
    ## 38      z͈ː
    ## 39      ʒ͈ː
    ## 40       ʈ͈
    ## 41  \u0236͈
    ## 42       t̪͈
    ## 43       t̺͈
    ## 44       r̺͈

``` r
# lenis (combining left angle below)
phoible %>% filter(grepl("͉", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        k͉
    ## 2        p͉
    ## 3        t͉
    ## 4       t͉s
    ## 5       t̠ʃ͉
    ## 6        ʈ͉
    ## 7        f͉
    ## 8        s͉
    ## 9        ʃ͉
    ## 10       ð͉
    ## 11      p͉ʲ
    ## 12      t͉ʲ
    ## 13      k͉ʷ
    ## 14       l͉
    ## 15       m͉
    ## 16       n͉
    ## 17       ʂ͉
    ## 18       b͉
    ## 19      ˀb͉
    ## 20       x͉
    ## 21      t͉ɕ͉
    ## 22      t̠͉ʃ
    ## 23  \u0236͉
    ## 24       t̪͉
    ## 25       t̺͉

``` r
# retracted tongue root (combining right tack below)
phoible %>% filter(grepl("̙", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1       a̟̙ː
    ## 2       ei̙
    ## 3       əɨ̙
    ## 4       ou̙
    ## 5       i̙ː
    ## 6        ɪ̙
    ## 7        ʊ̙
    ## 8       ʊ̙ː
    ## 9        r̠̙
    ## 10       a̙
    ## 11      a̙ː
    ## 12      a̙ˠ
    ## 13       ɛ̙
    ## 14      ɛ̙ː
    ## 15      ɛ̙ˠ
    ## 16      ɪ̙ː
    ## 17      ɪ̙ˠ
    ## 18       ɔ̙
    ## 19      ɔ̙ː
    ## 20       e̙
    ## 21       i̙
    ## 22       o̙
    ## 23       u̙
    ## 24       k̙
    ## 25       ɡ̙
    ## 26      d̙ˤ
    ## 27      ð̪̙ˤ
    ## 28      s̙ˤ
    ## 29      t̪̙ˤ
    ## 30       ʕ̙
    ## 31       ɑ̙
    ## 32       o̞̙
    ## 33       ɯ̙
    ## 34      ɯ̙i̯

``` r
# advanced tongue root (combining left tack below)
phoible %>% filter(grepl("̘", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1       e̘ː
    ## 2       ə̘ː
    ## 3       ɛ̘ː
    ## 4       o̘ː
    ## 5       ɔ̘ː
    ## 6        e̘
    ## 7        i̘
    ## 8        o̘
    ## 9        u̘
    ## 10       ɨ̘

``` r
# lowered (combining down tack below)
phoible %>% filter(grepl("̞", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##     Phoneme
    ## 1         ɵ̞
    ## 2        ɵ̞ː
    ## 3        e̞ˤ
    ## 4        ø̞ˤ
    ## 5        e̞ː
    ## 6        o̞ː
    ## 7         e̞
    ## 8         o̞
    ## 9         ẽ̞
    ## 10       ẽ̞ː
    ## 11        õ̞
    ## 12       õ̞ː
    ## 13        β̞
    ## 14       β̞ː
    ## 15        ɯ̞
    ## 16        z̞̩
    ## 17        ɤ̞
    ## 18       ɤ̞ː
    ## 19        ɤ̞̃
    ## 20       z̞̩ˠ
    ## 21       z̞̩̃ˠ
    ## 22        ɯ̞̯
    ## 23        ø̞
    ## 24       ø̞ː
    ## 25       ie̞
    ## 26        ʁ̞
    ## 27        ĕ̞
    ## 28        ŏ̞
    ## 29        ɵ̞̆
    ## 30       o̞ˤ
    ## 31        ɨ̞
    ## 32       ao̞
    ## 33       ɛo̞
    ## 34       e̞i
    ## 35       o̞i
    ## 36       o̞u
    ## 37       e̞u
    ## 38        e̞̥
    ## 39        o̞̥
    ## 40       ae̞
    ## 41       e̞ə
    ## 42       õ̞ĩ
    ## 43       e̞o̞
    ## 44      ae̞ˤ
    ## 45      ãẽ̞ˤ
    ## 46      ao̞ˤ
    ## 47      ãõ̞ˤ
    ## 48       ẽ̞ĩ
    ## 49       ẽ̞ũ
    ## 50       o̞a
    ## 51       õ̞ã
    ## 52      o̞aˤ
    ## 53      õ̞ãˤ
    ## 54       o̞e̞
    ## 55      o̞iˤ
    ## 56      õ̞ĩˤ
    ## 57       õ̞ˤ
    ## 58      o̞ˤː
    ## 59       ĩẽ̞
    ## 60       õ̞ũ
    ## 61        e̞̰
    ## 62        ẽ̞̰
    ## 63        o̞̰
    ## 64        õ̞̰
    ## 65        ɸ̞
    ## 66       ɪe̞
    ## 67        ɯ̞̆
    ## 68       io̞
    ## 69       e̞a
    ## 70       yø̞
    ## 71       ʁ̞ʷ
    ## 72       ʁ̞̰ʷ
    ## 73        ɨ̞̃
    ## 74       β̞ʲ
    ## 75        ʉ̞
    ## 76       β̞ˠ
    ## 77        e̠̞
    ## 78        o̟̞
    ## 79        ɤ̟̞̃
    ## 80       oe̞
    ## 81        w̞
    ## 82        ð̞
    ## 83        i̞
    ## 84       i̞ː
    ## 85        u̞
    ## 86       u̞ː
    ## 87        ŋ̞
    ## 88        d̞
    ## 89        æ̞̃
    ## 90        ë̞
    ## 91        ö̞
    ## 92        ɛ̞
    ## 93       i̞ä
    ## 94       i̞e̞
    ## 95        a̞
    ## 96        ɪ̞
    ## 97        œ̞
    ## 98       i̯œ̞
    ## 99       œ̞ʌ̯
    ## 100      ɪ̯u̞
    ## 101       y̞
    ## 102      o̞ä
    ## 103      ue̞
    ## 104      e̞u̯
    ## 105      o̞i̯
    ## 106       o̞̜
    ## 107      œ̞ː
    ## 108      œ̞ɛ̞
    ## 109       o̞̙
    ## 110      i̯e̞
    ## 111      ɯ̯ɤ̞
    ## 112       ʊ̞
    ## 113      ɟʝ̞
    ## 114       ɾ̞
    ## 115      u̞ˑ
    ## 116       ə̞̆
    ## 117      o̞̜ː
    ## 118       ɔ̞
    ## 119       ĩ̞
    ## 120      uo̞
    ## 121       ɔ̞̃
    ## 122      ɔ̞ː
    ## 123      ɔ̞̃ː
    ## 124       ɪ̞̈
    ## 125       ʊ̞̈
    ## 126       ø̠̞
    ## 127       ɨ̞̰
    ## 128      ɨ̞ː
    ## 129      ɨ̞̃ː
    ## 130       ð̺̞
    ## 131       ɵ̞̰
    ## 132       ø̞̜
    ## 133      ø̞̜ː
    ## 134       ɤ̞̆
    ## 135       ʃ̞
    ## 136      ʃ̞ʼ

``` r
# raised (combining up tack below)
phoible %>% filter(grepl("̝", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        ɶ̝
    ## 2        ʕ̝
    ## 3        ɒ̝
    ## 4        ɑ̝
    ## 5        r̝
    ## 6        e̝
    ## 7        o̝
    ## 8        ɹ̝
    ## 9        ɪ̝
    ## 10      ɪ̝ː
    ## 11       ɛ̝
    ## 12      ɛ̝ː
    ## 13       ɔ̝
    ## 14      ɔ̝ː
    ## 15       ɯ̝
    ## 16       ɑ̝̹
    ## 17      ɑ̝ː
    ## 18       ɑ̟̝
    ## 19       ɤ̝

``` r
# advanced (combining plus sign below)
phoible %>% filter(grepl("̟", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        k̟
    ## 2       a̟ː
    ## 3        ç̟
    ## 4       ɡ̟ʲ
    ## 5       ɣ̟ʲ
    ## 6        ʝ̟
    ## 7      k̟ʲʰ
    ## 8      k̟ʲʼ
    ## 9       x̟ʲ
    ## 10       a̟
    ## 11      a̟̙ː
    ## 12      a̟˞
    ## 13       ə̟
    ## 14      k̟ʰ
    ## 15      ŋɡ̟
    ## 16       c̟
    ## 17       ɲ̟
    ## 18       ʎ̟
    ## 19      ɲ̟ʝ̟
    ## 20       ã̟
    ## 21      ã̟ː
    ## 22       ɡ̟
    ## 23       ɤ̟
    ## 24      ia̟
    ## 25       j̟
    ## 26       o̟
    ## 27      a̟i
    ## 28      ã̟ĩ
    ## 29       o̟̞
    ## 30       ɤ̟̞̃
    ## 31  \u2c71̟
    ## 32       ʉ̟
    ## 33       d̟
    ## 34      d̟z̟
    ## 35      d̟ʒ̟
    ## 36       l̟
    ## 37       r̟
    ## 38       s̟
    ## 39       t̟
    ## 40      t̟s̟
    ## 41      t̟ʃ̟
    ## 42       z̟
    ## 43       ʊ̟
    ## 44     ɲ̟dʑ
    ## 45    ɲ̟tɕʰ
    ## 46       ɕ̟
    ## 47      ɕ̟ː
    ## 48     d̟ːʑ̟
    ## 49      d̟ʑ̟
    ## 50     t̟ːɕ̟
    ## 51      t̟ɕ̟
    ## 52       ʑ̟
    ## 53       ɐ̟
    ## 54       c̻̟
    ## 55       ɭ̟
    ## 56       ʎ̻̟
    ## 57       ɲ̻̟
    ## 58       ɳ̟
    ## 59       ɻ̟
    ## 60       ʈ̟
    ## 61       ʎ̟̥
    ## 62       ɲ̟̥
    ## 63       ɤ̟̹
    ## 64       ɯ̟
    ## 65       ɟ̟
    ## 66      ɑ̟ː
    ## 67       ɑ̟̝
    ## 68      ɑ̟ˑ
    ## 69      o̟ˑ
    ## 70       ɵ̟
    ## 71       ə̟̆
    ## 72       ɔ̟̜
    ## 73       u̟
    ## 74       ŋ̟
    ## 75       ɔ̟̜̰
    ## 76      ɔ̟̜ː
    ## 77      ɔ̟̜̃ː
    ## 78      ʉ̟ː
    ## 79      ɛa̟
    ## 80      ua̟
    ## 81       ɘ̟

``` r
# retracted (combining minus sign below)
phoible %>% filter(grepl("̠", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##      Phoneme
    ## 1         t̠ʃ
    ## 2        t̠ʃʰ
    ## 3        t̠ʃˀ
    ## 4         t̠͈ʃ
    ## 5        t̠͈ʃʷ
    ## 6       t̠ʃʷʰ
    ## 7       t̠ʃʷʼ
    ## 8        t̠ʃʼ
    ## 9         d̠ʒ
    ## 10        d̠ʒ̤
    ## 11       d̠ʒː
    ## 12         r̠
    ## 13       t̠ʃː
    ## 14       ˀd̠ʒ
    ## 15       n̠d̠ʒ
    ## 16        ɖr̠
    ## 17       ɳɖr̠
    ## 18       ɳʈr̠̥
    ## 19        ʈr̠̥
    ## 20        d̠͈ʒ
    ## 21       t̠͈ʃː
    ## 22        t̠ʃ͉
    ## 23       t̠ːʃ
    ## 24      t̠ʃʰː
    ## 25      t̠ʃʼː
    ## 26        n̠ʒ
    ## 27       ʰt̠ʃ
    ## 28      t̠ʃʲʰ
    ## 29         ɪ̠
    ## 30       d̠ʒʷ
    ## 31       t̠ʃʷ
    ## 32       t̠ʃʲ
    ## 33       d̠ʒʲ
    ## 34       d̠ʒ̤ː
    ## 35        d̠ⁿ
    ## 36         l̠
    ## 37         n̠
    ## 38         t̠
    ## 39        n̠d̠
    ## 40       n̠t̠ʃ
    ## 41         d̠
    ## 42         n̠̥
    ## 43         r̠̙
    ## 44      n̠t̠ʃʰ
    ## 45         ɗ̠
    ## 46         ɾ̠
    ## 47         ɺ̠
    ## 48        kǃ̠
    ## 49       kǃ̠ʰ
    ## 50        ŋǃ̠
    ## 51       ŋ̥ǃ̠ˀ
    ## 52        n̠t̠
    ## 53         e̠
    ## 54       d̠ʒˠ
    ## 55       d̠ʒʼ
    ## 56       t̠ʃˠ
    ## 57      t̠ʃʷː
    ## 58         ø̠
    ## 59         ɺ̠̰
    ## 60      kǃ̠xʰ
    ## 61       ŋ̥ǃ̠ʰ
    ## 62        t̠ʰ
    ## 63         d̠̰
    ## 64  ɖr̠\u0353
    ## 65        ʈɹ̠̥
    ## 66         l̠̤
    ## 67         n̠̤
    ## 68        ɡǃ̠
    ## 69       kǃ̠ˀ
    ## 70         e̠̞
    ## 71      n̠d̠ʒʷ
    ## 72        n̠ʃ
    ## 73       n̠ʃʷ
    ## 74         a̠
    ## 75        a̠ː
    ## 76        t̠ʃ̰
    ## 77       d̠ʒʱ
    ## 78       n̠̥t̠ʃ
    ## 79       t̠ʃ̺ʰ
    ## 80        t̠n̠
    ## 81       n̠̩d̠ʒ
    ## 82        d̠̤ʒ̤
    ## 83       n̠̤d̠̤ʒ
    ## 84      n̠t̠ʃʼ
    ## 85      n̠t̠ʃʷ
    ## 86       d̠ʒʰ
    ## 87      d̠ʒxʼ
    ## 88       t̠ʃx
    ## 89     t̪ʃ|t̠ʃ
    ## 90       d̠ʒɾ
    ## 91      n̠t̠ʃɾ
    ## 92       t̠ʃɾ
    ## 93        r̠ʲ
    ## 94        l̠˞
    ## 95         c̠
    ## 96         ɲ̠
    ## 97         i̠
    ## 98         o̠
    ## 99         u̠
    ## 100      d̠z̠ʲ
    ## 101       l̠ʲ
    ## 102       n̠ʲ
    ## 103     t̠s̠ʲʰ
    ## 104        ə̠̃
    ## 105      d̠ːʒ
    ## 106       cn̠
    ## 107       t̠͉ʃ
    ## 108     ʰt̠ʃː
    ## 109        ɛ̠
    ## 110     t̠ʃˤʰ
    ## 111     t̠ʃˤʼ
    ## 112        x̠
    ## 113      ⁿd̠ʒ
    ## 114       d̠ʲ
    ## 115       t̠ʲ
    ## 116      t̠ʃˤ
    ## 117        ä̠
    ## 118       ä̠ː
    ## 119       ɛ̠ː
    ## 120        ɞ̠
    ## 121       ɞ̠ː
    ## 122       iɛ̠
    ## 123       ɨä̠
    ## 124        œ̠
    ## 125       œ̠ː
    ## 126       ʉɞ̠
    ## 127       ʉœ̠
    ## 128        s̠
    ## 129        s̺̠
    ## 130        s̻̠
    ## 131       t̠ʃ̺
    ## 132        z̠
    ## 133        ɘ̠
    ## 134        ɨ̠
    ## 135     t̠ʃʲː
    ## 136     ʰt̠ʃʰ
    ## 137        ø̠̞
    ## 138        y̠
    ## 139       u̠u
    ## 140        ɺ̺̠

``` r
# centralized (combining diaresis)
phoible %>% filter(grepl("̈", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        ɪ̈
    ## 2       ɪ̈ː
    ## 3        ʊ̈
    ## 4       ʊ̈ː
    ## 5        ẅ
    ## 6        ö
    ## 7        ü
    ## 8        ë
    ## 9        ï
    ## 10       ä
    ## 11       ë̞
    ## 12       ö̞
    ## 13      üː
    ## 14      ëː
    ## 15       ɑ̈
    ## 16      ɑ̈ː
    ## 17      äi
    ## 18      äu̽
    ## 19      i̞ä
    ## 20     iäu̽
    ## 21      äː
    ## 22      o̞ä
    ## 23      uä
    ## 24      äu̯
    ## 25      äi̯
    ## 26       ä̠
    ## 27      ä̠ː
    ## 28      ɨä̠
    ## 29      ʊ̈i
    ## 30       ã̈
    ## 31      e̯ä
    ## 32      o̯ä
    ## 33      ɪ̯ä
    ## 34       ä̤
    ## 35      ã̈ː
    ## 36      ã̤̈ː
    ## 37      oä
    ## 38       ɪ̞̈
    ## 39       ʊ̞̈
    ## 40       ʊ̜̈

``` r
# mid-centralized (combining x above)
phoible %>% filter(grepl("̽", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1       äu̽
    ## 2      iäu̽
    ## 3       iɔ̽
    ## 4        ɔ̽
    ## 5       ɔ̽i
    ## 6       u̽i
    ## 7        i̽
    ## 8        ʊ̽
    ## 9        ɯ̽
    ## 10      ɯ̽ː
    ## 11      ɯ̰̽ː

``` r
# more round (combining right half ring)
phoible %>% filter(grepl("̹", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        ɐ̹̆
    ## 2        ɤ̟̹
    ## 3        ɨ̹
    ## 4        ɑ̝̹
    ## 5        z̹̩
    ## 6        ʐ̹̩
    ## 7        ɜ̹
    ## 8       ɜ̹ː
    ## 9       ɨ̹ː
    ## 10       ə̹

``` r
# less round (combining left half ring)
phoible %>% filter(grepl("̜", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        i̜
    ## 2        ɨ̜
    ## 3        u̜
    ## 4       au̜
    ## 5       ɐ̃u̜
    ## 6       eu̜
    ## 7       ɛu̜
    ## 8       iu̜
    ## 9        œ̜
    ## 10      œ̜ː
    ## 11       ɔ̜
    ## 12      ɔ̜ː
    ## 13       o̞̜
    ## 14       ʊ̜
    ## 15      ɔ̜̃ː
    ## 16      o̞̜ː
    ## 17       ɔ̟̜
    ## 18       w̜
    ## 19      w̜ʲ
    ## 20       ʊ̜̈
    ## 21       ɞ̜
    ## 22       ɞ̜̰
    ## 23      ɞ̜ː
    ## 24      ɞ̜̃ː
    ## 25       ɔ̟̜̰
    ## 26      ɔ̟̜ː
    ## 27      ɔ̟̜̃ː
    ## 28       ø̞̜
    ## 29      ø̞̜ː

``` r
# syllabic (combining vertical line below)
phoible %>% filter(grepl("̩", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        m̩
    ## 2        ŋ̩
    ## 3        z̞̩
    ## 4        n̩
    ## 5        l̩
    ## 6        ɲ̩
    ## 7        r̩
    ## 8       z̞̩ˠ
    ## 9       z̞̩̃ˠ
    ## 10       b̩
    ## 11       d̩
    ## 12       ɡ̩
    ## 13      ɡb̩
    ## 14      ɡ̩ʷ
    ## 15       ɣ̩
    ## 16      ŋ̩ʷ
    ## 17       v̩
    ## 18       z̩
    ## 19       l̪̩
    ## 20       ɭ̩
    ## 21       ɹ̩
    ## 22       ɟ̰̩
    ## 23     n̠̩d̠ʒ
    ## 24       ɽ̩
    ## 25      i̩ː
    ## 26       n̪̩
    ## 27       f̩
    ## 28       s̩
    ## 29       ʋ̩
    ## 30       z̹̩
    ## 31      z̩ˠ
    ## 32       ʐ̩
    ## 33       ʐ̹̩
    ## 34       ʒ̩
    ## 35       z̰̩

``` r
# non-syllabic (combining inverted breve below)
phoible %>% filter(grepl("̯", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##     Phoneme
    ## 1         ɪ̯
    ## 2         ʊ̯
    ## 3         ʌ̯
    ## 4        ɜɪ̯
    ## 5         o̯
    ## 6        ˀo̯
    ## 7         e̯
    ## 8         ə̯
    ## 9         ɤ̯
    ## 10        ɯ̞̯
    ## 11        ɛ̯
    ## 12        ɔ̯
    ## 13       æe̯
    ## 14       æẽ̯
    ## 15       ɔo̯
    ## 16       ɔõ̯
    ## 17       i̯a
    ## 18       i̯ɔ
    ## 19       i̯u
    ## 20       u̯ə
    ## 21       ɛi̯
    ## 22       ai̯
    ## 23       au̯
    ## 24       ei̯
    ## 25       eu̯
    ## 26      i̯ai̯
    ## 27      i̯au̯
    ## 28       i̯e
    ## 29      i̯eu̯
    ## 30       i̯o
    ## 31       oi̯
    ## 32       ou̯
    ## 33       u̯a
    ## 34      u̯ai̯
    ## 35       u̯e
    ## 36      u̯ei̯
    ## 37       ui̯
    ## 38       u̯i
    ## 39       u̯o
    ## 40       ɛu̯
    ## 41       i̯ɛ
    ## 42      i̯ãː
    ## 43       ii̯
    ## 44       uu̯
    ## 45       æu̯
    ## 46       æʌ̯
    ## 47       ɑi̯
    ## 48       ɑu̯
    ## 49       ɒu̯
    ## 50       eʌ̯
    ## 51       i̯æ
    ## 52       i̯ɑ
    ## 53       i̯ɒ
    ## 54       i̯ø
    ## 55       i̯œ
    ## 56       i̯œ̞
    ## 57       iu̯
    ## 58       iʌ̯
    ## 59       i̯ʌ
    ## 60       i̯y
    ## 61       œi̯
    ## 62       œu̯
    ## 63       œ̞ʌ̯
    ## 64       øu̯
    ## 65       oʌ̯
    ## 66       øʌ̯
    ## 67       uʌ̯
    ## 68       yu̯
    ## 69       yʌ̯
    ## 70       eɪ̯
    ## 71       æi̯
    ## 72       iə̯
    ## 73       uə̯
    ## 74       yə̯
    ## 75       ɔi̯
    ## 76       aɪ̯
    ## 77       aʊ̯
    ## 78       ɐ̃ɪ̯̃
    ## 79       ɐ̃ʊ̯̃
    ## 80       ẽɪ̯̃
    ## 81       eʊ̯
    ## 82       ɛɪ̯
    ## 83       ɛʊ̯
    ## 84       iʊ̯
    ## 85       oɪ̯
    ## 86       õɪ̯̃
    ## 87       oʊ̯
    ## 88       ɔɪ̯
    ## 89       ɔʊ̯
    ## 90       uɪ̯
    ## 91       ũɪ̯̃
    ## 92       uʊ̯
    ## 93      ʊ̯aɪ̯
    ## 94       ʊ̯ɐ̃
    ## 95       ɪ̯a
    ## 96       ɪ̯ɛ
    ## 97       ɪ̯u̞
    ## 98       ʊ̯ɔ
    ## 99      uei̯
    ## 100      ʌi̯
    ## 101      ae̯
    ## 102      aɵ̯
    ## 103      ɞʏ̯
    ## 104     ɑːi̯
    ## 105     eːi̯
    ## 106     oːu̯
    ## 107     ɛ̯æː
    ## 108      ɞe̯
    ## 109      ʉi̯
    ## 110      yi̯
    ## 111      ɐi̯
    ## 112      äu̯
    ## 113       i̯
    ## 114       u̯
    ## 115      u̯õ
    ## 116     u̯oː
    ## 117     u̯õː
    ## 118      äi̯
    ## 119      e̞u̯
    ## 120      i̯ẽ
    ## 121      o̞i̯
    ## 122     i̯æi
    ## 123     i̯ɛi
    ## 124     u̯æi
    ## 125     u̯ɛi
    ## 126      ãi̯
    ## 127      ãu̯
    ## 128      ẽi̯
    ## 129      õi̯
    ## 130      ɯ̙i̯
    ## 131      əi̯
    ## 132      əu̯
    ## 133      ɨi̯
    ## 134      ɔu̯
    ## 135      u̯ɛ
    ## 136      i̯e̞
    ## 137      ɯ̯ɤ̞
    ## 138      ɯi̯
    ## 139      æɪ̯
    ## 140      æ̃ɪ̯̃
    ## 141      ɔ̃ʊ̯̃
    ## 142       ĩ̯
    ## 143      e̯ä
    ## 144      o̯ä
    ## 145      ɪ̯ä
    ## 146      ɪ̯ɔ
    ## 147     i̯aː
    ## 148     i̯ai
    ## 149     i̯au
    ## 150     i̯oː
    ## 151     i̯oi
    ## 152     i̯uː
    ## 153    i̯uaː
    ## 154     i̯ui
    ## 155     u̯aː
    ## 156     u̯ai
    ## 157      u̯ɑ
    ## 158     ai̯ː
    ## 159     ãɪ̯ː
    ## 160     i̯uo
    ## 161     i̯ũo
    ## 162     ɔy̯ː
    ## 163     i̯ei̯
    ## 164     i̯ɛi̯
    ## 165     u̯eu̯
    ## 166     u̯ɛi̯
    ## 167     iɛi̯
    ## 168     aːi̯
    ## 169     ɛ̃ːi̯
    ## 170      y̯ø
    ## 171      i̯ɛ̃
    ## 172      u̯ã
    ## 173      u̯ə̃
    ## 174      u̯ɛ̃
    ## 175      u̯ɔ
    ## 176      u̯ɔ̃
    ## 177      øɪ̯
    ## 178      ʏi̯
    ## 179      ãũ̯
    ## 180      ɜi̯
    ## 181     aːu̯
    ## 182     ɒːi̯
    ## 183     øːy̯
    ## 184      øy̯
    ## 185     u̯ɔː
    ## 186      iɪ̯
    ## 187       ũ̯
    ## 188      ɯɪ̯
    ## 189      ɑʊ̯

``` r
# short (combining breve)
phoible %>% filter(grepl("̆", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1        ə̆
    ## 2        ă
    ## 3        ẵ
    ## 4        ĕ
    ## 5        ɛ̆
    ## 6        ɛ̆̃
    ## 7        ĭ
    ## 8        ŏ̃
    ## 9        ŭ
    ## 10       ŭ̃
    ## 11       ɯ̆
    ## 12       y̆
    ## 13       ɨ̆
    ## 14       ɒ̆
    ## 15       ĕ̞
    ## 16       ŏ̞
    ## 17       ɵ̞̆
    ## 18       ɔ̆
    ## 19       ø̆
    ## 20       ŏ
    ## 21       ɐ̆
    ## 22       ɐ̹̆
    ## 23       ʉ̆
    ## 24       ɪ̆
    ## 25       ɯ̞̆
    ## 26       ʊ̆
    ## 27     aːĭ
    ## 28     aːŭ
    ## 29     iːŭ
    ## 30     ɔːĭ
    ## 31     uːĭ
    ## 32       ə̞̆
    ## 33       ə̟̆
    ## 34       ɑ̆
    ## 35       ɘ̆
    ## 36       ɤ̞̆
    ## 37       ɵ̆

``` r
# devoiced (combining ring below)
phoible %>% filter(grepl("̥", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##         Phoneme
    ## 1             l̥
    ## 2             m̥
    ## 3             n̥
    ## 4             ɲ̥
    ## 5             ŋ̥
    ## 6             r̥
    ## 7             b̤̥
    ## 8             d̤̥
    ## 9             ɡ̤̥
    ## 10           tz̤̥
    ## 11            j̥
    ## 12            i̥
    ## 13            u̥
    ## 14           ŋ̥ʲ
    ## 15          ɳʈr̠̥
    ## 16           ʈr̠̥
    ## 17            d̪̤̥
    ## 18           m̥ʷ
    ## 19            ɓ̥
    ## 20            ɗ̥
    ## 21    kʟ\u0353̥ʼ
    ## 22           l̥ˠ
    ## 23           l̥ʲ
    ## 24           n̥ˠ
    ## 25           n̥ʲ
    ## 26           ɾ̥ˠ
    ## 27           ɾ̥ʲ
    ## 28            ə̥
    ## 29          n̪̥|n̥
    ## 30     ɬʟ\u0353̥
    ## 31          l̪̥|l̥
    ## 32          R̪̥|R̥
    ## 33            n̠̥
    ## 34            n̪̥
    ## 35           ŋ̥ʷ
    ## 36          ɹ̪̥|ɹ̥
    ## 37    ŋ̥ǀ\u0353ˀ
    ## 38    ŋ̥ǁ\u0353ˀ
    ## 39          ŋ̥ǃ̠ˀ
    ## 40            ɭ̥
    ## 41            ɳ̥
    ## 42           ŋ̥m̥
    ## 43            ḁ
    ## 44            e̞̥
    ## 45            o̞̥
    ## 46            ʛ̥
    ## 47    ŋ̥ǀ\u0353ʰ
    ## 48   ŋ̥ǀ\u0353xˀ
    ## 49          ŋ̥ǂʰ
    ## 50   ŋ̥ǂ\u0353ˡʰ
    ## 51  ŋ̥ǂ\u0353ˡxˀ
    ## 52   ŋ̥ǂ\u0353ˡˀ
    ## 53         ŋ̥ǂxˀ
    ## 54          ŋ̥ǂˀ
    ## 55           ŋ̥ǃ
    ## 56         ŋ̥ǃˠˀ
    ## 57          ŋ̥ǃˀ
    ## 58    ŋ̥ǁ\u0353ʰ
    ## 59          ŋ̥ǃ̠ʰ
    ## 60           ʈɹ̠̥
    ## 61          r̪̥|r̥
    ## 62            l̪̥
    ## 63      ʟ\u0353̥
    ## 64          kʟ̥ʼ
    ## 65            ɠ̥
    ## 66           ɠ̥ʲ
    ## 67           ɠ̥ʷ
    ## 68            ʄ̥
    ## 69            ɾ̥
    ## 70           ɱ̥f
    ## 71           n̥ʃ
    ## 72          n̠̥t̠ʃ
    ## 73            w̥
    ## 74            ɥ̥
    ## 75            ʎ̥
    ## 76            ɨ̥
    ## 77           ˀj̥
    ## 78           ˀl̪̥
    ## 79           ˀm̥
    ## 80           ˀn̪̥
    ## 81           ˀw̥
    ## 82            ɣ̥
    ## 83            x̥
    ## 84            z̥
    ## 85           l̥ː
    ## 86            e̥
    ## 87           e̥ː
    ## 88           m̥p
    ## 89           m̥ʰ
    ## 90           ɓ̥ː
    ## 91           j̥ʷ
    ## 92            ɺ̥
    ## 93           xʀ̥
    ## 94           m̥m
    ## 95           n̥n
    ## 96           ɲ̥ɲ̥
    ## 97            b̥
    ## 98           b̥ʰ
    ## 99            d̥
    ## 100          d̥z̥
    ## 101           ʎ̟̥
    ## 102           ɲ̟̥
    ## 103           r̺̥
    ## 104          b̥ː
    ## 105          d̥ː
    ## 106         d̥z̥ʲ
    ## 107          d̥ʑ̥
    ## 108         l̥ʲː
    ## 109          m̥ː
    ## 110          m̥ʲ
    ## 111         m̥ʲː
    ## 112          n̥ː
    ## 113         n̥ʲː
    ## 114          r̥ː
    ## 115          r̥ʲ
    ## 116         r̥ʲː
    ## 117           v̥
    ## 118           d̺̥
    ## 119          d̺̥z̺̥
    ## 120          d̥ʒ̥
    ## 121           ɹ̥
    ## 122           d̪̥
    ## 123          d̥ʲ
    ## 124           ɫ̥
    ## 125          ʰl̥
    ## 126          d̥ʒ̊
    ## 127          ɖ̥ʐ̥
    ## 128           ɢ̥
    ## 129          ɟ̥ʝ̥

``` r
# devoiced (combining ring above)
phoible %>% filter(grepl("̊", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##    Phoneme
    ## 1       ŋ̊ǀ
    ## 2       ŋ̊ǁ
    ## 3       ŋ̊ǂ
    ## 4       ŋ̊ǃ
    ## 5       ŋ̊ʘ
    ## 6        å
    ## 7        ɾ̪̊
    ## 8       ɾ̪̊ʰ
    ## 9       ŋ̊ŋ
    ## 10       ɡ̊
    ## 11      ɡ̊ʰ
    ## 12       ɲ̊
    ## 13       ŋ̊
    ## 14       j̊
    ## 15      ɡ̊ː
    ## 16       ɣ̊
    ## 17       ʒ̊
    ## 18      ɡ̊ʷ
    ## 19       ɽ̊
    ## 20      d̥ʒ̊
    ## 21       ɥ̊
    ## 22       l̪̊
    ## 23       n̪̊
    ## 24       ɻ̊

``` r
# unreleased (combining left angle above)
phoible %>% filter(grepl("̚", Phoneme)) %>% select(Phoneme) %>% distinct()
```

    ##   Phoneme
    ## 1       q̚
    ## 2       d̪̚

``` r
phoible %>% filter(grepl("̚", Phoneme)) %>% select(InventoryID, LanguageName, Phoneme)
```

    ##   InventoryID LanguageName Phoneme
    ## 1        1411       Ngomba       q̚
    ## 2        1567      Tangale       d̪̚
