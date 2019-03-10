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

Test the different features
===========================

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
table(phoible$continuant)
```

    ## 
    ##       -   -,-,+     -,+   -,+,+       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   44556      79     694       4   57964       9    2151      23       1

``` r
table(phoible$delayedRelease)
```

    ## 
    ##       -   -,-,+     -,+   -,+,+       +     +,-       0   0,-,+ 0,0,-,+ 
    ##   27439      39     481       7   19487       6   57987      34       1

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
