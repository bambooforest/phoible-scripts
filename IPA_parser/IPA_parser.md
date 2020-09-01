Investigate segments that don’t part with IPA parser
================
Steven Moran

    library(tidyverse)
    library(knitr)

    col_types <- cols(InventoryID='i', Marginal='l', .default='c')
    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types = col_types)

    segments <- c('N', 'R', 'Rʲ', 'R̪', 'R̪', 'R̪̥', 'R̪̰', 'b̪', 'dʼkxʼ', 'd̠ʒxʼ', 'd̠ʓ', 'd̠ʓʷ', 'd̪ʼkxʼ', 'fʃ', 'hv', 'j̪', 'kɡ', 'k‼', 'k‼x', 'k‼xʼ', 'k‼ʰ', 'k‼ʰʼ', 'k‼ʼ', 'ld', 'm̪', 'm̼', 'pt', 'pʼkxʼ', 'p̪', 'p̼', 'st', 's̻θ', 'tʼkxʼ', 't̠ʃx', 't̠ʆ', 't̠ʆʰ', 't̠ʆʷ', 't̠ʆʷʰ', 't̠ʆʷʼ', 't̪ʙ', 't̪ʼkxʼ', 'v̼', 'xk', 'xʀ̥', 'ŋɡmb', 'ŋ‼', 'ŋ‼ʱ', 'ȴ', 'ȴʷ', 'ȵ', 'ȵʷ', 'ȶ', 'ȶȵ', 'ȶʷ', 'ȶ͈', 'ȶ͉', 'ɡʼkxʼ', 'ɡ‼', 'ɡ‼x', 'ɡ‼xʼ', 'ɡ‼ʱ', 'ɣv', 'ʀʁ', 'ʃt', 'ʆ', 'ʆʷ', 'ʆʷʼ', 'ʆʼ', 'ʍw', 'ʓ', 'ʓʷ', 'ᴅ', 'ᴅ̪', 'ᴅ̪̰')

    temp <- phoible %>% filter(Phoneme %in% segments) %>% select(InventoryID, Glottocode, LanguageName, Source, Phoneme) %>% arrange(Phoneme)

    temp %>% group_by(Source) %>% summarize(count=n()) %>% kable()

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Source | count |
|:-------|------:|
| aa     |     1 |
| ea     |    31 |
| er     |  1059 |
| gm     |    29 |
| ph     |     2 |
| saphon |    11 |
| spa    |     1 |
| upsid  |    22 |
| uz     |     9 |

    temp %>% group_by(Phoneme) %>% filter(Source=="ea") %>% summarize(count = n()) %>% arrange(desc(count)) %>% kable()

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Phoneme | count |
|:--------|------:|
| ʆ       |     4 |
| xk      |     4 |
| ʓ       |     4 |
| d̠ʓʷ     |     3 |
| ʆʼ      |     3 |
| ʆʷ      |     2 |
| t̠ʆʷʰ    |     2 |
| t̠ʆʷʼ    |     2 |
| ʓʷ      |     2 |
| d̠ʓ      |     1 |
| ʆʷʼ     |     1 |
| t̠ʆ      |     1 |
| t̠ʆʰ     |     1 |
| t̠ʆʷ     |     1 |

    phoible %>% filter(Phoneme == "d̠ʓʷ" )%>% select(InventoryID, Glottocode, LanguageName, Source, Phoneme)

    ## # A tibble: 3 x 5
    ##   InventoryID Glottocode LanguageName Source Phoneme
    ##         <int> <chr>      <chr>        <chr>  <chr>  
    ## 1        2349 adyg1241   Adyghe       ea     d̠ʓʷ    
    ## 2        2468 abkh1244   Abkhaz       ea     d̠ʓʷ    
    ## 3        2552 abkh1244   Abkhaz       ea     d̠ʓʷ

    temp %>% group_by(Phoneme) %>% filter(Source=="er") %>% summarize(count = n()) %>% arrange(desc(count)) %>% kable()

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Phoneme | count |
|:--------|------:|
| ȵ       |   379 |
| ȶ       |   313 |
| ȴ       |   202 |
| ȶ͈       |    66 |
| ȶ͉       |    64 |
| ȴʷ      |    10 |
| ȵʷ      |    10 |
| ȶʷ      |    10 |
| j̪       |     5 |

    phoible %>% filter(Phoneme == "ȵ") %>% head() %>% kable()

| InventoryID | Glottocode | ISO6393 | LanguageName      | SpecificDialect | GlyphID | Phoneme | Allophones | Marginal | SegmentClass | Source | tone | stress | syllabic | short | long | consonantal | sonorant | continuant | delayedRelease | approximant | tap | trill | nasal | lateral | labial | round | labiodental | coronal | anterior | distributed | strident | dorsal | high | low | front | back | tense | retractedTongueRoot | advancedTongueRoot | periodicGlottalSource | epilaryngealSource | spreadGlottis | constrictedGlottis | fortis | raisedLarynxEjective | loweredLarynxImplosive | click |
|------------:|:-----------|:--------|:------------------|:----------------|:--------|:--------|:-----------|:---------|:-------------|:-------|:-----|:-------|:---------|:------|:-----|:------------|:---------|:-----------|:---------------|:------------|:----|:------|:------|:--------|:-------|:------|:------------|:--------|:---------|:------------|:---------|:-------|:-----|:----|:------|:-----|:------|:--------------------|:-------------------|:----------------------|:-------------------|:--------------|:-------------------|:-------|:---------------------|:-----------------------|:------|
|        2158 | east2379   | aer     | Arrernte, Central | Alice Springs   | 0235    | ȵ       | ȵ nʷʲ      | FALSE    | consonant    | uz     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |
|        2629 | bura1267   | bvr     | Burarra           | NA              | 0235    | ȵ       | NA         | FALSE    | consonant    | er     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |
|        2630 | buna1275   | bck     | Bunuba            | NA              | 0235    | ȵ       | NA         | FALSE    | consonant    | er     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |
|        2631 | goon1238   | gni     | Gooniyandi        | NA              | 0235    | ȵ       | NA         | FALSE    | consonant    | er     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |
|        2632 | madn1237   | zml     | Matngele          | NA              | 0235    | ȵ       | NA         | FALSE    | consonant    | er     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |
|        2633 | mull1237   | mpb     | Malak-Malak       | NA              | 0235    | ȵ       | NA         | FALSE    | consonant    | er     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \-     | 0     | 0           | \+      | \+       | \+          | \-       | \+     | \+   | \-  | \+    | \-   | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |

    phoible %>% filter(Phoneme == "m̪")%>% kable()

| InventoryID | Glottocode | ISO6393 | LanguageName | SpecificDialect | GlyphID   | Phoneme | Allophones | Marginal | SegmentClass | Source | tone | stress | syllabic | short | long | consonantal | sonorant | continuant | delayedRelease | approximant | tap | trill | nasal | lateral | labial | round | labiodental | coronal | anterior | distributed | strident | dorsal | high | low | front | back | tense | retractedTongueRoot | advancedTongueRoot | periodicGlottalSource | epilaryngealSource | spreadGlottis | constrictedGlottis | fortis | raisedLarynxEjective | loweredLarynxImplosive | click |
|------------:|:-----------|:--------|:-------------|:----------------|:----------|:--------|:-----------|:---------|:-------------|:-------|:-----|:-------|:---------|:------|:-----|:------------|:---------|:-----------|:---------------|:------------|:----|:------|:------|:--------|:-------|:------|:------------|:--------|:---------|:------------|:---------|:-------|:-----|:----|:------|:-----|:------|:--------------------|:-------------------|:----------------------|:-------------------|:--------------|:-------------------|:-------|:---------------------|:-----------------------|:------|
|        1359 | shil1265   | shk     | Shilluk      | NA              | 006D+032A | m̪       | m̪          | FALSE    | consonant    | gm     | 0    | \-     | \-       | \-    | \-   | \+          | \+       | \-         | 0              | \-          | \-  | \-    | \+    | \-      | \+     | \-    | \-          | \+      | \+       | \+          | 0        | \-     | 0    | 0   | 0     | 0    | 0     | 0                   | 0                  | \+                    | \-                 | \-            | \-                 | \-     | \-                   | \-                     | \-    |

    phoible %>% filter(InventoryID == 1359) %>% select(LanguageName, Phoneme) %>% kable()

| LanguageName | Phoneme |
|:-------------|:--------|
| Shilluk      | b       |
| Shilluk      | b̪       |
| Shilluk      | d       |
| Shilluk      | f       |
| Shilluk      | h       |
| Shilluk      | j       |
| Shilluk      | k       |
| Shilluk      | l       |
| Shilluk      | m       |
| Shilluk      | m̪       |
| Shilluk      | n       |
| Shilluk      | p       |
| Shilluk      | p̪       |
| Shilluk      | t       |
| Shilluk      | tç      |
| Shilluk      | w       |
| Shilluk      | x       |
| Shilluk      | ð       |
| Shilluk      | ŋ       |
| Shilluk      | ɟʝ      |
| Shilluk      | ɡ       |
| Shilluk      | ɣ       |
| Shilluk      | ɲ       |
| Shilluk      | ɹ       |
| Shilluk      | θ       |
| Shilluk      | ˦       |
| Shilluk      | ˧       |
| Shilluk      | ˨       |
| Shilluk      | a       |
| Shilluk      | au      |
| Shilluk      | aɪ      |
| Shilluk      | aː      |
| Shilluk      | e       |
| Shilluk      | eɪ      |
| Shilluk      | eː      |
| Shilluk      | e̥       |
| Shilluk      | e̥ː      |
| Shilluk      | i       |
| Shilluk      | iː      |
| Shilluk      | o       |
| Shilluk      | oː      |
| Shilluk      | u       |
| Shilluk      | uː      |
| Shilluk      | ɑ       |
| Shilluk      | ɑː      |
| Shilluk      | ɔ       |
| Shilluk      | ɔɪ      |
| Shilluk      | ɔː      |
| Shilluk      | ɛ       |
| Shilluk      | ɛː      |
| Shilluk      | ɪ       |
| Shilluk      | ɪː      |
| Shilluk      | ʊ       |
| Shilluk      | ʊː      |
