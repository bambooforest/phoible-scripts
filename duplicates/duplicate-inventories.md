Which languages in PHOIBLE have multiple inventories?
================
Steven Moran
30 July, 2020

    library(testthat)
    library(dplyr)
    library(knitr)

Overview
========

PHOIBLE has both ISO 639-3 language name identifiers (aka language
codes) and Glottocodes for most phonological inventories in its dataset:

<a href="https://github.com/phoible/dev/blob/master/mappings/InventoryID-LanguageCodes.csv" class="uri">https://github.com/phoible/dev/blob/master/mappings/InventoryID-LanguageCodes.csv</a>

ISO codes identify language-level entries. Glottocodes identify all
languoids, i.e. all families, languages, and dialects. In PHOIBLE, we
assign the “lowest” level Glottocode, e.g. the dialect level when it’s
known for a given phonological inventory.

This means that there are two levels of multiple inventories for data
points in PHOIBLE: at the ISO versus the Glottocode levels.

Get the latest phoible dev data.

    phoible <- read.csv('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true')
    # expect_equal(nrow(phoible), 105467) # latest phoible number of data points

Collapse inventories and get each inventory’s phoneme count (note the
warning that some have NAs).

    counts <- phoible %>% group_by(InventoryID, Glottocode, ISO6393) %>% summarize(phonemes = n()) %>% arrange(ISO6393, Glottocode, phonemes)

    ## `summarise()` regrouping output by 'InventoryID', 'Glottocode' (override with `.groups` argument)

    expect_equal(nrow(counts), 3020) # phoible 2.0 has 3020 data points

Get the distribution of multiple inventories at the ISO level.

    dup.iso <- counts %>% group_by(ISO6393) %>% summarize(inventories = n()) %>% arrange(desc(inventories))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    # expect_equal(nrow(dup.iso), 2092) # phoible 2.0 number of unique iso codes (should be less than dup glottocodes)

We expect more duplicates entries at the ISO 639-3 level because it
encodes languages, but phoible often has multiple data points at the
dialect level.

    table(dup.iso$inventories) %>% kable()

| Var1 | Freq |
|:-----|-----:|
| 1    | 1542 |
| 2    |  361 |
| 3    |  126 |
| 4    |   36 |
| 5    |   19 |
| 6    |    7 |
| 7    |    2 |
| 8    |    3 |
| 9    |    1 |
| 10   |    1 |
| 12   |    1 |
| 28   |    1 |

Note that some ISO 639-3 codes are (currently) NA, hence the 37
“duplicates” in the tables above and below.

    head(dup.iso) %>% kable()

| ISO6393 | inventories |
|:--------|------------:|
| mis     |          28 |
| oss     |          12 |
| bzr     |          10 |
| eng     |           9 |
| eus     |           8 |
| khg     |           8 |

Get the distribution of multiple inventories at the Glottocode level.

    dup.glottocodes <- counts %>% group_by(Glottocode) %>% summarize(inventories = n()) %>% arrange(desc(inventories))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    # expect_equal(nrow(dup.glottocodes), 2184) # phoible 2.0 number of unique glottocodes

What’s the distribution?

    table(dup.glottocodes$inventories) %>% kable()

| Var1 | Freq |
|:-----|-----:|
| 1    | 1641 |
| 2    |  352 |
| 3    |  119 |
| 4    |   36 |
| 5    |   18 |
| 6    |    4 |
| 7    |    2 |
| 8    |    2 |
| 9    |    1 |
| 10   |    1 |
| 11   |    1 |

There are less duplicates per Glottocode code (subsumes dialects).

    head(dup.glottocodes) %>% kable()

| Glottocode | inventories |
|:-----------|------------:|
| osse1243   |          11 |
| biri1256   |          10 |
| stan1293   |           9 |
| dutc1256   |           8 |
| kham1282   |           8 |
| basq1248   |           7 |

Here’s an example.

    counts %>% filter(Glottocode=="osse1243") %>% arrange(InventoryID)

    ## # A tibble: 11 x 4
    ## # Groups:   InventoryID, Glottocode [11]
    ##    InventoryID Glottocode ISO6393 phonemes
    ##          <int> <chr>      <chr>      <int>
    ##  1         976 osse1243   oss           39
    ##  2        2283 osse1243   oss           39
    ##  3        2324 osse1243   oss           47
    ##  4        2351 osse1243   oss           44
    ##  5        2389 osse1243   oss           47
    ##  6        2408 osse1243   oss           47
    ##  7        2424 osse1243   oss           47
    ##  8        2432 osse1243   oss           40
    ##  9        2476 osse1243   oss           44
    ## 10        2509 osse1243   oss           43
    ## 11        2626 osse1243   oss           46

    counts %>% filter(ISO6393=="oss") %>% arrange(InventoryID)

    ## # A tibble: 12 x 4
    ## # Groups:   InventoryID, Glottocode [12]
    ##    InventoryID Glottocode ISO6393 phonemes
    ##          <int> <chr>      <chr>      <int>
    ##  1         976 osse1243   oss           39
    ##  2        2283 osse1243   oss           39
    ##  3        2324 osse1243   oss           47
    ##  4        2351 osse1243   oss           44
    ##  5        2389 osse1243   oss           47
    ##  6        2408 osse1243   oss           47
    ##  7        2424 osse1243   oss           47
    ##  8        2432 osse1243   oss           40
    ##  9        2476 osse1243   oss           44
    ## 10        2509 osse1243   oss           43
    ## 11        2590 digo1242   oss           42
    ## 12        2626 osse1243   oss           46

Show some ISO singletons.

    single.iso <- dup.iso %>% filter(inventories == 1)
    temp <- left_join(counts, single.iso)

    ## Joining, by = "ISO6393"

    temp %>% arrange(InventoryID) %>% filter(!is.na(inventories)) %>% head() %>% kable()

| InventoryID | Glottocode | ISO6393 | phonemes | inventories |
|------------:|:-----------|:--------|---------:|------------:|
|          35 | sund1252   | sun     |       26 |           1 |
|          42 | maor1246   | mri     |       20 |           1 |
|          47 | mara1386   | zmr     |       24 |           1 |
|          55 | tele1256   | tlf     |       27 |           1 |
|          57 | awiy1238   | auy     |       20 |           1 |
|          68 | squa1248   | squ     |       37 |           1 |

Get the ranges, means, and stuff for ISO.

    temp1 <- counts %>% group_by(ISO6393) %>% summarize(num_inventories = n(), min_phonemes = min(phonemes), max_phonemes = max(phonemes), mean_phonemes = mean(phonemes)) %>% arrange(desc(num_inventories))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    temp2 <- counts %>% group_by(ISO6393) %>% summarize(range = paste(phonemes, collapse=","))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    temp <- left_join(temp1, temp2)

    ## Joining, by = "ISO6393"

Note range may not be ordered correctly. Again, NA is meaningless here.

    temp %>% head(n=20) %>% kable()

| ISO6393 | num\_inventories | min\_phonemes | max\_phonemes | mean\_phonemes | range                                                                               |
|:--------|-----------------:|--------------:|--------------:|---------------:|:------------------------------------------------------------------------------------|
| mis     |               28 |            17 |            68 |       27.82143 | 32,22,21,36,38,18,19,28,28,23,58,23,22,29,23,29,23,26,17,25,29,28,21,21,26,23,23,68 |
| oss     |               12 |            39 |            47 |       43.75000 | 42,39,39,40,43,44,44,46,47,47,47,47                                                 |
| bzr     |               10 |            18 |            18 |       18.00000 | 18,18,18,18,18,18,18,18,18,18                                                       |
| eng     |                9 |            39 |            45 |       41.22222 | 39,39,39,40,40,41,44,44,45                                                          |
| eus     |                8 |            28 |            43 |       32.25000 | 28,28,28,30,30,35,36,43                                                             |
| khg     |                8 |            51 |           133 |       77.75000 | 51,61,62,64,77,78,96,133                                                            |
| nld     |                8 |            37 |            58 |       49.37500 | 37,39,43,51,54,56,57,58                                                             |
| gup     |                7 |            24 |            28 |       26.71429 | 27,27,27,28,24,27,27                                                                |
| mhr     |                7 |            27 |            34 |       30.42857 | 27,27,30,31,31,33,34                                                                |
| gwn     |                6 |            46 |            65 |       53.83333 | 46,48,48,52,64,65                                                                   |
| kca     |                6 |            25 |            36 |       30.33333 | 25,26,31,32,32,36                                                                   |
| lit     |                6 |            40 |            60 |       52.50000 | 40,47,52,57,59,60                                                                   |
| lzz     |                6 |            32 |            38 |       36.00000 | 32,34,36,38,38,38                                                                   |
| nyf     |                6 |            47 |            47 |       47.00000 | 47,47,47,47,47,47                                                                   |
| nys     |                6 |            22 |            22 |       22.00000 | 22,22,22,22,22,22                                                                   |
| sgw     |                6 |            39 |            45 |       42.33333 | 44,39,42,42,42,45                                                                   |
| ben     |                5 |            43 |            72 |       52.60000 | 43,47,48,53,72                                                                      |
| bod     |                5 |            34 |            56 |       44.60000 | 34,41,42,50,56                                                                      |
| bsk     |                5 |            43 |            59 |       50.20000 | 43,48,48,53,59                                                                      |
| bym     |                5 |            17 |            21 |       19.00000 | 17,18,19,20,21                                                                      |

Get the ranges, means, and stuff for Glottocodes.

    temp1 <- counts %>% group_by(Glottocode) %>% summarize(num_inventories = n(), min_phonemes = min(phonemes), max_phonemes = max(phonemes), mean_phonemes = mean(phonemes)) %>% arrange(desc(num_inventories))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    temp2 <- counts %>% group_by(Glottocode) %>% summarize(range = paste(phonemes, collapse=","))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    temp <- left_join(temp1, temp2)

    ## Joining, by = "Glottocode"

Note range may not be ordered correctly. Some languages have
consistently the same (or similar) numbers of phonemes. Others vary
greatly.

    temp %>% head(n=20) %>% kable()

| Glottocode | num\_inventories | min\_phonemes | max\_phonemes | mean\_phonemes | range                            |
|:-----------|-----------------:|--------------:|--------------:|---------------:|:---------------------------------|
| osse1243   |               11 |            39 |            47 |       43.90909 | 39,39,40,43,44,44,46,47,47,47,47 |
| biri1256   |               10 |            18 |            18 |       18.00000 | 18,18,18,18,18,18,18,18,18,18    |
| stan1293   |                9 |            39 |            45 |       41.22222 | 39,39,39,40,40,41,44,44,45       |
| dutc1256   |                8 |            37 |            58 |       49.37500 | 37,39,43,51,54,56,57,58          |
| kham1282   |                8 |            51 |           133 |       77.75000 | 51,61,62,64,77,78,96,133         |
| basq1248   |                7 |            28 |            36 |       30.71429 | 28,28,28,30,30,35,36             |
| east2328   |                7 |            27 |            34 |       30.42857 | 27,27,30,31,31,33,34             |
| gwan1268   |                6 |            46 |            65 |       53.83333 | 46,48,48,52,64,65                |
| khan1273   |                6 |            25 |            36 |       30.33333 | 25,26,31,32,32,36                |
| lazz1240   |                6 |            32 |            38 |       36.00000 | 32,34,36,38,38,38                |
| lith1251   |                6 |            40 |            60 |       52.50000 | 40,47,52,57,59,60                |
| beng1280   |                5 |            43 |            72 |       52.60000 | 43,47,48,53,72                   |
| bidy1243   |                5 |            17 |            21 |       19.00000 | 17,18,19,20,21                   |
| buru1296   |                5 |            43 |            59 |       50.20000 | 43,48,48,53,59                   |
| chec1245   |                5 |            44 |            70 |       61.80000 | 44,64,64,67,70                   |
| gali1262   |                5 |            15 |            28 |       19.40000 | 15,15,17,22,28                   |
| gang1268   |                5 |            18 |            22 |       18.80000 | 18,18,18,18,22                   |
| haus1257   |                5 |            31 |            46 |       40.60000 | 31,38,43,45,46                   |
| hind1269   |                5 |            55 |            94 |       68.40000 | 55,58,61,74,94                   |
| iris1253   |                5 |            49 |            69 |       57.60000 | 49,50,52,68,69                   |

Compare differences individual inventories
==========================================

We can use the `pull` command in `dplyr` to compare phoneme inventories
next to each other. For example, incorrect ejectives in one of the two
barb1263 inventories.

Example of doculect differences
-------------------------------

### barb1263

Barbareño
(<a href="https://glottolog.org/resource/languoid/id/barb1263" class="uri">https://glottolog.org/resource/languoid/id/barb1263</a>)
is an example of two inventories from different doculects (see
references below) that have slightly different phoneme inventories.

<a href="https://phoible.org/inventories/view/1239" class="uri">https://phoible.org/inventories/view/1239</a>

versus:

<a href="https://phoible.org/inventories/view/862" class="uri">https://phoible.org/inventories/view/862</a>

Inventory (PH) 1239 has 44 phonemes:

    phoible %>% filter(InventoryID == 1239) %>% pull(Phoneme)

    ##  [1] "h"   "j"   "jˀ"  "k"   "kʰ"  "kʼ"  "l"   "lˀ"  "m"   "mˀ"  "n"   "nˀ" 
    ## [13] "p"   "pʰ"  "pʼ"  "q"   "qʰ"  "qʼ"  "s"   "sʰ"  "sʼ"  "t"   "ts"  "tsʰ"
    ## [25] "tsʼ" "tʰ"  "tʼ"  "t̠ʃ"  "t̠ʃʰ" "t̠ʃʼ" "w"   "wˀ"  "ʃ"   "ʃʰ"  "ʃʼ"  "ʔ"  
    ## [37] "χ"   "χʼ"  "a"   "e"   "i"   "o"   "u"   "ɨ"

Inventory (PH) 862 has 40 phonemes:

    phoible %>% filter(InventoryID == 862) %>% pull(Phoneme)

    ##  [1] "h"   "j"   "jˀ"  "k"   "kʰ"  "kʼ"  "l"   "lˀ"  "m"   "mˀ"  "n"   "nˀ" 
    ## [13] "p"   "pʰ"  "pʼ"  "q"   "qʰ"  "qʼ"  "s"   "sʰ"  "t"   "ts"  "tsʰ" "tsʼ"
    ## [25] "tʰ"  "tʼ"  "t̠ʃ"  "t̠ʃʰ" "t̠ʃʼ" "w"   "wˀ"  "ʃ"   "ʔ"   "χ"   "a"   "e"  
    ## [37] "i"   "o"   "u"   "ɨ"

The difference between the two inventories is reflected clearly in their
sources, i.e. Beeler 1971 posits four additional segments (/ s̰, ̰ʃ,̰ x,
ʃʰ /)whereas Wash 2001 does not.

References:

    @phdthesis{boi_wash2001,
        Author = {Wash, Suzanne},
        Date-Added = {2013-09-01 18:16:42 +0000},
        Date-Modified = {2013-09-01 18:16:42 +0000},
        Filenames = {boi_wash2001.pdf},
        School = {The University of California at Santa Barbara},
        Title = {{Adverbial Clauses in Barbare{\~n}o Chumash Narrative Discourse}},
        Year = {2001}}

    @article{boi_chumash1970,
        Author = {Beeler, M. S.},
        Date-Added = {2013-09-01 18:16:42 +0000},
        Date-Modified = {2013-09-01 18:16:42 +0000},
        Filenames = {boi_chumash1970.pdf},
        Journal = {International Journal of American Linguistics},
        Number = {1},
        Pages = {14--17},
        Publisher = {The University of Chicago Press},
        Title = {{Sibilant Harmony in Chumash}},
        Volume = {36},
        Year = {1970}}

trum1247
--------

Two inventories for Trumai
(<a href="https://glottolog.org/resource/languoid/id/trum1247" class="uri">https://glottolog.org/resource/languoid/id/trum1247</a>),
an isolate spoken in Brazil, are given as follows. The first comes from
UPSID (Maddieson 194l Maddieson & Precoda 1990) and the second from work
by Guiardello (1999). This is another case where more recent fieldwork
reanalyzes the language. (Note also that inventories in UPSID were
typologized by Maddieson, i.e. sometimes reanalyzed so that the segments
were unified across descriptions to make typological comparison
possible. The work by Maddieson built on the precursor database, the
Stanford Phonology Archive, and there are systematic differences between
inventories in the two sources from the same doculects. We revisit this
issue in a separate section. )

<a href="https://phoible.org/inventories/view/588" class="uri">https://phoible.org/inventories/view/588</a>

    phoible %>% filter(InventoryID == 588) %>% pull(Phoneme)

    ##  [1] "f"  "h"  "j"  "k"  "l̪"  "m̥"  "n̪"  "p"  "r̪"  "s"  "t"  "ts" "t̪"  "w"  "xː"
    ## [16] "ɗ̥"  "ʃ"  "ʔ"  "a"  "i"  "o"  "u"  "ɘ"  "ɛ"

Note it does not contain ejectives. The second is from Guiardello (1999)
and it does list ejectives.

<a href="https://phoible.org/inventories/view/1939" class="uri">https://phoible.org/inventories/view/1939</a>

    phoible %>% filter(InventoryID == 1939) %>% pull(Phoneme)

    ##  [1] "d"   "h"   "j"   "k"   "kʼ"  "l"   "m"   "n"   "p"   "s"   "t"   "ts" 
    ## [13] "tsʼ" "tʼ"  "t̪"   "t̪ʼ"  "w"   "x"   "ɬ"   "ɸ"   "ɾ"   "ʃ"   "ʔ"   "a"  
    ## [25] "e"   "i"   "o"   "u"   "ɨ"

Guiardello (1999:1) writes:

    Trumai has 23 consonants and 6 vowels. In Guiardello (1992), the number of Trumai phonemic consonants was smaller, given that the analysis for some phones was different from the current one: the lateral fricative /ɬ/ was analyzed as an allophone of /l/, but now it is analyzed as a independent phoneme; the affricate /ts/ and the ejectives /t̪ʼ/, /tʼ/,/kʼ/, and /tsʼ/ were previously classified as consonant clusters (i.e. /t+s/ and /t+s+ʔ/), but now we believe they are more adequately analyzed as single phonemes. Therefore, the consonant chart presented here has some differences in relation to the one presented in previous work. 

Wikipedia
(<a href="https://en.wikipedia.org/wiki/Trumai_language" class="uri">https://en.wikipedia.org/wiki/Trumai_language</a>)
notes:

    This inventory is atypical of Amazonian languages (Trumai is a recent immigrant to the Xingu basin) in its ejective consonants, the lateral fricative /ɬ/, and the alveolar–dental distinction. Guirardello, who specializes on Trumai, has presented varied inventories of these phonemes: Guirardello (1999a)[4] lists /t̪ t̪' ts ts' s/ as dental, and /t t' d n l ɬ ɾ/ as alveolar; whereas Guirardello (1999b) lists only /t/ and /t'/ as alveolar.[18] Younger speakers do not make the ejective distinction.

    @book{Trumai1975,
        Address = {Paris},
        Author = {Monod-Becquelin, Aurore},
        Date-Added = {2013-09-01 12:19:49 +0000},
        Date-Modified = {2013-09-01 12:19:49 +0000},
        Iso_Code = {tpy},
        Keywords = {Trumai},
        Olac_Field = {phonology},
        Publisher = {Centre National de la Recherche Scientifique},
        Refdb_Id = {http://wals.info/refdb/record/575},
        Title = {{La pratique linguistique des indiens trumai (Haut-Xingu, Mato Grosso, Br{\'{e}}sil)}},
        Wals_Code = {tru},
        Year = {1975}}

    @phdthesis{1939_Guiardello1999,
        Author = {Guiardello, Raquel},
        Bibtexkey = {Guiardello1999},
        Date-Added = {2014-07-09 13:41:25 +0000},
        Date-Modified = {2014-07-09 13:41:26 +0000},
        Inventoryid = {1939},
        Languagecode = {tpy},
        Languagename = {Trumai},
        Languagevariantcode = {tpy},
        Pages = {1--6},
        School = {Rice -- University},
        Title = {A Reference Grammar of Trumai},
        Year = {1999}}

### Khanty

<a href="https://phoible.org/languages/khan1273" class="uri">https://phoible.org/languages/khan1273</a>

### Greek

<a href="https://phoible.org/languages/mode1248" class="uri">https://phoible.org/languages/mode1248</a>

TODO
====

### Moseten

    phoible %>% filter(InventoryID == 944) %>% pull(Phoneme)

    ##  [1] "b"   "d"   "dʲ"  "f"   "j"   "k"   "kʰ"  "l"   "m"   "n"   "p"   "pʰ" 
    ## [13] "r"   "s"   "t"   "ts"  "tsʰ" "tʲ"  "t̠ʃ"  "t̠ʃʰ" "w"   "ɡ"   "ɲ"   "ʃ"  
    ## [25] "ʔ"   "χ"   "a"   "ã"   "e"   "ẽ"   "i"   "iː"  "ĩ"   "ĩː"  "o"   "õ"  
    ## [37] "ə"   "ə̃"

    phoible %>% filter(InventoryID == 1986) %>% pull(Phoneme)

    ##  [1] "b"   "d"   "dʲ"  "f"   "h"   "j"   "k"   "kʰ"  "m"   "n"   "p"   "pʰ" 
    ## [13] "s"   "t"   "ts"  "tʲ"  "t̠ʃ"  "t̠ʃʰ" "ɲ"   "ɾ"   "ʃ"   "ʋ"   "ʔ"   "a"  
    ## [25] "ã"   "e"   "eː"  "ẽ"   "ẽː"  "i"   "iː"  "ĩ"   "ĩː"  "o"   "õ"   "ə"  
    ## [37] "əː"  "ə̃"   "ə̃ː"

Uvulars
-------

Same doculect, same inventory?
------------------------------

We need to check that inventories from the same source documents contain
the same realizations, e.g.

-   <a href="https://phoible.org/inventories/view/2266" class="uri">https://phoible.org/inventories/view/2266</a>
-   <a href="https://phoible.org/inventories/view/2219" class="uri">https://phoible.org/inventories/view/2219</a>

<!-- -->

    phoible %>% filter(InventoryID == 2266) %>% pull(Phoneme)

    ##  [1] "b"  "dʑ" "d̪"  "f"  "h"  "j"  "k"  "l"  "m"  "n"  "p"  "q"  "s"  "tɕ" "t̪" 
    ## [16] "v"  "z"  "ɕ"  "ɡ"  "ɾ"  "ʁ"  "ʑ"  "χ"  "ä"  "e"  "i"  "o"  "u"  "ɵ"

    phoible %>% filter(InventoryID == 2219) %>% pull(Phoneme)

    ##  [1] "b"  "dʑ" "d̻"  "f"  "h"  "j"  "k"  "l"  "m"  "n"  "p"  "q"  "s"  "tɕ" "t̻" 
    ## [16] "v"  "x"  "z"  "ɕ̟"  "ɡ"  "ɾ"  "ʁ"  "ʑ̟"  "e"  "i"  "o"  "u"  "ɑ"  "ɵ"

Investigating Dan
-----------------

-   <a href="https://phoible.org/inventories/view/298" class="uri">https://phoible.org/inventories/view/298</a>
-   <a href="https://phoible.org/inventories/view/1393" class="uri">https://phoible.org/inventories/view/1393</a>

<!-- -->

    phoible %>% 
      filter(InventoryID == 1393 | InventoryID == 298) %>%
      select(InventoryID, Phoneme) %>%
      group_split(InventoryID) %>%
      lapply(function(tib) pull(tib, Phoneme))

    ## [[1]]
    ##  [1] "b"      "d"      "f"      "j"      "k"      "kp"     "l"      "m"     
    ##  [9] "n"      "p"      "s"      "t"      "v"      "w"      "z"      "ɓ"     
    ## [17] "ɡ"      "ɡb"     "\u1d91" "a"      "ã"      "a̟"      "ã̟"      "e"     
    ## [25] "ẽ̞"      "i"      "ĩ"      "o"      "õ̞"      "u"      "ũ"      "ɒ"     
    ## [33] "ɒ̃"      "ɔ"      "ɘ"      "ə̃"      "ɛ"      "ɜ"      "ɨ"     
    ## 
    ## [[2]]
    ##  [1] "b"      "bʲ"     "d"      "dʲ"     "f"      "fʲ"     "j"      "k"     
    ##  [9] "kp"     "kpʲ"    "kʲ"     "kʷ"     "l"      "m"      "mʲ"     "n"     
    ## [17] "nʲ"     "nʷ"     "p"      "pʲ"     "s"      "sʲ"     "sʷ"     "t"     
    ## [25] "tʲ"     "v"      "vʲ"     "w"      "wʲ"     "z"      "zʲ"     "zʷ"    
    ## [33] "ŋ"      "ɓ"      "ɓʲ"     "ɗ"      "ɗʲ"     "ɡ"      "ɡb"     "ɡbʲ"   
    ## [41] "ɡʲ"     "ɡʷ"     "ɲ"      "\u1d91" "˥"      "˥˨˧"    "˦"      "˧"     
    ## [49] "˧˥˨"    "˧˨"     "˨"      "˩"      "a"      "aː"     "ã"      "ãː"    
    ## [57] "e"      "eː"     "i"      "iː"     "ĩ"      "ĩː"     "o"      "oː"    
    ## [65] "u"      "uː"     "ũ"      "ũː"     "æ"      "æː"     "æ̃"      "æ̃ː"    
    ## [73] "ɒ"      "ɒː"     "ɒ̃"      "ɒ̃ː"     "ɔ"      "ɔː"     "ɔ̃"      "ɔ̃ː"    
    ## [81] "ə"      "əː"     "ə̃"      "ə̃ː"     "ɛ"      "ɛː"     "ɛ̃"      "ɛ̃ː"    
    ## [89] "ɨ"      "ɨː"     "ɵ"      "ɵː"

    phoible %>% 
      filter(InventoryID == 962 | InventoryID == 1411) %>%
      select(InventoryID, Phoneme) %>%
      arrange(Phoneme) %>%
      group_split(InventoryID) %>%
      lapply(function(tib) pull(tib, Phoneme))

    ## [[1]]
    ##  [1] "˦"  "˦˨" "˨"  "˨˦" "a"  "bv" "ç"  "cç" "d"  "dz" "ə"  "ɛ"  "f"  "ɡ"  "i" 
    ## [16] "j"  "ɟʝ" "k"  "m"  "n"  "ɲ"  "ŋ"  "ɔ"  "p"  "pf" "s"  "t"  "ts" "u"  "ʉ" 
    ## [31] "w"  "ʔ" 
    ## 
    ## [[2]]
    ##  [1] "˦"  "˨"  "a"  "ɛ"  "f"  "ɣ"  "i"  "ɨ"  "j"  "k"  "l"  "m"  "n"  "ŋ"  "ɔ" 
    ## [16] "p"  "pf" "s"  "t"  "ts" "u"  "v"  "w"  "z"  "ʔ"

SPA vs UPSID
------------

Maddieson typologized many of the phonological inventories in SPA when
creating UPSID. Hence, many of the inventories point to the same
bibliographic sources. Moran (2012) notes some systematic difference
between the UPSID and SPA, in particular, the way in which the vowels
are encoded with diacritics in UPSID. UPSID also jettisoned descriptions
of tone.

Investigate Cham in SPA vs UPSID (todo: make a comparison of all
SPA/UPSID languages and quantify the differences).

    # https://github.com/phoible/dev/issues/276
    # 

    # Clean up
    rm(list = ls())
