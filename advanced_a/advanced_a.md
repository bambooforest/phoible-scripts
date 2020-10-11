Advanced front vowel?
================
Steven Moran
&lt;<a href="mailto:steven.moran@unine.ch" class="email">steven.moran@unine.ch</a>&gt;
11 October, 2020

    library(tidyverse)
    library(knitr)

Overview
========

A question regarding \[a̟\]: as an advanced front vowel is raised here:

<a href="https://linguistics.stackexchange.com/questions/37345/a%cc%9f-advanced-front-vowel" class="uri">https://linguistics.stackexchange.com/questions/37345/a%cc%9f-advanced-front-vowel</a>

So in which inventory/ies does it occur and why?

Load the PHOIBLE development data from the GitHub repository.

    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types=c(InventoryID='i', Marginal='l', .default='c'))

Get just the rows that contain an advanced \[a\].

    advanced_a <- phoible %>% filter(grepl("a̟", Phoneme))

Trim it down a bit to check if it’s something that’s source-specific.

    advanced_a %>% select(InventoryID, Glottocode, ISO6393, LanguageName, Source, Phoneme) %>% kable()

| InventoryID | Glottocode | ISO6393 | LanguageName     | Source | Phoneme |
|------------:|:-----------|:--------|:-----------------|:-------|:--------|
|           4 | kaba1278   | kbd     | Kabardian        | spa    | a̟ː      |
|           5 | nucl1302   | kat     | Georgian         | spa    | a̟       |
|          14 | cent1989   | khm     | Cambodian        | spa    | a̟       |
|          14 | cent1989   | khm     | Cambodian        | spa    | a̟̙ː      |
|          15 | viet1252   | vie     | Vietnamese       | spa    | a̟ː      |
|          16 | mand1415   | cmn     | Mandarin Chinese | spa    | a̟˞      |
|          52 | nucl1632   | set     | Sentani          | spa    | a̟       |
|          56 | gads1258   | gaj     | Gadsup           | spa    | a̟ː      |
|          93 | tewa1260   | tew     | Tewa             | spa    | a̟       |
|          93 | tewa1260   | tew     | Tewa             | spa    | a̟ː      |
|          93 | tewa1260   | tew     | Tewa             | spa    | ã̟       |
|          93 | tewa1260   | tew     | Tewa             | spa    | ã̟ː      |
|          95 | hopi1249   | hop     | Hopi             | spa    | a̟ː      |
|         130 | tigr1270   | tig     | Tigre            | spa    | a̟ː      |
|         132 | egyp1253   | arz     | Egyptian Arabic  | spa    | a̟       |
|         141 | gaaa1244   | gaa     | Ga               | spa    | a̟       |
|         141 | gaaa1244   | gaa     | Ga               | spa    | ã̟       |
|         150 | cent2050   | knc     | Kanuri           | spa    | a̟       |
|         155 | nama1265   | naq     | Nama             | spa    | a̟       |
|         155 | nama1265   | naq     | Nama             | spa    | ã̟       |
|         157 | bret1244   | bre     | Breton           | spa    | a̟       |
|         162 | stan1290   | fra     | French           | spa    | a̟       |
|         166 | russ1263   | rus     | Russian          | spa    | a̟       |
|         172 | west2369   | pes     | Persian          | spa    | a̟       |
|         178 | sinh1246   | sin     | Sinhalese        | spa    | a̟       |
|         187 | nort2697   | azj     | Azerbaijani      | spa    | a̟       |
|         193 | nort2745   | ykg     | Yukaghir         | spa    | a̟       |
|         193 | nort2745   | ykg     | Yukaghir         | spa    | a̟ː      |
|         198 | afad1236   | aal     | KOTOKO           | upsid  | a̟       |
|         224 | ando1256   | ano     | ANDOKE           | upsid  | a̟       |
|         224 | ando1256   | ano     | ANDOKE           | upsid  | ã̟       |
|         236 | nort2697   | azj     | AZERBAIJANI      | upsid  | a̟       |
|         259 | lave1249   | brb     | BRAO             | upsid  | a̟       |
|         298 | dann1241   | daf     | DAN              | upsid  | a̟       |
|         298 | dann1241   | daf     | DAN              | upsid  | ã̟       |
|         322 | even1260   | eve     | EVEN             | upsid  | ia̟      |
|         330 | fefe1239   | fmp     | FE?FE?           | upsid  | a̟       |
|         331 | stan1290   | fra     | FRENCH           | upsid  | a̟       |
|         336 | gads1258   | gaj     | GADSUP           | upsid  | a̟ː      |
|         373 | itel1242   | itl     | ITELMEN          | upsid  | a̟       |
|         390 | nucl1302   | kat     | GEORGIAN         | upsid  | a̟       |
|         391 | kaba1278   | kbd     | KABARDIAN        | upsid  | a̟ː      |
|         407 | cent1989   | khm     | KHMER            | upsid  | a̟       |
|         407 | cent1989   | khm     | KHMER            | upsid  | a̟ː      |
|         409 | kiow1266   | kio     | KIOWA            | upsid  | a̟       |
|         409 | kiow1266   | kio     | KIOWA            | upsid  | a̟i      |
|         409 | kiow1266   | kio     | KIOWA            | upsid  | ã̟       |
|         409 | kiow1266   | kio     | KIOWA            | upsid  | ã̟ĩ      |
|         413 | west2632   | kjq     | ACOMA            | upsid  | a̟       |
|         422 | konk1267   | knn     | KONKANI          | upsid  | a̟       |
|         422 | konk1267   | knn     | KONKANI          | upsid  | ã̟       |
|         427 | kory1246   | kpy     | KORYAK           | upsid  | a̟       |
|         489 | newa1246   | new     | NEWARI           | upsid  | a̟       |
|         489 | newa1246   | new     | NEWARI           | upsid  | ã̟       |
|         494 | ngan1291   | nio     | NGANASAN         | upsid  | a̟       |
|         502 | nung1283   | nut     | LUNGCHOW         | upsid  | a̟       |
|         510 | ormu1247   | oru     | ORMURI           | upsid  | a̟       |
|         516 | west2369   | pes     | FARSI            | upsid  | a̟       |
|         523 | pwon1235   | pww     | PHLONG           | upsid  | a̟       |
|         538 | selk1253   | sel     | SELKUP           | upsid  | a̟       |
|         538 | selk1253   | sel     | SELKUP           | upsid  | a̟ː      |
|         540 | nucl1632   | set     | SENTANI          | upsid  | a̟       |
|         545 | sinh1246   | sin     | SINHALESE        | upsid  | a̟       |
|         576 | tigr1270   | tig     | TIGRE            | upsid  | a̟ː      |
|         617 | kama1365   | woi     | WOISIKA          | upsid  | a̟       |
|         620 | wara1290   | wrz     | WARAY            | upsid  | a̟       |
|         634 | nort2745   | ykg     | YUKAGHIR         | upsid  | a̟       |
|        2566 | skol1241   | sms     | Skolt Saami      | ea     | a̟       |
|        2566 | skol1241   | sms     | Skolt Saami      | ea     | ua̟      |
|        2566 | skol1241   | sms     | Skolt Saami      | ea     | ɛa̟      |
|        2594 | stan1289   | cat     | Catalan          | ea     | a̟       |

It’s mainly in SPA and UPSID (with two data points in EA).

EA
--

[EA](http://eurasianphonology.info/) lists all its segments
[here](http://eurasianphonology.info/segments). In the “Plain series”
for vowels, advanced a is listed under “near-front central”. It is found
in
[Saami](http://eurasianphonology.info/search_exact?dialects=True&query=a%CC%9F)

In the [pre-release version of EA](https://eurphon.info/), [Catalan
(Mallorcan)](https://eurphon.info/languages/html?lang_id=443) also had
the advanced \[a\]. The inventory seems to have been removed in the
stable version of EA. The inventory source:

-   Lloret, Maria-Rosa. 2011. La fonologia del català. Col. «El que
    sabem de…» Articles de suport a la docència. Barcelona: Santillana.

UPSID
-----

In UPSID, the conversion from ASCII representations to Unicode IPA shows
that:

-   [low front unrounded
    vowel](https://github.com/phoible/dev/blob/master/raw-data/UPSID/UPSID_IPA_correspondences.tsv#L819)
    is encoded as &lt;a+&gt;

and

-   [low central unrounded
    vowel](https://github.com/phoible/dev/blob/master/raw-data/UPSID/UPSID_IPA_correspondences.tsv#L809)
    as <a>

The low front unrounded vowel is reported in 26 languages (our of 451):

-   <a href="http://menzerath.phonetik.uni-frankfurt.de/S/S0302.html" class="uri">http://menzerath.phonetik.uni-frankfurt.de/S/S0302.html</a>

SPA
---

The [IPA correspondences in
SPA](https://github.com/phoible/dev/blob/master/raw-data/SPA/SPA_IPA_correspondences.tsv)
are more detailed than UPSID because SPA contains a rich set of
allophonic (phonetic) descriptions.

Here we also have an “a” vs “a-front” distinction:

-   <a href="https://github.com/phoible/dev/blob/master/raw-data/SPA/SPA_IPA_correspondences.tsv#L1536-L1537" class="uri">https://github.com/phoible/dev/blob/master/raw-data/SPA/SPA_IPA_correspondences.tsv#L1536-L1537</a>

Contrastive?
------------

Let’s look and see if there is an “a” vs “a-front” phonemic contrast in
any of these languages.

    df <- phoible %>% filter(InventoryID %in% advanced_a$InventoryID) %>% select(InventoryID, Glottocode, ISO6393, LanguageName, SpecificDialect, Source, Phoneme, Allophones, SegmentClass)
    df %>% filter(grepl("a", Phoneme)) %>% kable()

| InventoryID | Glottocode | ISO6393 | LanguageName     | SpecificDialect          | Source | Phoneme | Allophones  | SegmentClass |
|------------:|:-----------|:--------|:-----------------|:-------------------------|:-------|:--------|:------------|:-------------|
|           4 | kaba1278   | kbd     | Kabardian        | NA                       | spa    | a̟ː      | a̟ː ɑː       | vowel        |
|           5 | nucl1302   | kat     | Georgian         | NA                       | spa    | a̟       | a̟ ɑ         | vowel        |
|          14 | cent1989   | khm     | Cambodian        | NA                       | spa    | a̟       | a̟           | vowel        |
|          14 | cent1989   | khm     | Cambodian        | NA                       | spa    | a̟̙ː      | a̟̙ː          | vowel        |
|          15 | viet1252   | vie     | Vietnamese       | NA                       | spa    | a̟ː      | a̟ː          | vowel        |
|          16 | mand1415   | cmn     | Mandarin Chinese | NA                       | spa    | a       | ɐ ɜ æ a̟ ɑ a | vowel        |
|          16 | mand1415   | cmn     | Mandarin Chinese | NA                       | spa    | a˞      | a˞ ɐ˞       | vowel        |
|          16 | mand1415   | cmn     | Mandarin Chinese | NA                       | spa    | a̟˞      | a̟˞ ɜ˞       | vowel        |
|          52 | nucl1632   | set     | Sentani          | NA                       | spa    | a̟       | a̟           | vowel        |
|          56 | gads1258   | gaj     | Gadsup           | NA                       | spa    | a̟ː      | a̟ː          | vowel        |
|          93 | tewa1260   | tew     | Tewa             | NA                       | spa    | a̟       | a̟           | vowel        |
|          93 | tewa1260   | tew     | Tewa             | NA                       | spa    | a̟ː      | a̟ː          | vowel        |
|          93 | tewa1260   | tew     | Tewa             | NA                       | spa    | ã̟       | ã̟           | vowel        |
|          93 | tewa1260   | tew     | Tewa             | NA                       | spa    | ã̟ː      | ã̟ː          | vowel        |
|          95 | hopi1249   | hop     | Hopi             | NA                       | spa    | a       | a           | vowel        |
|          95 | hopi1249   | hop     | Hopi             | NA                       | spa    | aː      | aː          | vowel        |
|          95 | hopi1249   | hop     | Hopi             | NA                       | spa    | ă       | ă̟ ă ă ə̆     | vowel        |
|          95 | hopi1249   | hop     | Hopi             | NA                       | spa    | a̟ː      | a̟ː          | vowel        |
|         130 | tigr1270   | tig     | Tigre            | NA                       | spa    | a̟ː      | a̟ː          | vowel        |
|         132 | egyp1253   | arz     | Egyptian Arabic  | NA                       | spa    | a̟       | a̟ ɒ         | vowel        |
|         141 | gaaa1244   | gaa     | Ga               | NA                       | spa    | a̟       | a̟ ã̟         | vowel        |
|         141 | gaaa1244   | gaa     | Ga               | NA                       | spa    | ã̟       | ã̟           | vowel        |
|         150 | cent2050   | knc     | Kanuri           | NA                       | spa    | a̟       | ɐ ɒ a̟ æ     | vowel        |
|         155 | nama1265   | naq     | Nama             | NA                       | spa    | a̟       | a̟           | vowel        |
|         155 | nama1265   | naq     | Nama             | NA                       | spa    | ã̟       | ã̟           | vowel        |
|         157 | bret1244   | bre     | Breton           | NA                       | spa    | a̟       | a̟           | vowel        |
|         162 | stan1290   | fra     | French           | NA                       | spa    | a̟       | a̟ a̟ː a̟      | vowel        |
|         166 | russ1263   | rus     | Russian          | NA                       | spa    | a̟       | a̟ ɑ ə æ ʌ   | vowel        |
|         172 | west2369   | pes     | Persian          | NA                       | spa    | a̟       | a̟           | vowel        |
|         178 | sinh1246   | sin     | Sinhalese        | NA                       | spa    | a       | a           | vowel        |
|         178 | sinh1246   | sin     | Sinhalese        | NA                       | spa    | aː      | aː          | vowel        |
|         178 | sinh1246   | sin     | Sinhalese        | NA                       | spa    | ãː      | ãː          | vowel        |
|         178 | sinh1246   | sin     | Sinhalese        | NA                       | spa    | a̟       | a̟           | vowel        |
|         187 | nort2697   | azj     | Azerbaijani      | NA                       | spa    | a̟       | a̟ a         | vowel        |
|         193 | nort2745   | ykg     | Yukaghir         | NA                       | spa    | a̟       | a a̟         | vowel        |
|         193 | nort2745   | ykg     | Yukaghir         | NA                       | spa    | a̟ː      | aː a̟ː       | vowel        |
|         198 | afad1236   | aal     | KOTOKO           | NA                       | upsid  | a̟       | NA          | vowel        |
|         224 | ando1256   | ano     | ANDOKE           | NA                       | upsid  | a̟       | NA          | vowel        |
|         224 | ando1256   | ano     | ANDOKE           | NA                       | upsid  | ã̟       | NA          | vowel        |
|         236 | nort2697   | azj     | AZERBAIJANI      | NA                       | upsid  | a̟       | NA          | vowel        |
|         259 | lave1249   | brb     | BRAO             | NA                       | upsid  | a       | NA          | vowel        |
|         259 | lave1249   | brb     | BRAO             | NA                       | upsid  | a̟       | NA          | vowel        |
|         298 | dann1241   | daf     | DAN              | NA                       | upsid  | a       | NA          | vowel        |
|         298 | dann1241   | daf     | DAN              | NA                       | upsid  | ã       | NA          | vowel        |
|         298 | dann1241   | daf     | DAN              | NA                       | upsid  | a̟       | NA          | vowel        |
|         298 | dann1241   | daf     | DAN              | NA                       | upsid  | ã̟       | NA          | vowel        |
|         322 | even1260   | eve     | EVEN             | NA                       | upsid  | ia̟      | NA          | vowel        |
|         330 | fefe1239   | fmp     | FE?FE?           | NA                       | upsid  | a̟       | NA          | vowel        |
|         331 | stan1290   | fra     | FRENCH           | NA                       | upsid  | a̟       | NA          | vowel        |
|         336 | gads1258   | gaj     | GADSUP           | NA                       | upsid  | a̟ː      | NA          | vowel        |
|         373 | itel1242   | itl     | ITELMEN          | NA                       | upsid  | a̟       | NA          | vowel        |
|         390 | nucl1302   | kat     | GEORGIAN         | NA                       | upsid  | a̟       | NA          | vowel        |
|         391 | kaba1278   | kbd     | KABARDIAN        | NA                       | upsid  | a̟ː      | NA          | vowel        |
|         407 | cent1989   | khm     | KHMER            | NA                       | upsid  | a̟       | NA          | vowel        |
|         407 | cent1989   | khm     | KHMER            | NA                       | upsid  | a̟ː      | NA          | vowel        |
|         409 | kiow1266   | kio     | KIOWA            | NA                       | upsid  | a̟       | NA          | vowel        |
|         409 | kiow1266   | kio     | KIOWA            | NA                       | upsid  | a̟i      | NA          | vowel        |
|         409 | kiow1266   | kio     | KIOWA            | NA                       | upsid  | ã̟       | NA          | vowel        |
|         409 | kiow1266   | kio     | KIOWA            | NA                       | upsid  | ã̟ĩ      | NA          | vowel        |
|         413 | west2632   | kjq     | ACOMA            | NA                       | upsid  | ai      | NA          | vowel        |
|         413 | west2632   | kjq     | ACOMA            | NA                       | upsid  | au      | NA          | vowel        |
|         413 | west2632   | kjq     | ACOMA            | NA                       | upsid  | a̟       | NA          | vowel        |
|         422 | konk1267   | knn     | KONKANI          | NA                       | upsid  | a       | NA          | vowel        |
|         422 | konk1267   | knn     | KONKANI          | NA                       | upsid  | ã       | NA          | vowel        |
|         422 | konk1267   | knn     | KONKANI          | NA                       | upsid  | a̟       | NA          | vowel        |
|         422 | konk1267   | knn     | KONKANI          | NA                       | upsid  | ã̟       | NA          | vowel        |
|         427 | kory1246   | kpy     | KORYAK           | NA                       | upsid  | a̟       | NA          | vowel        |
|         489 | newa1246   | new     | NEWARI           | NA                       | upsid  | a̟       | NA          | vowel        |
|         489 | newa1246   | new     | NEWARI           | NA                       | upsid  | ã̟       | NA          | vowel        |
|         494 | ngan1291   | nio     | NGANASAN         | NA                       | upsid  | a       | NA          | vowel        |
|         494 | ngan1291   | nio     | NGANASAN         | NA                       | upsid  | a̟       | NA          | vowel        |
|         502 | nung1283   | nut     | LUNGCHOW         | NA                       | upsid  | aɯ      | NA          | vowel        |
|         502 | nung1283   | nut     | LUNGCHOW         | NA                       | upsid  | ă       | NA          | vowel        |
|         502 | nung1283   | nut     | LUNGCHOW         | NA                       | upsid  | a̟       | NA          | vowel        |
|         510 | ormu1247   | oru     | ORMURI           | NA                       | upsid  | a̟       | NA          | vowel        |
|         516 | west2369   | pes     | FARSI            | NA                       | upsid  | a̟       | NA          | vowel        |
|         523 | pwon1235   | pww     | PHLONG           | NA                       | upsid  | a       | NA          | vowel        |
|         523 | pwon1235   | pww     | PHLONG           | NA                       | upsid  | ai      | NA          | vowel        |
|         523 | pwon1235   | pww     | PHLONG           | NA                       | upsid  | au      | NA          | vowel        |
|         523 | pwon1235   | pww     | PHLONG           | NA                       | upsid  | aɨ      | NA          | vowel        |
|         523 | pwon1235   | pww     | PHLONG           | NA                       | upsid  | a̟       | NA          | vowel        |
|         538 | selk1253   | sel     | SELKUP           | NA                       | upsid  | a       | NA          | vowel        |
|         538 | selk1253   | sel     | SELKUP           | NA                       | upsid  | aː      | NA          | vowel        |
|         538 | selk1253   | sel     | SELKUP           | NA                       | upsid  | a̟       | NA          | vowel        |
|         538 | selk1253   | sel     | SELKUP           | NA                       | upsid  | a̟ː      | NA          | vowel        |
|         540 | nucl1632   | set     | SENTANI          | NA                       | upsid  | a̟       | NA          | vowel        |
|         545 | sinh1246   | sin     | SINHALESE        | NA                       | upsid  | aː      | NA          | vowel        |
|         545 | sinh1246   | sin     | SINHALESE        | NA                       | upsid  | a̟       | NA          | vowel        |
|         576 | tigr1270   | tig     | TIGRE            | NA                       | upsid  | a̟ː      | NA          | vowel        |
|         617 | kama1365   | woi     | WOISIKA          | NA                       | upsid  | a       | NA          | vowel        |
|         617 | kama1365   | woi     | WOISIKA          | NA                       | upsid  | a̟       | NA          | vowel        |
|         620 | wara1290   | wrz     | WARAY            | NA                       | upsid  | a̟       | NA          | vowel        |
|         634 | nort2745   | ykg     | YUKAGHIR         | NA                       | upsid  | a̟       | NA          | vowel        |
|        2566 | skol1241   | sms     | Skolt Saami      | Skolt Saami (Suõʹnnʼjel) | ea     | a̟       | NA          | vowel        |
|        2566 | skol1241   | sms     | Skolt Saami      | Skolt Saami (Suõʹnnʼjel) | ea     | ua̟      | NA          | vowel        |
|        2566 | skol1241   | sms     | Skolt Saami      | Skolt Saami (Suõʹnnʼjel) | ea     | ɛa̟      | NA          | vowel        |
|        2594 | stan1289   | cat     | Catalan          | Catalan (Mallorcan)      | ea     | a̟       | NA          | vowel        |

And indeed there is – although not in every case of “a” vs “a-advanced”.
