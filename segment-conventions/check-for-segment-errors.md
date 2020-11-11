Check for errors in PHOIBLE segment conventions
================
Steven Moran
(11 November, 2020)

Let’s load the libraries and PHOIBLE.

    library(dplyr)
    library(readr)
    library(testthat)
    library(knitr)

    col_types <- cols(InventoryID = "i", Marginal = "l", .default = "c")
    phoible <- read_csv(url("https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true"), col_types = col_types)

    # Note the phoible dev version
    expect_equal(nrow(phoible), 105460) # https://github.com/phoible/dev/commit/9d21f8fa7d8bb592f6aa6378fb456757354d1441

# Overview

Check for segment errors in the [PHOIBLE dev
data](https://github.com/phoible/dev).

First, we look at various suggested updates from Cormac Anderson.

He notes:

-   Sesotho not in New Guinea ;). PH1017 should be iso: sot, glotto:
    sout2807 not iso: sso, glotto: siss1243

This is an old issue that’s already been resolved
[here](https://github.com/phoible/dev/issues/221).

-   In GM 1378 (Mawa) the inventory given lists a number of phonemes,
    i.e. /ʔ/, /ʔw/, /h/, /f/, /z/ that the source (Roberts 2009)
    explicitly says are not phonemic.

This issue is raised here:

-   <a href="https://github.com/phoible/dev/issues/318" class="uri">https://github.com/phoible/dev/issues/318</a>

Next:

-   Would you be happy to represent the Arandic stop-nasal segments in
    PH 1163, PH 1235, UZ 2158 as pre-stopped nasals, e.g. /ᵗn/, rather
    than stop-nasal clusters, e.g. /tn/? cf. the prestopped lateral in
    Kiowa UPSID 409

This will be discussed over email.

Next:

-   Just another brief follow up on the Arandic ones. I see now that UZ
    2158 uses voiced stop plus nasal, e.g. /bm/, /dn/, whereas PH 1163
    and PH 1235 used voiceless stop plus nasal, e.g. /pm/, /tn/. I
    haven’t looked at these languages for a while, so don’t know if any
    phonetic differences are attested in the original material, but I
    would expect that it is just transcriptional and that there would be
    a case for using the same convention.

So let’s check where we have incompatible transcription practices.

Which rows have voiced/voiceless stop + nasal segments?

    phoible %>% filter(grepl("bm|pm|dn|tn", Phoneme)) %>% select(InventoryID, Glottocode, LanguageName, SpecificDialect, Phoneme, Allophones, Source) %>% kable()

| InventoryID | Glottocode | LanguageName      | SpecificDialect  | Phoneme | Allophones | Source |
|------------:|:-----------|:------------------|:-----------------|:--------|:-----------|:-------|
|        1163 | alya1239   | Alyawarra         | NA               | pm      | pm         | ph     |
|        1163 | alya1239   | Alyawarra         | NA               | tn      | tn tʰ      | ph     |
|        1235 | west2441   | Arrarnte          | Western Arrarnte | pm      | pm         | ph     |
|        1235 | west2441   | Arrarnte          | Western Arrarnte | tn      | tn         | ph     |
|        2158 | east2379   | Arrernte, Central | Alice Springs    | bm      | bm bmʷ     | uz     |
|        2158 | east2379   | Arrernte, Central | Alice Springs    | dn      | dn dnʷ     | uz     |

This needs investigation.

Now display all the issues and suggestions that CA made. Some of these
also need investigation.

    cormac <- read_tsv('PHOIBLE data corrections.tsv')

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   INVENTORY = col_character(),
    ##   OLD = col_character(),
    ##   NEW = col_character(),
    ##   JUSTIFICATION = col_character()
    ## )

    cormac %>% kable()

| INVENTORY   | OLD  | NEW  | JUSTIFICATION                                                                                                                                                                                                                                            |
|:------------|:-----|:-----|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| SAPHON 1921 | tx   | tʃ   | Source (Alexander 2005: 96) says that <x> is likely /ʃ/                                                                                                                                                                                                  |
| SAPHON 2118 | tx   | tʃ   | Vasconcelos, Ione P. 2003. Aspectos fonológicos e morfofonológicos da língua Aikanã. Ph.D. dissertation, Universidade Federal de Alagoas, Maceió.                                                                                                        |
| SPA 63      | tx   | tʰ   | Source (Li 1946) says all aspirated stops appear with “guttural spirant glide”, but only marks it on the coronal (others are Cʰ)                                                                                                                         |
| GM 1260     | nl   | NA   | looking at the original source (Nforgwei 2004), /nl/ is in the phonemic chart on p. 28 but not in previous (phonetic) chart on p. 20 nor in discussion on p. 42-43. I would conclude that it is ephemeral                                                |
| GM 1576     | ŋgmb | ŋmgb | Original source (Lienhardt and Giger 2009) has rather /ŋmgb/                                                                                                                                                                                             |
| GM 1514     | hv   | hʷ   | I cannot access the original source (Konoshenko 2009), but Konoshenko (2019) has rather hʷ                                                                                                                                                               |
| GM 1500     | km   | kpⁿ  | Original source (Boyd 1999) describes these as postnasalised labiovelars                                                                                                                                                                                 |
| GM 1500     | gm   | gbⁿ  | Original source (Boyd 1999) describes these as postnasalised labiovelars                                                                                                                                                                                 |
| GM 1336     | ɲɟʑ  | ɲdʑ  | Source (Kutsch Lojenga 1994) has this as /ɲdʑ/                                                                                                                                                                                                           |
| PH 1168     | tl   | tɬ   | In a series with segments transcribed /tɬʰ/ and /tɬ’/                                                                                                                                                                                                    |
| UZ 2214     | ʔj   | ʔʲ   | Source (Tench 2007) says that /ʔj/ in Tera is the same as /ʔj/ in Hausa, which is included in UZ 2188 as /ʔʲ/.                                                                                                                                           |
| GM 1518     | kg   | k    | I can’t access the original (Khatchaturyan 2009) but in the PhD thesis of the same author (Khatchaturyan 2015) we see /k/ and /g/ listed separately, with no sign of anything that could be /kg/. I presume this is a simple mistake of missing a space. |
| GM 1518     | kg   | g    | I can’t access the original (Khatchaturyan 2009) but in the PhD thesis of the same author (Khatchaturyan 2015) we see /k/ and /g/ listed separately, with no sign of anything that could be /kg/. I presume this is a simple mistake of missing a space. |
| PH 878      | ŋb   | ŋgʲ  | Original source (Boyeldieu 200) has no /ŋb/but /ŋgʲ/, /ŋg/, /ngb/ missing from inventory                                                                                                                                                                 |
| PH 878      | ŋb   | ŋg   | Original source (Boyeldieu 200) has no /ŋb/but /ŋgʲ/, /ŋg/, /ngb/ missing from inventory                                                                                                                                                                 |
| PH 878      | ŋb   | ŋgb  | Original source (Boyeldieu 200) has no /ŋb/but /ŋgʲ/, /ŋg/, /ngb/ missing from inventory                                                                                                                                                                 |
| UZ 2231     | nb   | mb   | Source (Chirkova et al. 2013) lists prenasalised voiced stops /Nb/, /Nd/, /Ng/ etc. which presumably should be understood as /mb/, /nd/, /ŋg/, etc. The inventory has /ŋg/, but rather /nb/                                                              |
| UZ 2203     | ɳɡ   | ŋg   | Source (Dawd and Hayward 2002) indeed have /ɳɡ/ and /ɳɡʷ/ in the chart, but I presume that this is a typo, as in the examples they have rather the expected /ŋg/ and /ŋgʷ/                                                                               |
| UZ 2203     | ɳɡʷ  | ŋgʷ  | Source (Dawd and Hayward 2002) indeed have /ɳɡ/ and /ɳɡʷ/ in the chart, but I presume that this is a typo, as in the examples they have rather the expected /ŋg/ and /ŋgʷ/                                                                               |
| GM 1318     | n̠̩d̠ʒ  | n̠d̠ʒ  | Presume that the syllabic marker on the n is a typo. No suggestion of this in source (Gowlett 2003)                                                                                                                                                      |
| GM 1424     | gbr  | br   | In source (Persson 2004) for Baka but not for Morokodo. Adjacent columns at top of second page, it’s an easy mistake.                                                                                                                                    |
| GM 1424     | kpr  | pr   | In source (Persson 2004) for Baka but not for Morokodo. Adjacent columns at top of second page, it’s an easy mistake.                                                                                                                                    |

# Older issues

Get a list of unique segments and which sources they appear in.

    phoneme.source <- phoible %>% select(Phoneme, Source) %>% group_by(Phoneme, Source) %>% distinct()
    distinct.segments <- phoneme.source %>% group_by(Phoneme) %>% summarize(Sources=tolower(paste(Source, collapse=";"))) %>% arrange(Phoneme)

    ## `summarise()` ungrouping output (override with `.groups` argument)

    expect_equal(nrow(distinct.segments), 3164)
    head(distinct.segments)

    ## # A tibble: 6 x 2
    ##   Phoneme Sources           
    ##   <chr>   <chr>             
    ## 1 ˥       spa;aa;ph;gm;ra;uz
    ## 2 ˥̰       spa               
    ## 3 ˥˦      spa;ra            
    ## 4 ˥˧      ph;gm             
    ## 5 ˥˧̰      ph                
    ## 6 ˥˧˥     spa

    # write.csv(distinct.segments, file="distinct-segments.csv", row.names = F)

Check for ambigously placed segments, e.g. ring above and below (awful
hack here because copy and pasting diacritics in R studio is
problematic).

    rings.below <- distinct.segments %>% filter(grepl("̥", Phoneme))
    rings.above <- distinct.segments %>% filter(grepl("̊", Phoneme))
    rings <- rbind(rings.above, rings.below)
    rings <- rings %>% arrange(Phoneme)
    write.csv(rings, file="rings.csv", row.names = F)

    kable(rings)

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
| d̺̥       | ea                                 |
| d̪̥       | ea                                 |
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
| l̪̥\|l̥    | upsid                              |
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
| n̠̥       | upsid                              |
| n̪̥       | upsid;ea                           |
| n̥       | spa;upsid;ph;gm;saphon;uz;ea       |
| n̪̥\|n̥    | upsid                              |
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
| ɲ̥ɲ      | uz                                 |
| ɳ̥       | upsid                              |
| ɳʈr̠̥     | spa                                |
| ŋ̊       | uz;ea                              |
| ŋ̥       | spa;upsid;ph;gm;saphon;uz;ea       |
| ŋ̥ʲ      | spa;upsid                          |
| ŋ̥m̥      | upsid                              |
| ŋ̥ŋ      | uz                                 |
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
| r̪̥\|r̥    | upsid                              |
| R̪̥\|R̥    | upsid                              |
| r̥ː      | ea                                 |
| r̥ʲ      | ea                                 |
| r̥ʲː     | ea                                 |
| ɹ̥       | ea                                 |
| ɹ̪̥\|ɹ̥    | upsid                              |
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

Check for ambigously placed segments, e.g. ring above and below (awful
hack here because copy and pasting diacritics in R studio is
problematic).

    syllabic.below <- distinct.segments %>% filter(grepl("̩", Phoneme))
    syllabic.above <- distinct.segments %>% filter(grepl("̍", Phoneme))
    syllabic <- rbind(syllabic.above, syllabic.below)
    syllabic <- syllabic %>% arrange(Phoneme)
    write.csv(syllabic, file="syllabic.csv", row.names = F)

    kable(syllabic)

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
