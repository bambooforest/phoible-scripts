PHOIBLE data tests
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
library(knitr)
library(testthat)
```

``` r
# Get the bibtex data
path <- 'https://raw.githubusercontent.com/phoible/dev/master/data/phoible-references.bib'
# path <- '../data/raw2019/inventory_biblio.txt'
bib <- bib2df(path)
head(bib)
```

    ## # A tibble: 6 x 130
    ##   CATEGORY BIBTEXKEY ADDRESS ANNOTE AUTHOR BOOKTITLE CHAPTER CROSSREF
    ##   <chr>    <chr>     <chr>   <chr>  <list> <chr>     <chr>   <chr>   
    ## 1 ARTICLE  Schadebe… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 2 MISC     Round2019 <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 3 MISC     Nikolaev… <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 4 MISC     saphon    <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 5 MISC     spa1979   <NA>    <NA>   <chr … <NA>      <NA>    <NA>    
    ## 6 BOOK     maddieso… Cambri… <NA>   <chr … <NA>      <NA>    <NA>    
    ## # ... with 122 more variables: EDITION <chr>, EDITOR <list>,
    ## #   HOWPUBLISHED <chr>, INSTITUTION <chr>, JOURNAL <chr>, KEY <chr>,
    ## #   MONTH <chr>, NOTE <chr>, NUMBER <chr>, ORGANIZATION <chr>,
    ## #   PAGES <chr>, PUBLISHER <chr>, SCHOOL <chr>, SERIES <chr>, TITLE <chr>,
    ## #   TYPE <chr>, VOLUME <chr>, YEAR <chr>, DATE.ADDED <chr>,
    ## #   DATE.MODIFIED <chr>, BDSK.URL.1 <chr>, URL <chr>, CATALOGUE.URL <chr>,
    ## #   ISBN <chr>, AIATSIS_CALLNUMBER <chr>, AIATSIS_CODE <chr>,
    ## #   AIATSIS_REFERENCE_LANGUAGE <chr>, CLASS_LOC <chr>,
    ## #   DOCUMENT_TYPE <chr>, FN <chr>, HHTYPE <chr>, INLG <chr>, LGCODE <chr>,
    ## #   MACRO_AREA <chr>, MPI_EVA_LIBRARY_SHELF <chr>, MPIFN <chr>,
    ## #   OCLC <chr>, OZBIB_ID <chr>, OZBIBREFTYPE <chr>, SRC <chr>,
    ## #   SUBJECT_HEADINGS <chr>, DOI <chr>, OZBIBNOTE <chr>, BDSK.URL.2 <chr>,
    ## #   THESISTYPE <chr>, CALL_NUMBER <chr>, FNNOTE <chr>, ADDED <chr>,
    ## #   ASJP_NAME <chr>, ISO_CODE <chr>, KEYWORDS <chr>, MODIFIED <chr>,
    ## #   OLAC_FIELD <chr>, REFDB_ID <chr>, WALS_CODE <chr>, COUNTRY <chr>,
    ## #   SIL_ID <chr>, SUBJECT <chr>, LOCATION <chr>, OWNER <chr>,
    ## #   TIMESTAMP <chr>, SHORTTITLE <chr>, LANGNOTE <chr>, LGFAMILY <chr>,
    ## #   BWONOTE <chr>, ZURICHCODE <chr>, NOSHAREFN <chr>, LAST_CHANGED <chr>,
    ## #   GULDEMANN_LOCATION <chr>, MED <chr>, EXTRA_HASH <chr>, VARIETY <chr>,
    ## #   OTHER_EDITIONS <chr>, FILENAMES <chr>, ABSTRACT <chr>,
    ## #   SHELF_LOCATION <chr>, INLG_CODE <chr>, INTERNETARCHIVE_ID <chr>,
    ## #   SRCTRICKLE <chr>, LAPOLLANOTE <chr>, SEANOTE <chr>,
    ## #   TITLE_ENGLISH <chr>, ADVISER <chr>, DEGREE <chr>,
    ## #   DIGITAL_FORMATS <chr>, SOURCE <chr>, UMI_ID <chr>,
    ## #   AUTHOR_STATEMENT <chr>, REVIEW <chr>, PERMISSION <chr>, HAL_ID <chr>,
    ## #   HAL_VERSION <chr>, PDF <chr>, INVENTORYID <chr>, LANGUAGECODE <chr>,
    ## #   LANGUAGENAME <chr>, LANGUAGEVARIANTCODE <chr>, SQUIB <chr>,
    ## #   NOTES <chr>, CONFERENCE <chr>, …

``` r
index.bibtex <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-Bibtex.csv', header=T, stringsAsFactors = F)
head(index.bibtex)
```

    ##   InventoryID     BibtexKey Source             Filename
    ## 1           1       Cho1967    spa kor_SPA1979_phon.pdf
    ## 2           1    Martin1951    spa kor_SPA1979_phon.pdf
    ## 3           1    Martin1954    spa kor_SPA1979_phon.pdf
    ## 4           1 MartinLee1969    spa kor_SPA1979_phon.pdf
    ## 5           1       Kim1968    spa kor_SPA1979_phon.pdf
    ## 6           1       Kim1972    spa kor_SPA1979_phon.pdf

``` r
# Which bibtex keys are in the phoible bibtex index and NOT in the bibtex file
index.bibtex[which(!(index.bibtex$BibtexKey %in% bib$BIBTEXKEY)), ]
```

    ##      InventoryID            BibtexKey Source
    ## 353          201      NO SOURCE GIVEN  upsid
    ## 1569         896        jar_lukas1961     ph
    ## 2391        1701        Natarajan1985     ra
    ## 2392        1702        Manoharan1989     ra
    ## 2393        1703        Ravindran1974     ra
    ## 2394        1704       Gurubasave1972     ra
    ## 2395        1705          Abraham1985     ra
    ## 2396        1706          Goswami1978     ra
    ## 2397        1707          Saksena1971     ra
    ## 2398        1708      Swaminathan1972     ra
    ## 2399        1709           Rangan1975     ra
    ## 2400        1710         Upadhyay1975     ra
    ## 2401        1711     Bhattacharya1988     ra
    ## 2402        1712         Kulkarni1976     ra
    ## 2403        1713        Ramaswami1992     ra
    ## 2405        1715        Ramaswami1976     ra
    ## 2406        1716          Jaiswal1962     ra
    ## 2407        1717           Sharma1989     ra
    ## 2408        1718              KaptoND     ra
    ## 2409        1719           Sharma1989     ra
    ## 2410        1720       Ashirvadam1992     ra
    ## 2411        1721           Sharma1988     ra
    ## 2412        1722         DasGupta1963     ra
    ## 2413        1723          Barling1961     ra
    ## 2414        1724           Sharma1979     ra
    ## 2415        1725             Nair1979     ra
    ## 2416        1726      GnanasundaramND     ra
    ## 2417        1727            Chila1972     ra
    ## 2419        1729          RamaswamiND     ra
    ## 2420        1730        Perialwar1970     ra
    ## 2421        1731           Satish1990     ra
    ## 2422        1732           Matson1964     ra
    ## 2423        1733          Chauhan1992     ra
    ## 2424        1734        Upadhyaya1972     ra
    ## 2425        1735        Srinivasa1978     ra
    ## 2426        1735        Srinivasa1978     ra
    ## 2427        1736         Jeyapaul1987     ra
    ## 2428        1737           Pillai1978     ra
    ## 2429        1738           Handoo1973     ra
    ## 2430        1739  Natanasabapathy1976     ra
    ## 2432        1741         Nagaraja1990     ra
    ## 2433        1742            Kapfo1992     ra
    ## 2434        1743     Balakrishnan1976     ra
    ## 2435        1744          Emeneau1961     ra
    ## 2436        1745    Krishnamurthy1969     ra
    ## 2437        1746      Bhaskararao1980     ra
    ## 2438        1747            Katre1966     ra
    ## 2439        1748             Bhat1971     ra
    ## 2440        1748           Shetty1978     ra
    ## 2441        1749             Zide1960     ra
    ## 2442        1750          Subbiah1986     ra
    ## 2443        1751              Rao1979     ra
    ## 2444        1752       Gopabandhu1981     ra
    ## 2445        1753           Sharma1985     ra
    ## 2446        1754             Ekka1985     ra
    ## 2447        1755            Reddy1974     ra
    ## 2448        1756           Koshal1976     ra
    ## 2449        1757            Trail1970     ra
    ## 2450        1758            Sinha1966     ra
    ## 2451        1759          Acharya1975     ra
    ## 2452        1760          Weidert1975     ra
    ## 2453        1761            Davis1984     ra
    ## 2454        1762          Syamala1972     ra
    ## 2455        1763              Das1973     ra
    ## 2456        1764            Singh1975     ra
    ## 2457        1765         Giridhar1994     ra
    ## 2458        1766           Kelkar1958     ra
    ## 2459        1766         Berntsen1975     ra
    ## 2460        1767           Sastry1984     ra
    ## 2461        1768           Prasad1991     ra
    ## 2462        1769            Kosha1989     ra
    ## 2463        1770            Sinha1974     ra
    ## 2464        1770            Osada1992     ra
    ## 2465        1771   Krishnamoorthy1984     ra
    ## 2466        1772       Srivastava1962     ra
    ## 2468        1774     Bhattacharya1956     ra
    ## 2469        1775       Pattanayak1966     ra
    ## 2470        1775       Pattanayak1972     ra
    ## 2471        1776           GiridharND     ra
    ## 2472        1777           Burrow1953     ra
    ## 2473        1778           Sharma1982     ra
    ## 2474        1779           Burrow1970     ra
    ## 2475        1780            Dulai1980     ra
    ## 2476        1781        Fernandez1969     ra
    ## 2477        1782          Sukumar1971     ra
    ## 2478        1783      Suryakumari1991     ra
    ## 2479        1784         Sreedhar1976     ra
    ## 2480        1785      Rajapurohit1983     ra
    ## 2481        1786      Kubchandani1963     ra
    ## 2482        1787        Ramamurti1931     ra
    ## 2483        1788          RamaswamiND     ra
    ## 2484        1789     Arokianathan1980     ra
    ## 2485        1790   Bandyoppadhyay1980     ra
    ## 2486        1791     Venkateswara1972     ra
    ## 2487        1792       Thirumalai1972     ra
    ## 2488        1793         Saktivel1976     ra
    ## 2489        1794       Karapurkar1972     ra
    ## 2491        1796         Mohanlal1991     ra
    ## 2492        1797            Hasan1980     ra
    ## 2493        1798        Srinivasa1970     ra
    ## 2494        1799      Mallikarjun1982     ra
    ## 2495        1800        Srinivasa1976     ra
    ## 3119        2323        hashimoto1973     ea
    ## 3415        2596 spence1987phonologie     ea
    ##                                               Filename
    ## 353  http://web.phonetik.uni-frankfurt.de/L/L8362.html
    ## 1569                            jar_lukas1961_phon.pdf
    ## 2391                                 mrr_Ramaswami.pdf
    ## 2392                         apq&njm_Ramaswami1999.pdf
    ## 2393                         apq&njm_Ramaswami1999.pdf
    ## 2394                         njo&apt_Ramaswami1999.pdf
    ## 2395                         njo&apt_Ramaswami1999.pdf
    ## 2396                         asm&awa_Ramaswami1999.pdf
    ## 2397                         asm&awa_Ramaswami1999.pdf
    ## 2398                         bfq&bft_Ramaswami1999.pdf
    ## 2399                         bfq&bft_Ramaswami1999.pdf
    ## 2400                         lmn&ben_Ramaswami1999.pdf
    ## 2401                         lmn&ben_Ramaswami1999.pdf
    ## 2402                         bhb&unr_Ramaswami1999.pdf
    ## 2403                         bhb&unr_Ramaswami1999.pdf
    ## 2405                         bwo&bkk_Ramaswami1999.pdf
    ## 2406                         bns&cdn_Ramaswami1999.pdf
    ## 2407                         bns&cdn_Ramaswami1999.pdf
    ## 2408                         nri&drd_Ramaswami1999.pdf
    ## 2409                         nri&drd_Ramaswami1999.pdf
    ## 2410                         gaq&gda_Ramaswami1999.pdf
    ## 2411                         gaq&gda_Ramaswami1999.pdf
    ## 2412                         adl&grt_Ramaswami1999.pdf
    ## 2413                         adl&grt_Ramaswami1999.pdf
    ## 2414                         gju&guj_Ramaswami1999.pdf
    ## 2415                         gju&guj_Ramaswami1999.pdf
    ## 2416                         gbj&hlb_Ramaswami1999.pdf
    ## 2417                         gbj&hlb_Ramaswami1999.pdf
    ## 2419                         hin&hoc_Ramaswami1999.pdf
    ## 2420                         iru&jns_Ramaswami1999.pdf
    ## 2421                         iru&jns_Ramaswami1999.pdf
    ## 2422                         jun&xnr_Ramaswami1999.pdf
    ## 2423                         jun&xnr_Ramaswami1999.pdf
    ## 2424                         kan&saz_Ramaswami1999.pdf
    ## 2425                         kan&saz_Ramaswami1999.pdf
    ## 2426                       varma_kanigkergotti1978.pdf
    ## 2427                         mjw&iru_Ramaswami1999.pdf
    ## 2428                         mjw&iru_Ramaswami1999.pdf
    ## 2429                         kas&xuj_Ramaswami1999.pdf
    ## 2430                         kas&xuj_Ramaswami1999.pdf
    ## 2432                         khr&kha_Ramaswami1999.pdf
    ## 2433                         nkh&kfa_Ramaswami1999.pdf
    ## 2434                         nkh&kfa_Ramaswami1999.pdf
    ## 2435                         kfb&kfc_Ramaswami1999.pdf
    ## 2436                         kfb&kfc_Ramaswami1999.pdf
    ## 2437                         gau&knn_Ramaswami1999.pdf
    ## 2438                         gau&knn_Ramaswami1999.pdf
    ## 2439                         kfd&kfq_Ramaswami1999.pdf
    ## 2440                         kfd&kfq_Ramaswami1999.pdf
    ## 2441                         kfd&kfq_Ramaswami1999.pdf
    ## 2442                         kfe&kff_Ramaswami1999.pdf
    ## 2443                         kfe&kff_Ramaswami1999.pdf
    ## 2444                         kxu&kfy_Ramaswami1999.pdf
    ## 2445                         kxu&kfy_Ramaswami1999.pdf
    ## 2446                         kru&kxv_Ramaswami1999.pdf
    ## 2447                         kru&kxv_Ramaswami1999.pdf
    ## 2448                         lbj&lmn_Ramaswami1999.pdf
    ## 2449                         lbj&lmn_Ramaswami1999.pdf
    ## 2450                         lep&njh_Ramaswami1999.pdf
    ## 2451                         lep&njh_Ramaswami1999.pdf
    ## 2452                         lus&mai_Ramaswami1999.pdf
    ## 2453                         lus&mai_Ramaswami1999.pdf
    ## 2454                         mal&mjt_Ramaswami1999.pdf
    ## 2455                         mal&mjt_Ramaswami1999.pdf
    ## 2456                         mni&nbi_Ramaswami1999.pdf
    ## 2457                         mni&nbi_Ramaswami1999.pdf
    ## 2458                         mar&mxj_Ramaswami1999.pdf
    ## 2459                         mar&mxj_Ramaswami1999.pdf
    ## 2460                         mar&mxj_Ramaswami1999.pdf
    ## 2461                         mrg&nmo_Ramaswami1999.pdf
    ## 2462                         mrg&nmo_Ramaswami1999.pdf
    ## 2463                         unr&nit_Ramaswami1999.pdf
    ## 2464                         unr&nit_Ramaswami1999.pdf
    ## 2465                         unr&nit_Ramaswami1999.pdf
    ## 2466                         nep&ncb_Ramaswami1999.pdf
    ## 2468                         gdb&ori_Ramaswami1999.pdf
    ## 2469                         gdb&ori_Ramaswami1999.pdf
    ## 2470                         gdb&ori_Ramaswami1999.pdf
    ## 2471                         pck&pci_Ramaswami1999.pdf
    ## 2472                         pck&pci_Ramaswami1999.pdf
    ## 2473                         lae&peg_Ramaswami1999.pdf
    ## 2474                         lae&peg_Ramaswami1999.pdf
    ## 2475                         pan&bfw_Ramaswami1999.pdf
    ## 2476                         pan&bfw_Ramaswami1999.pdf
    ## 2477                         san&sat_Ramaswami1999.pdf
    ## 2478                         san&sat_Ramaswami1999.pdf
    ## 2479                         nsm&scl_Ramaswami1999.pdf
    ## 2480                         nsm&scl_Ramaswami1999.pdf
    ## 2481                         snd&srb_Ramaswami1999.pdf
    ## 2482                         snd&srb_Ramaswami1999.pdf
    ## 2483                         tam&nmf_Ramaswami1999.pdf
    ## 2484                         tam&nmf_Ramaswami1999.pdf
    ## 2485                         nst&tel_Ramaswami1999.pdf
    ## 2486                         nst&tel_Ramaswami1999.pdf
    ## 2487                         tcz&tcx_Ramaswami1999.pdf
    ## 2488                         tcz&tcx_Ramaswami1999.pdf
    ## 2489                         trp&tcy_Ramaswami1999.pdf
    ## 2491                         url&urd_Ramaswami1999.pdf
    ## 2492                         url&urd_Ramaswami1999.pdf
    ## 2493                         vaa&yea_Ramaswami1999.pdf
    ## 2494                         vaa&yea_Ramaswami1999.pdf
    ## 2495                             yeu_Ramaswami1999.pdf
    ## 3119                                              <NA>
    ## 3415                                              <NA>

<!--
## load(url('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.RData'))
-->
