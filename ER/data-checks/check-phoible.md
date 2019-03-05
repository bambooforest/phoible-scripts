Check PHOIBLE references
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(bib2df)
library(dplyr)
library(knitr)
library(testthat)
```

Data
====

``` r
# phoible bibtex references
ph.refs <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-Bibtex.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
expect_equal(nrow(ph.refs), 3831)
head(ph.refs)
```

    ##   InventoryID     BibtexKey Source
    ## 1           1       Cho1967    spa
    ## 2           1    Martin1951    spa
    ## 3           1    Martin1954    spa
    ## 4           1 MartinLee1969    spa
    ## 5           1       Kim1968    spa
    ## 6           1       Kim1972    spa

``` r
# phoible bib file
path <- 'https://raw.githubusercontent.com/phoible/dev/master/data/phoible-references.bib'
ph.bib <- bib2df(path)
```

    ## Warning in bib2df_tidy(bib, separate_names): NAs introduced by coercion

    ## Column `YEAR` contains character strings.
    ##               No coercion to numeric applied.

``` r
# Strange that these aren't equal.
expect_equal(nrow(ph.bib), 2835)
expect_equal(nrow(ph.bib %>% select(BIBTEXKEY) %>% distinct()), 2834)
```

Everything in everything?
=========================

``` r
# Which InventoryID-Bibtex.tsv bibtex keys not in phoible-references.bib?
ph.refs[which(!(ph.refs$BibtexKey %in% ph.bib$BIBTEXKEY)),]
```

    ##      InventoryID            BibtexKey Source
    ## 353          201      NO SOURCE GIVEN  upsid
    ## 1569         896        jar_lukas1961     ph
    ## 2376        1701        Natarajan1985     ra
    ## 2377        1702        Manoharan1989     ra
    ## 2378        1703        Ravindran1974     ra
    ## 2379        1704       Gurubasave1972     ra
    ## 2380        1705          Abraham1985     ra
    ## 2381        1706          Goswami1978     ra
    ## 2382        1707          Saksena1971     ra
    ## 2383        1708      Swaminathan1972     ra
    ## 2384        1709           Rangan1975     ra
    ## 2385        1710         Upadhyay1975     ra
    ## 2386        1711     Bhattacharya1988     ra
    ## 2387        1712         Kulkarni1976     ra
    ## 2388        1713        Ramaswami1992     ra
    ## 2390        1715        Ramaswami1976     ra
    ## 2391        1716          Jaiswal1962     ra
    ## 2392        1717           Sharma1989     ra
    ## 2393        1718              KaptoND     ra
    ## 2394        1719           Sharma1989     ra
    ## 2395        1720       Ashirvadam1992     ra
    ## 2396        1721           Sharma1988     ra
    ## 2397        1722         DasGupta1963     ra
    ## 2398        1723          Barling1961     ra
    ## 2399        1724           Sharma1979     ra
    ## 2400        1725             Nair1979     ra
    ## 2401        1726      GnanasundaramND     ra
    ## 2402        1727            Chila1972     ra
    ## 2404        1729          RamaswamiND     ra
    ## 2405        1730        Perialwar1970     ra
    ## 2406        1731           Satish1990     ra
    ## 2407        1732           Matson1964     ra
    ## 2408        1733          Chauhan1992     ra
    ## 2409        1734        Upadhyaya1972     ra
    ## 2410        1735        Srinivasa1978     ra
    ## 2411        1736         Jeyapaul1987     ra
    ## 2412        1737           Pillai1978     ra
    ## 2413        1738           Handoo1973     ra
    ## 2414        1739  Natanasabapathy1976     ra
    ## 2416        1741         Nagaraja1990     ra
    ## 2417        1742            Kapfo1992     ra
    ## 2418        1743     Balakrishnan1976     ra
    ## 2419        1744          Emeneau1961     ra
    ## 2420        1745    Krishnamurthy1969     ra
    ## 2421        1746      Bhaskararao1980     ra
    ## 2422        1747            Katre1966     ra
    ## 2423        1748             Bhat1971     ra
    ## 2424        1748           Shetty1978     ra
    ## 2425        1749             Zide1960     ra
    ## 2426        1750          Subbiah1986     ra
    ## 2427        1751              Rao1979     ra
    ## 2428        1752       Gopabandhu1981     ra
    ## 2429        1753           Sharma1985     ra
    ## 2430        1754             Ekka1985     ra
    ## 2431        1755            Reddy1974     ra
    ## 2432        1756           Koshal1976     ra
    ## 2433        1757            Trail1970     ra
    ## 2434        1758            Sinha1966     ra
    ## 2435        1759          Acharya1975     ra
    ## 2436        1760          Weidert1975     ra
    ## 2437        1761            Davis1984     ra
    ## 2438        1762          Syamala1972     ra
    ## 2439        1763              Das1973     ra
    ## 2440        1764            Singh1975     ra
    ## 2441        1765         Giridhar1994     ra
    ## 2442        1766           Kelkar1958     ra
    ## 2443        1766         Berntsen1975     ra
    ## 2444        1767           Sastry1984     ra
    ## 2445        1768           Prasad1991     ra
    ## 2446        1769            Kosha1989     ra
    ## 2447        1770            Sinha1974     ra
    ## 2448        1770            Osada1992     ra
    ## 2449        1771   Krishnamoorthy1984     ra
    ## 2450        1772       Srivastava1962     ra
    ## 2452        1774     Bhattacharya1956     ra
    ## 2453        1775       Pattanayak1966     ra
    ## 2454        1775       Pattanayak1972     ra
    ## 2455        1776           GiridharND     ra
    ## 2456        1777           Burrow1953     ra
    ## 2457        1778           Sharma1982     ra
    ## 2458        1779           Burrow1970     ra
    ## 2459        1780            Dulai1980     ra
    ## 2460        1781        Fernandez1969     ra
    ## 2461        1782          Sukumar1971     ra
    ## 2462        1783      Suryakumari1991     ra
    ## 2463        1784         Sreedhar1976     ra
    ## 2464        1785      Rajapurohit1983     ra
    ## 2465        1786      Kubchandani1963     ra
    ## 2466        1787        Ramamurti1931     ra
    ## 2467        1788          RamaswamiND     ra
    ## 2468        1789     Arokianathan1980     ra
    ## 2469        1790   Bandyoppadhyay1980     ra
    ## 2470        1791     Venkateswara1972     ra
    ## 2471        1792       Thirumalai1972     ra
    ## 2472        1793         Saktivel1976     ra
    ## 2473        1794       Karapurkar1972     ra
    ## 2475        1796         Mohanlal1991     ra
    ## 2476        1797            Hasan1980     ra
    ## 2477        1798        Srinivasa1970     ra
    ## 2478        1799      Mallikarjun1982     ra
    ## 2479        1800        Srinivasa1976     ra
    ## 3103        2323        hashimoto1973     ea
    ## 3399        2596 spence1987phonologie     ea
    ## 3529        2719               475849     er
    ## 3534        2724               475849     er
    ## 3665        2851               477054     er
    ## 3667        2853               473255     er
    ## 3708        2894                 <NA>     er
    ## 3743        2929                 <NA>     er
    ## 3828        3013                 6451     er

``` r
# Which phoible-references.bib bibtex keys not in InventoryID-Bibtex.tsv?
ph.bib[which(!(ph.bib$BIBTEXKEY %in% ph.refs$BibtexKey)),] %>% select(BIBTEXKEY)
```

    ## # A tibble: 112 x 1
    ##    BIBTEXKEY           
    ##    <chr>               
    ##  1 316915              
    ##  2 468946              
    ##  3 469869              
    ##  4 471471              
    ##  5 haiman1992rhaeto    
    ##  6 spence1987consonants
    ##  7 zai_pickett2011     
    ##  8 1701_Natarajan1985  
    ##  9 1702_Manoharan1989  
    ## 10 1703_Ravindran1974  
    ## # ... with 102 more rows

``` r
# TODO: Needs some fixing, especially in `ra` inventories! But doesn't necesarily mean we need to remove the non-mapping bibtex keys from the phoible bib file.
```
