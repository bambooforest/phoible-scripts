Update phoible index with ISO codes from Glottolog data
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(testthat)
library(knitr)
```

Check and assign ISO codes
==========================

``` r
# Get phoible index (TODO: update to online version when recent PRs are merged)
# index <- read.csv('https://raw.githubusercontent.com/phoible/dev/master/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors=F)
index <- read.csv('../../phoible/mappings/InventoryID-LanguageCodes.csv', header=T, stringsAsFactors = F)
expect_equal(nrow(index), 3020)
head(index)
```

    ##   InventoryID LanguageCode Glottocode LanguageName Source
    ## 1           1          kor   kore1280       Korean    spa
    ## 2           2          ket   kett1243          Ket    spa
    ## 3           3          lbe   lakk1252          Lak    spa
    ## 4           4          kbd   kaba1278    Kabardian    spa
    ## 5           5          kat   nucl1302     Georgian    spa
    ## 6           6          bsk   buru1296   Burushaski    spa

``` r
# Identify the parent languages of dialects
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/glottolog_languoid.csv.zip
languoids <- read.csv('glottolog_languoid.csv/languoid.csv', header=T, stringsAsFactors=F)
languoids <- languoids %>% select(id, family_id, parent_id, name, level, status, iso639P3code)
```

``` r
# Merge in the languiods data for our Glottolog codes
index <- left_join(index, languoids, by=c("Glottocode"="id"))
```

``` r
# First check that all present phoible Glottocodes are valid -- currently 2 <NA>s
table(index$Glottocode %in% languoids$id)
```

    ## 
    ## FALSE  TRUE 
    ##     2  3018

``` r
index[which(!(index$Glottocode %in% languoids$id)),]
```

    ##      InventoryID LanguageCode Glottocode                  LanguageName
    ## 2281        2281         <NA>       <NA> Modern Aramaic (Northeastern)
    ## 2729        2729         <NA>       <NA>                     Djindewal
    ##      Source family_id parent_id name level status iso639P3code
    ## 2281     ea      <NA>      <NA> <NA>  <NA>   <NA>         <NA>
    ## 2729     er      <NA>      <NA> <NA>  <NA>   <NA>         <NA>

``` r
# These are phoible inventories where our ISO codes don't match Glottolog codes (14 mismatches)
expect_equal(nrow(index %>% filter(LanguageCode != iso639P3code) %>% filter(iso639P3code != "")), 0)
index %>% filter(LanguageCode != iso639P3code) %>% filter(iso639P3code != "")
```

    ##  [1] InventoryID  LanguageCode Glottocode   LanguageName Source      
    ##  [6] family_id    parent_id    name         level        status      
    ## [11] iso639P3code
    ## <0 rows> (or 0-length row.names)

Which inventories don't have a Glottocode? Assign them from Glottolog
=====================================================================

``` r
missing.iso <- index %>% filter(iso639P3code == "")
dim(missing.iso) # 123 missing
```

    ## [1] 123  11

``` r
head(missing.iso)
```

    ##   InventoryID LanguageCode Glottocode LanguageName Source family_id
    ## 1          40          khl   kali1299       Kaliai    spa  aust1307
    ## 2         136          xtc   katc1250       Katcha    spa  kadu1256
    ## 3         155          naq   nama1265         Nama    spa  khoe1240
    ## 4         871          kpr   kora1295       Korafe     ph  nucl1709
    ## 5         875          enb   endo1242         Endo     ph  nilo1247
    ## 6         885          wya   huro1249        Huron     ph  iroq1247
    ##   parent_id   name   level status iso639P3code
    ## 1  lusi1240 Kaliai dialect   safe             
    ## 2  katc1249 Katcha dialect   safe             
    ## 3  nama1264   Nama dialect   safe             
    ## 4  kora1294 Korafe dialect   safe             
    ## 5  mark1255   Endo dialect   safe             
    ## 6  wyan1247  Huron dialect   safe

``` r
# Merge in the parents and their glottocodes
parent.index <- left_join(missing.iso, languoids, by=c("parent_id"="id"))
head(parent.index)
```

    ##   InventoryID LanguageCode Glottocode LanguageName Source family_id.x
    ## 1          40          khl   kali1299       Kaliai    spa    aust1307
    ## 2         136          xtc   katc1250       Katcha    spa    kadu1256
    ## 3         155          naq   nama1265         Nama    spa    khoe1240
    ## 4         871          kpr   kora1295       Korafe     ph    nucl1709
    ## 5         875          enb   endo1242         Endo     ph    nilo1247
    ## 6         885          wya   huro1249        Huron     ph    iroq1247
    ##   parent_id name.x level.x status.x iso639P3code.x family_id.y parent_id.y
    ## 1  lusi1240 Kaliai dialect     safe                   aust1307    kali1298
    ## 2  katc1249 Katcha dialect     safe                   kadu1256    katc1251
    ## 3  nama1264   Nama dialect     safe                   khoe1240    nort3245
    ## 4  kora1294 Korafe dialect     safe                   nucl1709    gaen1235
    ## 5  mark1255   Endo dialect     safe                   nilo1247    nort3239
    ## 6  wyan1247  Huron dialect     safe                   iroq1247    nort2947
    ##                name.y  level.y              status.y iso639P3code.y
    ## 1                Lusi language                  safe            khl
    ## 2 Katcha-Kadugli-Miri language            vulnerable            xtc
    ## 3      Nama (Namibia) language            vulnerable            naq
    ## 4        Korafe-Yegha language                  safe            kpr
    ## 5           Markweeta language definitely endangered            enb
    ## 6             Wyandot language critically endangered            wya

``` r
# Which phoible NAs can get assign when we merge in the parents and their ISO codes
parent.index %>% filter(is.na(LanguageCode)) %>% filter(iso639P3code.y != "")
```

    ##  [1] InventoryID    LanguageCode   Glottocode     LanguageName  
    ##  [5] Source         family_id.x    parent_id      name.x        
    ##  [9] level.x        status.x       iso639P3code.x family_id.y   
    ## [13] parent_id.y    name.y         level.y        status.y      
    ## [17] iso639P3code.y
    ## <0 rows> (or 0-length row.names)

``` r
# Which codes don't match because we've added them by hand -- 2689 Gudanji was purposedly given [wmb] for Wambaya (two nodes up in the Glottolog 3.3 tree)
parent.index[which(parent.index$LanguageCode != parent.index$iso639P3code.y), ] %>% select(InventoryID, LanguageCode, Glottocode, iso639P3code.y, LanguageName)
```

    ##    InventoryID LanguageCode Glottocode iso639P3code.y LanguageName
    ## 66        2689          wmb   guda1243                     Gudanji

``` r
# No phoible language codes should be blank
expect_equal(nrow(parent.index %>% filter(LanguageCode=="")), 0)
```

``` r
# Which phoible language codes are NA (35)
parent.index %>% filter(is.na(LanguageCode)) %>% select(InventoryID, LanguageCode, Glottocode, iso639P3code.y, LanguageName, level.x, level.y)
```

    ##    InventoryID LanguageCode Glottocode iso639P3code.y
    ## 1         2143         <NA>   pisa1245               
    ## 2         2352         <NA>   lizu1234               
    ## 3         2388         <NA>   east2773               
    ## 4         2420         <NA>   zhon1235               
    ## 5         2434         <NA>   vach1239               
    ## 6         2450         <NA>   fore1274               
    ## 7         2544         <NA>   nucl1729               
    ## 8         2691         <NA>   mink1237           <NA>
    ## 9         2692         <NA>   ngum1253               
    ## 10        2714         <NA>   guwa1244               
    ## 11        2718         <NA>   warr1257               
    ## 12        2727         <NA>   dhud1236               
    ## 13        2748         <NA>   mith1236               
    ## 14        2773         <NA>   cola1237               
    ## 15        2778         <NA>   yari1243               
    ## 16        2782         <NA>   west2443               
    ## 17        2783         <NA>   djad1246               
    ## 18        2792         <NA>   kera1256               
    ## 19        2793         <NA>   lowe1402               
    ## 20        2794         <NA>   ngin1247               
    ## 21        2818         <NA>   gudj1237               
    ## 22        2877         <NA>   pall1243               
    ## 23        2882         <NA>   kawa1290               
    ## 24        2883         <NA>   kawa1290               
    ## 25        2907         <NA>   wala1263               
    ## 26        2911         <NA>   tyan1235               
    ## 27        2913         <NA>   luth1234               
    ## 28        2914         <NA>   mbiy1238               
    ## 29        2916         <NA>   ngko1236               
    ## 30        2920         <NA>   yadh1237               
    ## 31        2921         <NA>   yinw1236               
    ## 32        2946         <NA>   bula1255               
    ## 33        2956         <NA>   yulp1239               
    ## 34        2988         <NA>   west2443               
    ## 35        2999         <NA>   sout2770               
    ##             LanguageName  level.x  level.y
    ## 1               Pisamira language   family
    ## 2                   Lizu language   family
    ## 3          Dolakha Newar language   family
    ## 4         Zhongu Tibetan language   family
    ## 5  Eastern Khanty (Vakh)  dialect language
    ## 6          Forest Nenets language   family
    ## 7      Tamang (Dhankuta)   family   family
    ## 8                 Minkin language     <NA>
    ## 9               Ngumbarl language   family
    ## 10                 Guwar language   family
    ## 11           Warrnambool language   family
    ## 12             Dhudhuroa language   family
    ## 13               Mithaka language   family
    ## 14             Kolakngat language   family
    ## 15             Yari-Yari  dialect language
    ## 16     East Djadjawurung language   family
    ## 17           Jardwadjali  dialect language
    ## 18               Keramin  dialect language
    ## 19             Ngayawang language   family
    ## 20              Ngintait  dialect language
    ## 21                Gudjal language   family
    ## 22      Pallanganmiddang language   family
    ## 23         Ogh Awarrangg language   family
    ## 24            Ogh Unyjan language   family
    ## 25             Walangama language   family
    ## 26          Thaynakwithi language   family
    ## 27               Luthigh  dialect language
    ## 28               Mbiywom language   family
    ## 29                Ngkoth language   family
    ## 30            Yadhaykenu language   family
    ## 31                Yinwum language   family
    ## 32               Bularnu language   family
    ## 33             Yulparija language   family
    ## 34     West Djadjawurung language   family
    ## 35              Ngunawal language   family

``` r
# What can be assigned by look up? I.e., what's NA as phoible ISO code but there exists something one level up in Glottolo
parent.index %>% filter(is.na(LanguageCode)) %>% filter(iso639P3code.y != "") %>% select(InventoryID, LanguageCode, Glottocode, iso639P3code.y, LanguageName)
```

    ## [1] InventoryID    LanguageCode   Glottocode     iso639P3code.y
    ## [5] LanguageName  
    ## <0 rows> (or 0-length row.names)

Looking at general categories
=============================

``` r
# How many levels do we have in the index?
table(index$level)
```

    ## 
    ##  dialect   family language 
    ##      146        6     2866

``` r
# These are the family level glottolog codes in the phoible index. TODO: check and update if ncessary
families <- index %>% filter(level=="family") %>% select(InventoryID, LanguageName, Glottocode, LanguageCode, iso639P3code, Source)
nrow(families)
```

    ## [1] 6

``` r
families
```

    ##   InventoryID        LanguageName Glottocode LanguageCode iso639P3code
    ## 1         488              NEPALI   east1436          nep          nep
    ## 2        1208             Jiarong   jiar1240          jya          jya
    ## 3        1398               Dinka   dink1262          din          din
    ## 4        2257   Caodeng rGyalrong   jiar1240          jya          jya
    ## 5        2464 Mongolian (Khalkha)   mong1331          mon          mon
    ## 6        2544   Tamang (Dhankuta)   nucl1729         <NA>             
    ##   Source
    ## 1  upsid
    ## 2     ph
    ## 3     gm
    ## 4     ea
    ## 5     ea
    ## 6     ea

``` r
# These are the dialects in the phoible index
dialects <- index %>% filter(level=="dialect") %>% select(InventoryID, LanguageName, Glottocode, LanguageCode, iso639P3code, Source)
nrow(dialects)
```

    ## [1] 146

``` r
head(dialects)
```

    ##   InventoryID LanguageName Glottocode LanguageCode iso639P3code Source
    ## 1          40       Kaliai   kali1299          khl                 spa
    ## 2         136       Katcha   katc1250          xtc                 spa
    ## 3         155         Nama   nama1265          naq                 spa
    ## 4         159    Norwegian   norw1259          nob          nob    spa
    ## 5         499    NORWEGIAN   norw1259          nob          nob  upsid
    ## 6         705         ẹzaa   ezaa1238          eza          eza     aa

``` r
# This should be only iso639P3code that are empty
dialects %>% filter(LanguageCode!=iso639P3code)
```

    ##    InventoryID                   LanguageName Glottocode LanguageCode
    ## 1           40                         Kaliai   kali1299          khl
    ## 2          136                         Katcha   katc1250          xtc
    ## 3          155                           Nama   nama1265          naq
    ## 4          871                         Korafe   kora1295          kpr
    ## 5          875                           Endo   endo1242          enb
    ## 6          885                          Huron   huro1249          wya
    ## 7          916                           Kuay   kuay1244          kdt
    ## 8          947                         Mvumbo   mvum1238          nmg
    ## 9          973                        Mianmin   mian1257          mpt
    ## 10         996                        Saanich   saan1246          str
    ## 11        1097                           Karo   karo1306          arr
    ## 12        1102                         Bikele   bike1242          biw
    ## 13        1192                       Bilinara   bili1250          nbj
    ## 14        1276                       Ikalanga   ikal1242          kck
    ## 15        1318                           Copi   copi1238          cce
    ## 16        1322                          Mmani   mman1238          buy
    ## 17        1325                         Frafra   fraf1238          gur
    ## 18        1382                            Oko   okoo1245          oks
    ## 19        1401                        Besleri   besl1239          hna
    ## 20        1456                          Soddo   sodd1242          gru
    ## 21        1460                           Ezha   ezha1238          sgw
    ## 22        1461                          Chaha   chah1248          sgw
    ## 23        1462                          Gumer   gume1239          sgw
    ## 24        1480                          Zayse   zays1236          zay
    ## 25        1523                          Efutu   efut1241          afu
    ## 26        1591                          Kambe   kamb1298          nyf
    ## 27        1592                          Kauma   kaum1238          nyf
    ## 28        1625                        Moghamo   mogh1246          mgo
    ## 29        1633                           Pana   pana1294          pnz
    ## 30        1701                      Abujmaria   abuj1237          mrr
    ## 31        1768                         Mising   miny1240          mrg
    ## 32        1771                          Naiki   naik1250          nit
    ## 33        1858                         Miraña   mira1254          boa
    ## 34        1994                      Kithaulhu   khit1238          nab
    ## 35        2015                        Shipibo   ship1255          shp
    ## 36        2092                           Karo   karo1306          arr
    ## 37        2225                        Kedayan   keda1250          kxd
    ## 38        2268            Beserman (Šamardan)   bese1243          udm
    ## 39        2298                           Puxi   puxi1242          jih
    ## 40        2332            Northwestern Pashto   nort2647          pbu
    ## 41        2381                Zuberoan Basque   basq1250          eus
    ## 42        2491                  Myeik Burmese   merg1238          tvn
    ## 43        2537                       Khalkhal   khal1271          tks
    ## 44        2586 Southwestern Pashto (Kandahar)   sout2436          pbt
    ## 45        2590                  Digor Ossetic   digo1242          oss
    ## 46        2634                  Murrinh-patha   murr1259          mwf
    ## 47        2656               Gun-Dedjnjenghmi   gund1246          gup
    ## 48        2657                    Gun-Djeihmi   gund1246          gup
    ## 49        2658                           Kune   gune1238          gup
    ## 50        2659                       Kuninjku   mura1269          gup
    ## 51        2660                      Kunwinjku   guma1252          gup
    ## 52        2661                         Mayali   naia1238          gup
    ## 53        2670                          Garig   gari1254          ilg
    ## 54        2671                          Ilgar   ilga1238          ilg
    ## 55        2684                     Ngaliwurru   ngal1294          djd
    ## 56        2688                       Binbinka   binb1242          wmb
    ## 57        2689                        Gudanji   guda1243          wmb
    ## 58        2703             Eastern Anmatyerre   east2380          amx
    ## 59        2704             Western Anmatyerre   west2442          amx
    ## 60        2706               Central Arrernte   mpar1238          aer
    ## 61        2712                     Bundjalung   yugu1249          xjb
    ## 62        2713                        Gidabal   gida1240          gih
    ## 63        2715                     Minjungbal   minj1242          xjb
    ## 64        2716                       Waalubal   waal1238          bdy
    ## 65        2722                       Wayilwan   wayi1238          wyb
    ## 66        2724                     Wiriyaraay   wirr1237          kld
    ## 67        2725                    Yuwaalaraay   yuwa1242          kld
    ## 68        2726                     Yuwaliyaay   yuwa1243          kld
    ## 69        2751                      Karangura   kara1508          nmv
    ## 70        2752                        Ngamini   ngam1284          nmv
    ## 71        2851                      Bilinarra   bili1250          nbj
    ## 72        2852                      Wanyjirra   wany1244          gue
    ## 73        2855                        Malngin   maln1239          gue
    ## 74        2867                       Kaniyang   kani1276          nys
    ## 75        2924                          Olkol   ulku1238          kjn
    ## 76        2930                      Oykangand   oyka1239          kjn
    ## 77        2941                      Gabi-Gabi   gabi1248          gbw
    ## 78        2943                    Duungidjawu   duun1241          wkw
    ## 79        2952                     Kartujarra   kart1247          mpj
    ## 80        2953                    Manjiljarra   many1256          mpj
    ## 81        2954                      Putijarra   pudi1238          mpj
    ## 82        2955                    Wangkajunga   wang1288          mpj
    ## 83        2966                  Kala Kawaw Ya   kala1378          mwp
    ## 84        2973                        Dhay'yi   dhal1246          dax
    ## 85        2992                          Djapu   djap1238          duj
    ## 86        3018                       Ngardily   west2437          wbp
    ##    iso639P3code Source
    ## 1                  spa
    ## 2                  spa
    ## 3                  spa
    ## 4                   ph
    ## 5                   ph
    ## 6                   ph
    ## 7                   ph
    ## 8                   ph
    ## 9                   ph
    ## 10                  ph
    ## 11                  ph
    ## 12                  ph
    ## 13                  ph
    ## 14                  gm
    ## 15                  gm
    ## 16                  gm
    ## 17                  gm
    ## 18                  gm
    ## 19                  gm
    ## 20                  gm
    ## 21                  gm
    ## 22                  gm
    ## 23                  gm
    ## 24                  gm
    ## 25                  gm
    ## 26                  gm
    ## 27                  gm
    ## 28                  gm
    ## 29                  gm
    ## 30                  ra
    ## 31                  ra
    ## 32                  ra
    ## 33              saphon
    ## 34              saphon
    ## 35              saphon
    ## 36              saphon
    ## 37                  uz
    ## 38                  ea
    ## 39                  ea
    ## 40                  ea
    ## 41                  ea
    ## 42                  ea
    ## 43                  ea
    ## 44                  ea
    ## 45                  ea
    ## 46                  er
    ## 47                  er
    ## 48                  er
    ## 49                  er
    ## 50                  er
    ## 51                  er
    ## 52                  er
    ## 53                  er
    ## 54                  er
    ## 55                  er
    ## 56                  er
    ## 57                  er
    ## 58                  er
    ## 59                  er
    ## 60                  er
    ## 61                  er
    ## 62                  er
    ## 63                  er
    ## 64                  er
    ## 65                  er
    ## 66                  er
    ## 67                  er
    ## 68                  er
    ## 69                  er
    ## 70                  er
    ## 71                  er
    ## 72                  er
    ## 73                  er
    ## 74                  er
    ## 75                  er
    ## 76                  er
    ## 77                  er
    ## 78                  er
    ## 79                  er
    ## 80                  er
    ## 81                  er
    ## 82                  er
    ## 83                  er
    ## 84                  er
    ## 85                  er
    ## 86                  er
