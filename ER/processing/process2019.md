Prepare Erich Round's Australian languages inventories for PHOIBLE
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
# Notes from the old script
# gara1261 is not a real glottolog code
# many of these gcodes are from dialects, so they don't match language codes
# gara1261 --> gara1269
# djiw1239 --> djiw1241
# kani1267 --> kani1276
# angu1240 --> angu1242
# woro1255 --> woro1258
```

``` r
library(dplyr)
library(ggplot2)
library(testthat)
```

``` r
# Get phoible index for ER inventories
pi.index <- read.table("/Users/stiv/Github/dev/mappings/InventoryID-LanguageCodes.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)

expect_equal(pi.index %>% filter(Source=="er") %>% nrow(), 392)
pi.index.er <- pi.index %>% filter(Source=="er")
rm(pi.index)

# Identify any duplicate entries in phoible
expect_equal(pi.index.er %>% group_by(LanguageName) %>% filter(n()>1) %>% nrow(), 0)
```

``` r
# Data for release (no ISO codes!)
df <- read.table("../data/raw2019/Australian_phonemes_for_PHOIBLE_20190118.tsv", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
```

``` r
er.names <- df %>% select(Variety_name, Glottolog_code) %>% distinct()
expect_equal(nrow(er.names), 392)
expect_equal(nrow(pi.index.er), 392)
             
x <- left_join(er.names, pi.index.er, by=c("Variety_name"="LanguageName"))
expect_equal(nrow(x), 392)

x$diff <- x$Glottolog_code == x$Glottocode
table(x$diff)
```

    ## 
    ## FALSE  TRUE 
    ##    90   287

``` r
table(x$diff, exclude = F)
```

    ## 
    ## TRUE <NA> 
    ##  287   15

``` r
x[which(x$diff==F),]
```

    ##           Variety_name         Glottolog_code InventoryID LanguageCode
    ## 21              Jandai            0000nunukul        2728          jan
    ## 24              Yagara               yaga1256        2731          yxg
    ## 27             Baanbay            0000baanbay        2734          kgs
    ## 30              Barada             0000barada        2798          gnl
    ## 31               Barna              0000barna        2799          bzr
    ## 33           Gabalbara          0000gabalbara        2801          gnl
    ## 34              Ganulu             0000ganulu        2803          gnl
    ## 35           Garingbal          0000garingbal        2804          bzr
    ## 36               Miyan              0000miyan        2805          bzr
    ## 37                Wiri               0000wiri        2806          bzr
    ## 38             Yambina            0000yambina        2807          bzr
    ## 39              Yangga             0000yangga        2808          bzr
    ## 40          Yetimarala         0000yetimarala        2809          gnl
    ## 41               Yilba              0000yilba        2810          bzr
    ## 42                Yuwi               0000yuwi        2811          bzr
    ## 43           Dharawala          0000dharawala        2812          bym
    ## 48         Wadjabangay        0000wadjabangay        2821          bym
    ## 49              Wangan             0000wangan        2822          bzr
    ## 54             Takalak            0000takalak        2879          ikr
    ## 60         Gugu Wakura       0000guugu_wakura        2885          gvn
    ## 63             Wagaman            0000wagaman        2889          gvn
    ## 69  Tableland Lamalama 0000tableland_lamalama        3020          lby
    ## 78          Yintyingka         0000yintyingka        2903          ayd
    ## 85          Ndra'ngith               tyan1235        2910          dgt
    ## 106      Yabula-Yabula      0000yabula-yabula        2980          xyy
    ## 108             Guweng             0000guweng        2939          gnr
    ## 109          Butchulla          0000butchulla        2940          gbw
    ## 110          Gabi-Gabi               kabi1260        2941          gbw
    ## 111          Barunggam          0000barunggam        2942          gbw
    ## 120             Dharuk             0000dharuk        2985          xdk
    ## 121               Eora               0000eora        2986          xdk
    ## 129      Murrinh-patha               murr1258        2634          mwf
    ## 137          Limilngan               limi1242        2643          lmc
    ## 150           Kuninjku           0000kuninjku        2659          gup
    ## 151          Kunwinjku               gunw1252        2660          gup
    ## 160              Garig               gari1253        2670          ilg
    ## 196        Ayerrerenge        0000ayerrerenge        2707          adg
    ## 211          Karangura          0000karangura        2751          nmv
    ## 212            Ngamini               ngam1265        2752          nmv
    ## 215            Nhirrpi            0000nhirrpi        2755          ynd
    ## 217         Yarluyandi         0000yarluyandi        2757          dif
    ## 219        Kungardutji        0000kungardutji        2759          xwk
    ## 234       Daungwurrung       0000daungwurrung        2774          wyi
    ## 235        Boonwurrung        0000boonwurrung        2775          wyi
    ## 238          Yari-Yari          0000yari-yari        2778         <NA>
    ## 239        Ladji-Ladji        0000ladji-ladji        2779          llj
    ## 240        Madhi-Madhi        0000madhi-madhi        2780          dmd
    ## 243  East Djadjawurung  0000east_djadjawurung        2782         <NA>
    ## 244  West Djadjawurung  0000west_djadjawurung        2988         <NA>
    ## 246          Nari Nari          0000nari_nari        2784          rnr
    ## 247         Djabwurung         0000djabwurung        2785          tjw
    ## 248        Wemba Wemba        0000wemba_wemba        2786          xww
    ## 249         Wotjobaluk         0000wotjobaluk        2787          xwt
    ## 252          Thangguai          0000thangguai        2790          unn
    ## 253              Nulit              0000nulit        2791          unn
    ## 258        Yitha Yitha               lowe1403        2796          xth
    ## 266         Yandjibara         0000yandjibara        2823          bym
    ## 267           Yiningay           0000yiningay        2824          bym
    ## 271         Mayi-Kulan               mayk1239        2829          mnt
    ## 273      Mayi-Thakurti      0000mayi-thakurti        2831          nxn
    ## 274          Mayi-Yapi          0000mayi-yapi        2832          mnt
    ## 276           Wunumara           0000wunumara        2834          nxn
    ## 279           Mirrniny           0000mirrniny        2837          nju
    ## 293          Balardung          0000balardung        2864          nys
    ## 294          Bibbulman          0000bibbulman        2865          xbp
    ## 295             Goreng             0000goreng        2866          xgg
    ## 297             Minang             0000minang        2868          nys
    ## 298           Pinjarup           0000pinjarup        2869          pnj
    ## 299           Wardandi           0000wardandi        2870          wxw
    ## 300              Wajuk               nyun1247        2871          xwj
    ## 301            Wiilman            0000wiilman        2872          nys
    ## 302            Wudjari            0000wudjari        2873          nys
    ## 303              Yuwat              0000yuwat        2874          nys
    ## 324             Palyku             0000palyku        2847          xny
    ## 327        Yinhawangka        0000yinhawangka        2850          yij
    ## 337     Eastern Wakaya     0000eastern_wakaya        2947          wga
    ## 339     Western Wakaya     0000western_wakaya        2949          wga
    ## 350             Ngalia             0000ngalia        2960          pjt
    ## 356         Malyangapa         0000malyangapa        2968          yga
    ## 357           Wadikali           0000wadikali        2969          wdk
    ## 358      Yardliyawarra               yarl1236        2970          yxl
    ## 370           Dharumba           0000dharumba        2994          tbh
    ## 372        Djirringany        0000djirringany        2996          xtv
    ## 373        Gundungurra               nort2760        2997          xrd
    ## 374            Ngarigu            0000ngarigu        2998          xni
    ## 376              Thawa              0000thawa        3000          xtv
    ## 380        Yangkaralda        0000yangkaralda        3004          gyd
    ## 386             Umiida               umii1235        3011          xud
    ## 388           Worrorra               woro1258        3013          wro
    ## 389         Yawijibaya         0000yawijibaya        3014          jbw
    ##     Glottocode Source  diff
    ## 21    jand1248     er FALSE
    ## 24    yaga1262     er FALSE
    ## 27    kumb1268     er FALSE
    ## 30    gang1268     er FALSE
    ## 31    biri1256     er FALSE
    ## 33    gang1268     er FALSE
    ## 34    gang1268     er FALSE
    ## 35    biri1256     er FALSE
    ## 36    biri1256     er FALSE
    ## 37    biri1256     er FALSE
    ## 38    biri1256     er FALSE
    ## 39    biri1256     er FALSE
    ## 40    gang1268     er FALSE
    ## 41    biri1256     er FALSE
    ## 42    biri1256     er FALSE
    ## 43    bidy1243     er FALSE
    ## 48    bidy1243     er FALSE
    ## 49    biri1256     er FALSE
    ## 54    ikar1243     er FALSE
    ## 60    kuku1273     er FALSE
    ## 63    kuku1273     er FALSE
    ## 69    lamu1254     er FALSE
    ## 78    ayab1239     er FALSE
    ## 85    ndra1239     er FALSE
    ## 106   yort1237     er FALSE
    ## 108   gure1255     er FALSE
    ## 109   kabi1260     er FALSE
    ## 110   gabi1248     er FALSE
    ## 111   kabi1260     er FALSE
    ## 120   sydn1236     er FALSE
    ## 121   sydn1236     er FALSE
    ## 129   murr1259     er FALSE
    ## 137   nucl1327     er FALSE
    ## 150   mura1269     er FALSE
    ## 151   guma1252     er FALSE
    ## 160   gari1254     er FALSE
    ## 196   ayer1246     er FALSE
    ## 211   kara1508     er FALSE
    ## 212   ngam1284     er FALSE
    ## 215   nhir1234     er FALSE
    ## 217   dier1241     er FALSE
    ## 219   wong1246     er FALSE
    ## 234   daun1234     er FALSE
    ## 235   woiw1237     er FALSE
    ## 238   yari1243     er FALSE
    ## 239   ladj1234     er FALSE
    ## 240   madh1244     er FALSE
    ## 243   west2443     er FALSE
    ## 244   west2443     er FALSE
    ## 246   nari1241     er FALSE
    ## 247   djab1234     er FALSE
    ## 248   wemb1241     er FALSE
    ## 249   wotj1234     er FALSE
    ## 252   gana1278     er FALSE
    ## 253   gana1278     er FALSE
    ## 258   yith1234     er FALSE
    ## 266   bidy1243     er FALSE
    ## 267   bidy1243     er FALSE
    ## 271   mayi1236     er FALSE
    ## 273   ngaw1240     er FALSE
    ## 274   mayi1235     er FALSE
    ## 276   ngaw1240     er FALSE
    ## 279   ngad1258     er FALSE
    ## 293   nyun1247     er FALSE
    ## 294   bibb1234     er FALSE
    ## 295   gore1235     er FALSE
    ## 297   nyun1247     er FALSE
    ## 298   pinj1244     er FALSE
    ## 299   ward1248     er FALSE
    ## 300   waju1234     er FALSE
    ## 301   nyun1247     er FALSE
    ## 302   nyun1247     er FALSE
    ## 303   nyun1247     er FALSE
    ## 324   nija1241     er FALSE
    ## 327   yind1247     er FALSE
    ## 337   waga1260     er FALSE
    ## 339   waga1260     er FALSE
    ## 350   pitj1243     er FALSE
    ## 356   maly1234     er FALSE
    ## 357   wadi1261     er FALSE
    ## 358   yard1234     er FALSE
    ## 370   thur1254     er FALSE
    ## 372   sout2771     er FALSE
    ## 373   gund1248     er FALSE
    ## 374   ngar1297     er FALSE
    ## 376   sout2771     er FALSE
    ## 380   kaya1319     er FALSE
    ## 386   umii1236     er FALSE
    ## 388   worr1237     er FALSE
    ## 389   yawi1239     er FALSE

``` r
# Create PHOIBLE inventories data format for the phoible aggregation script
m <- left_join(df, pi.index.er, by=c("Variety_name"="LanguageName"))
out <- m %>% select(InventoryID, Glottocode, LanguageCode, Variety_name, IPA_for_PHOIBLE) %>% arrange(InventoryID)
colnames(out) <- c("InventoryID", "Glottocode", "LanguageCode", "LanguageName", "Phoneme")
```

``` r
# A few data checks
table(out$Phoneme)
```

    ## 
    ##       a       ã      aː       æ       æ̃      æː       ɕ       ð       e 
    ##     392       1     180       9       1       2       6      28     131 
    ##       ẽ      eː       ə       ɣ       h       i       ĩ      iː       ɨ 
    ##       1      30      26      28       4     392       1     159      13 
    ##       j       j̪      jʷ       k       k͉       k͈      kʷ       l       l̺ 
    ##     392       5      10     322      69      70      11     267     125 
    ##       l̪      lʷ      l̪ʷ       ɭ      ɭʷ  \u0234 \u0234ʷ       m      mʷ 
    ##      98      10      10     248      10     202      10     392      10 
    ##       n       n̪       n̺       n̻      nʷ      n̪ʷ       ɳ      ɳʷ  \u0235 
    ##     267     236     125       1      10      10     259      10     379 
    ## \u0235ʷ       ŋ      ŋʷ       o       ø      oː      øː       ɔ       ɵ 
    ##      10     392      11      98       8      22       1       1       5 
    ##      ɵː       p       p͉       p͈      pʷ       ɸ       r       r̺       r̺͈ 
    ##       2     322      70      70      10       5     265     124       6 
    ##      rʷ       ɹ̺       ɺ       ɺ̢       ɻ      ɻʷ       ɽ       ɾ       ɾ̺ 
    ##      10     113       6       6     262      10      20      18       4 
    ##       t       t͉       t͈       t̪       t̺       t̻       t̪͉       t̪͈       t̺͉ 
    ##     222      44      45     201      99       1      43      44      24 
    ##       t̺͈      tʷ      t̪ʷ       ʈ       ʈ͉       ʈ͈      ʈʷ  \u0236  \u0236͉ 
    ##      24      10      10     222      45      44      10     313      64 
    ##  \u0236͈ \u0236ʷ       u      uː       ʉ       ɰ       w       x       y 
    ##      66      10     389     157       2      18     392       1       2 
    ##       z̺       ʐ       ʑ       ʔ       β       θ 
    ##       1       4       8      43      29       3

``` r
table(out$LanguageName)
```

    ## 
    ##         Adnyamathanha       Aghu Tharnggala               Agwamin 
    ##                    27                    27                    21 
    ##                 Alawa               Alngith              Alyawarr 
    ##                    26                    28                    43 
    ##               Amurdak            Angkamuthi            Anguthimri 
    ##                    24                    24                    36 
    ##          Anindilyakwa           Antakirinya        Antekerrepenhe 
    ##                    26                    23                    44 
    ##               Arabana        Aritinngithigh              Atampaya 
    ##                    24                    22                    26 
    ##              Awabakal             Awu Alaya              Ayapathu 
    ##                    16                    31                    25 
    ##           Ayerrerenge               Baanbay              Badimaya 
    ##                    43                    19                    23 
    ##                Bakanh             Balardung            Bandjigali 
    ##                    31                    22                    26 
    ##                Barada                 Bardi                 Barna 
    ##                    18                    24                    18 
    ##             Barunggam             Bibbulman              Bidhawal 
    ##                    23                    22                    25 
    ##               Bidyara              Bigambal             Bilinarra 
    ##                    20                    16                    23 
    ##              Binbinka                  Biri           Boonwurrung 
    ##                    23                    18                    23 
    ##               Bularnu            Bundjalung                Bunuba 
    ##                    32                    20                    24 
    ##               Burarra             Butchulla              Buwandik 
    ##                    26                    24                    23 
    ##      Central Arrernte               Dalabon            Darkinyung 
    ##                    44                    29                    18 
    ##          Daungwurrung                Dhangu              Dharawal 
    ##                    23                    26                    25 
    ##             Dharawala                Dharuk              Dharumba 
    ##                    17                    19                    25 
    ##             Dharumbal               Dhay'yi             Dhudhuroa 
    ##                    21                    26                    21 
    ##                Dhurga                Diyari              Djabugay 
    ##                    25                    25                    19 
    ##            Djabwurung        Djambarrpuyngu               Djangun 
    ##                    21                    31                    16 
    ##                 Djapu               Djinang                Djinba 
    ##                    25                    25                    25 
    ##             Djindewal           Djirringany           Duungidjawu 
    ##                    19                    25                    23 
    ##               Dyirbal     East Djadjawurung    Eastern Anmatyerre 
    ##                    16                    21                    44 
    ##      Eastern Arrernte        Eastern Wakaya                  Emmi 
    ##                    44                    25                    32 
    ##                  Eora                  Erre              Gaagudju 
    ##                    19                    23                    27 
    ##             Gabalbara             Gabi-Gabi               Galaagu 
    ##                    18                    19                    26 
    ##               Gambera            Gamilaraay            Gangalidda 
    ##                    25                    21                    24 
    ##               Gangulu                Ganulu                 Garig 
    ##                    22                    18                    24 
    ##             Garingbal               Garlali                Garrwa 
    ##                    18                    21                    17 
    ##               Gidabal            Gooniyandi                Goreng 
    ##                    21                    23                    22 
    ##               Gudanji                Gudjal           Gugu Badhun 
    ##                    19                    18                    17 
    ##           Gugu Wakura                Gumatj           Gumbaynggir 
    ##                    16                    31                    19 
    ##      Gun-Dedjnjenghmi           Gun-Djeihmi           Gundungurra 
    ##                    27                    27                    25 
    ##             Gungabula              Gunggari                 Gunya 
    ##                    20                    18                    31 
    ##            Gupapuyngu         Gureng-gureng              Gurindji 
    ##                    31                    23                    23 
    ##             Gurr-Goni       Guugu Yimidhirr                  Guwa 
    ##                    27                    23                    23 
    ##                Guwamu                 Guwar                Guweng 
    ##                    21                    19                    23 
    ##           Ikarranggal                 Ilgar               Iwaidja 
    ##                    23                    24                    23 
    ##         Jabirr-Jabirr             Jaminjung                Jandai 
    ##                    20                    22                    19 
    ##           Jardwadjali                  Jaru                  Jawi 
    ##                    21                    23                    20 
    ##                Jawoyn               Jingulu               Jiwarli 
    ##                    27                    20                    26 
    ##                 Jukun         Kala Kawaw Ya         Kala Lagaw Ya 
    ##                    28                    24                    24 
    ##             Kalaamaya            Kalkatungu              Kaniyang 
    ##                    26                    26                    22 
    ##             Karajarri             Karangura             Kariyarra 
    ##                    21                    25                    23 
    ##            Kartujarra              Katthang                Kaurna 
    ##                    24                    19                    25 
    ##             Kayardild              Kaytetye               Keramin 
    ##                    23                    43                    23 
    ##                  Kija               Kok Nar             Koko Bera 
    ##                    24                    25                    21 
    ##             Kolakngat        Kugu Nganhcara                Kukata 
    ##                    22                    30                    23 
    ##                Kukatj               Kukatja          Kuku Yalanji 
    ##                    23                    23                    16 
    ##                  Kune           Kungarakany           Kungardutji 
    ##                    28                    22                    30 
    ##              Kungkari            Kunindirri              Kuninjku 
    ##                    25                    17                    27 
    ##             Kunwinjku               Kurrama               Kurtjar 
    ##                    27                    27                    28 
    ##               Kuthant            Kuugu Ya'u                 Kwini 
    ##                    25                    21                    28 
    ##           Ladji-Ladji              Lamalama                Lardil 
    ##                    21                    29                    25 
    ##              Larrakia             Limilngan           Linngithigh 
    ##                    24                    25                    29 
    ## Lower Southern Aranda               Luthigh           Madhi-Madhi 
    ##                    44                    22                    20 
    ##           Malak-Malak               Malkana               Malngin 
    ##                    19                    26                    21 
    ##          Malthanmungu            Malyangapa               Mangala 
    ##                    25                    27                    24 
    ##            Mangarrayi           Manjiljarra               Margany 
    ##                    22                    23                    31 
    ##                 Marra        Marramaninyshi                Marrgu 
    ##                    23                    20                    26 
    ##            Marringarr           Marrithiyel              Marti Ke 
    ##                    32                    24                    31 
    ##          Martuthunira              Matngele                 Mawng 
    ##                    26                    22                    22 
    ##                Mayali            Mayi-Kulan           Mayi-Kutuna 
    ##                    27                    21                    21 
    ##         Mayi-Thakurti             Mayi-Yapi              Mbabaram 
    ##                    22                    21                    27 
    ##                 Mbara               Mbiywom            Mengerrdji 
    ##                    27                    29                    23 
    ##                Minang            Minjungbal                Minkin 
    ##                    22                    20                    23 
    ##             Miriwoong              Mirrniny               Mithaka 
    ##                    20                    26                    29 
    ##                 Miyan            Mpalitjanh              Mudburra 
    ##                    18                    26                    23 
    ##             Muk-Thang             Muluridji         Murrinh-patha 
    ##                    22                    16                    28 
    ##              Muruwari                Nakara             Nari Nari 
    ##                    25                    25                    23 
    ##             Narrungga             Ndjebbana            Ndra'ngith 
    ##                    27                    21                    23 
    ##         Ngaanyatjarra           Ngadjunmaya             Ngalakgan 
    ##                    23                    26                    27 
    ##                Ngalia            Ngaliwurru               Ngamini 
    ##                    23                    21                    25 
    ##       Ngan'gityemerri                Ngandi           Nganyaywana 
    ##                    23                    35                    17 
    ##                Ngardi              Ngardily               Ngarigu 
    ##                    24                    24                    18 
    ##             Ngarinyin            Ngarinyman                Ngarla 
    ##                    22                    23                    23 
    ##          Ngarlawangka              Ngarluma               Ngarnka 
    ##                    23                    23                    21 
    ##                Ngawun             Ngayawang              Ngintait 
    ##                    21                    23                    23 
    ##             Ngiyambaa                Ngkoth              Ngumbarl 
    ##                    21                    26                    20 
    ##              Ngunawal                Nhanda                Nhangu 
    ##                    25                    33                    31 
    ##               Nhirrpi               Nhuwala             Nimanburu 
    ##                    29                    26                    23 
    ##                Nukunu                 Nulit               Nungali 
    ##                    28                    22                    17 
    ##                Nyamal           Nyangumarta              Nyawaygi 
    ##                    23                    21                    18 
    ##               Nyikina            Nyiyaparli              Nyulnyul 
    ##                    20                    25                    20 
    ##         Ogh Awarrangg            Ogh Unyjan                 Olkol 
    ##                    28                    28                    28 
    ##             Oykangand      Pallanganmiddang                Palyku 
    ##                    28                    25                    26 
    ##              Panyjima             Parnkalla           Patjtjamalh 
    ##                    25                    23                    22 
    ##               Payungu               Piangil              Pinjarup 
    ##                    26                    21                    22 
    ##               Pintupi             Pirlatapa               Pirriya 
    ##                    23                    25                    24 
    ##        Pitjantjatjara           Pitta Pitta            Punthamara 
    ##                    23                    26                    21 
    ##               Purduna             Putijarra            Rembarrnga 
    ##                    26                    23                    28 
    ##        Rimanggudinhma            Ritharrngu    Southern Paakintyi 
    ##                    29                    31                    26 
    ##    Tableland Lamalama               Takalak             Thaayorre 
    ##                    29                    27                    26 
    ##             Thalanyji             Thanggati             Thangguai 
    ##                    26                    19                    22 
    ##             Tharrkari                 Thawa          Thaynakwithi 
    ##                    31                    25                    29 
    ##                 Thiin              Thirarri                  Tiwi 
    ##                    26                    25                    22 
    ##               Turubul             Umbugarla             Umbuygamu 
    ##                    19                    25                    32 
    ##                Umiida                Umpila               Unggumi 
    ##                    25                    22                    24 
    ##                Uradhi            Urningangg              Waalubal 
    ##                    22                    23                    20 
    ##                Waanyi              Wadikali           Wadjabangay 
    ##                    20                    27                    18 
    ##               Wagaman               Wagiman                 Wajuk 
    ##                    16                    27                    22 
    ##             Waka Waka             Walangama            Walmajarri 
    ##                    23                    28                    23 
    ##               Wambaya                Wangan           Wangkajunga 
    ##                    23                    18                    23 
    ##          Wangkangurru         Wangkayutyuru            Wangkumara 
    ##                    24                    26                    30 
    ##             Wanyjirra              Wardaman              Wardandi 
    ##                    21                    22                    22 
    ##             Warlmanpa              Warlpiri            Warluwarra 
    ##                    28                    24                    33 
    ##           Warndarrang               Warnman                Warray 
    ##                    20                    23                    21 
    ##             Warrgamay           Warriyangga           Warrnambool 
    ##                    19                    26                    26 
    ##                Warrwa             Warumungu               Warungu 
    ##                    20                    28                    17 
    ##          Wathawurrung           Wathi Wathi              Watjarri 
    ##                    23                    21                    26 
    ##              Wayilwan           Wemba Wemba     West Djadjawurung 
    ##                    21                    22                    21 
    ##    Western Anmatyerre      Western Arrernte        Western Wakaya 
    ##                    44                    44                    26 
    ##               Wiilman           Wik Mungkan           Wik-Ngathan 
    ##                    22                    26                    28 
    ##             Wiradjuri               Wirangu                  Wiri 
    ##                    21                    23                    18 
    ##            Wiriyaraay            Woiwurrung              Worrorra 
    ##                    21                    23                    25 
    ##            Wotjobaluk                 Wubuy               Wudjari 
    ##                    19                    25                    22 
    ##               Wulguru             Wuli Wuli              Wunambal 
    ##                    21                    23                    28 
    ##              Wunumara               Wurrugu         Yabula-Yabula 
    ##                    22                    23                    20 
    ##            Yadhaykenu                Yagara             Yalarnnga 
    ##                    26                    19                    24 
    ##               Yambina                 Yanda            Yandjibara 
    ##                    18                    24                    21 
    ##          Yandruwandha                Yangga           Yangkaralda 
    ##                    30                    18                    24 
    ##               Yangman       Yankunytjatjara               Yanyuwa 
    ##                    22                    23                    24 
    ##               Yaraldi         Yardliyawarra             Yari-Yari 
    ##                    25                    27                    23 
    ##            Yarluyandi         Yawarrawarrka            Yawijibaya 
    ##                    24                    30                    25 
    ##                Yawuru                Yaygir            Yetimarala 
    ##                    28                    20                    18 
    ##                Yidiny                 Yilba          Yindjibarndi 
    ##                    19                    18                    25 
    ##             Yingkarta           Yinhawangka              Yiningay 
    ##                    26                    26                    19 
    ##            Yintyingka                Yinwum            Yir Yoront 
    ##                    26                    27                    26 
    ##           Yitha Yitha           Yorta Yorta              Yugambal 
    ##                    23                    19                    16 
    ##             Yulparija           Yuwaalaraay            Yuwaliyaay 
    ##                    23                    21                    21 
    ##                 Yuwat                  Yuwi 
    ##                    22                    18

``` r
out %>% group_by(LanguageName) %>% filter(n()<10)
```

    ## # A tibble: 0 x 5
    ## # Groups:   LanguageName [0]
    ## # ... with 5 variables: InventoryID <int>, Glottocode <chr>,
    ## #   LanguageCode <chr>, LanguageName <chr>, Phoneme <chr>

``` r
# Write the inventories to disk
write.table(out, file="../data/formatted2019/ER_inventories.tsv", sep="\t", quote=FALSE, row.names=FALSE)
```

``` r
# Update the bibtex keys
refs <- m %>% select(InventoryID, Source_ref) %>% distinct() %>% arrange(InventoryID)

# Expect that there are the same number of bibtex keys as inventories
expect_equal(nrow(refs), 392)
expect_true(all(!is.na(refs$Source_ref)))

refs$Source <- 'er'
head(refs)
```

    ##   InventoryID                   Source_ref Source
    ## 1        2629            green_sketch_1987     er
    ## 2        2630           rumsey_bunuba_2000     er
    ## 3        2631     mcgregor_functional_1990     er
    ## 4        2632       zandvoort_grammar_1999     er
    ## 5        2633          birk_phonology_1975     er
    ## 6        2634 mansfield_polysynthetic_2014     er

``` r
tail(refs)
```

    ##     InventoryID                     Source_ref Source
    ## 387        3015            merlan_grammar_1994     er
    ## 388        3016            merlan_grammar_1994     er
    ## 389        3017             austin_guwamu_1980     er
    ## 390        3018          nash_preliminary_1979     er
    ## 391        3019            blake_dialects_2011     er
    ## 392        3020 verstraete_mbarrumbathama_2018     er

``` r
write.table(refs, file="../data/formatted2019/ER_InventoryID-Bibtex.tsv", sep="\t", quote=FALSE, row.names=FALSE)
```
