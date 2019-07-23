Get EA inventories-IPA correspondences data for PHOIBLE
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;
23 July, 2019

``` r
# NOTE: deprecated!! This file does not contain any changes made directly to the EA_IPA_correspondences.tsv file here:
  
# https://github.com/phoible/dev/tree/master/raw-data/EA
```

Functions for cleaning up IPA
-----------------------------

``` r
# Denormalize strings
denorm <- function(x) stri_trans_general(x, "Any-NFD")

# Re-normalize just the c-cedilla after denormalizing
fix_c_cedilla <- function(strings) {
  c_cedilla <- "\u00E7"
  c_cedilla_denormed <- "\u0063\u0327"
  stri_replace_all_fixed(strings, pattern=c_cedilla_denormed,
                         replacement=c_cedilla)
}

# Remove brackets
removeBrackets <- function (x, type="square") {
  brak <- switch(type, square=c("[", "]"), angle=c("<", ">"))
  x <- stri_replace_first_fixed(x, pattern=brak[1], replacement="")
  x <- stri_replace_first_fixed(x, pattern=brak[2], replacement="")
}

# Convert strings to plus-separated codepoints
codepoints <- function(strings) {
  cps <- stri_trans_general(strings, "Any-Hex/Unicode")
  cps <- stri_replace_all_fixed(cps, replacement="", pattern = "U")
  cps <- stri_replace_first_fixed(cps, replacement="", pattern = "+")
  cps
}

## Impose canonical ordering of codepoints
## (gets applied in the "CleanUp" function)
orderIPA <- function(strings, lang=NA, source=NA) {
  ## DEFINITION OF GLYPH TYPES
  ## Tones are not internally reordered so the order here is arbitrary.
  tones <- c("˩", "˨", "˧", "˦", "˥", "↓")
  ## Order of elements in "diacritics" and "modifiers" sets canonical order!!
  modifiers <- c(
    "˞",  # rhotic wing
    "ː",  # long
    "ˑ",  # half-long
    "ᴱ",  # epilaryngeal source
    "ⁿ",  # nasal release
    "ˡ",  # lateral release
    "ʱ",  # breathy aspirated
    "ʰ",  # aspirated
    "˭",  # unaspirated
    "ˀ",  # glottalized
    "ˤ",  # pharyngealized
    "ˠ",  # velarized
    "ʲ",  # palatalized
    "ʷ",  # labialized
    "ᶣ",  # labial-palatalized  # TODO: should use labial+palatal?
    "ᵊ",  # schwa-like release # TODO: check what this really is
    "ʼ"  # ejective
  )
  diacritics <- c(
    "̴",  # velarized/pharyngealized (combining tilde overlay)
    "̃",  # nasalized (combining tilde)
    "͊",  # denasalized (combining not tilde above)
    "͋",  # nasal emission (combining homothetic)
    "̮",  # derhoticized (combining breve below)
    "̤",  # breathy (combining diaresis below)
    "̰",  # creaky (combining tilde below)
    "̬",  # stiff (combining caron below)
    "͓",  # frictionalized (combining x below)
    "̼",  # linguolabial (combining seagull below)
    "̪",  # dental (combining bridge below)
    "̺",  # apical (combining inverted bridge below)
    "̻",  # laminal (combining square below)
    "͇",  # non-sibilant (combining equals sign below)
    "͈",  # fortis (combining double vertical line below)
    "͉",  # lenis (combining left angle below)
    "̙",  # retracted tongue root (combining right tack below)
    "̘",  # advanced tongue root (combining left tack below)
    "̞",  # lowered (combining down tack below)
    "̝",  # raised (combining up tack below)
    "̟",  # advanced (combining plus sign below)
    "̠",  # retracted (combining minus sign below)
    "̈",  # centralized (combining diaresis)
    "̽",  # mid-centralized (combining x above)
    "̹",  # more round (combining right half ring)
    "̜",  # less round (combining left half ring)
    "̩",  # syllabic (combining vertical line below)
    "̯",  # non-syllabic (combining inverted breve below)
    "̆",  # short (combining breve)
    "̥",  # devoiced (combining ring below)
    "̊",  # devoiced (combining ring above)
    "̚"   # unreleased (combining left angle above)
  )
  ## The base glyphs are broken up into types for convenience only; at present
  ## their order does not matter.
  vowels <- c("i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ", "e", "ø", "ɘ", "ɵ",
              "ɤ", "o", "ə", "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ", "a", "ɶ",
              "ɑ", "ɒ", "ɚ", "ɝ")
  stops <- c("p", "b", "t", "d", "ʈ", "ɖ", "c", "ɟ", "k", "ɡ", "q", "ɢ", "ʡ",
             "ʔ")
  nasals <- c("m", "ɱ", "n", "ɳ", "ɲ", "ŋ", "ɴ")
  fricatives <- c("ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ɕ", "ʑ", "ʃ", "ʒ",
                  "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "ʜ", "ʢ",
                  "h", "ɦ", "ɬ", "ɮ", "ɧ", "ʍ")
  flaps <- c("ʙ", "ⱱ", "r", "ɾ", "ᴅ", "ɽ", "ʀ", "ɺ")  
  affricates <- c("ʦ", "ʣ", "ʧ", "ʤ")
  implosives <- c("ƥ", "ɓ", "ƭ", "ɗ", "ᶑ", "ƈ", "ʄ", "ƙ", "ɠ", "ʠ", "ʛ")
  approximants <- c("ʋ", "ɹ", "ɻ", "j", "ɥ", "ɰ", "l", "ɭ", "ʎ", "ʟ", "ɫ", "w")
  clicks <- c("ʘ", "ǀ", "ǁ", "ǃ", "ǂ", "‼")
  archephonemes <- c("R", "N")  # R = tap/trill; N = placeless nasal
  base.glyphs <- c(stops, nasals, fricatives, flaps, affricates, implosives,
                   approximants, clicks, vowels, archephonemes)
  ## start by denormalizing
  strings <- denorm(strings)
  ## re-normalize c-cedilla
  strings <- fix_c_cedilla(strings)
  ## replace *R/*N with R/N
  strings <- stri_replace_all_fixed(strings, pattern="*R", replacement="R")
  strings <- stri_replace_all_fixed(strings, pattern="*N", replacement="N")
  ## remove whitespace, tiebars, & square brackets
  strings <- removeBrackets(strings)
  strings <- stri_trim(strings)
  strings <- stri_replace_all_fixed(strings, replacement="", pattern="͡")
  strings <- stri_replace_all_fixed(strings, replacement="", pattern="͜")
  ## construct parallel string showing character classes
  df <- data.frame(phone=strings, lang=lang, source=source, stringsAsFactors = F)
  chars <- sapply(seq_len(nrow(df)), function(x) {
    i <- df[x, "phone"]
    if (is.na(i)) return(NA)
    iso <- df[x, "lang"]
    src <- df[x, "source"]
    chr <- strsplit(i, "")[[1]]
    typ <- codepoints(chr)
    typ[typ %in% codepoints("|")] <- "|"  # restore upsid disjuncts
    typ[typ %in% codepoints(base.glyphs)] <- "B"
    typ[typ %in% codepoints(modifiers)] <- "M"
    typ[typ %in% codepoints(diacritics)] <- "D"
    typ[typ %in% codepoints(tones)] <- "T"
    if (!all(typ %in% c("B", "M", "D", "T", "|"))) {
      #         debug <<- rbind(debug, data.frame(phone=i, codepoints=codepoints(i),
      #                                           iso=iso, src=src))
      #            sink(phone.validity.log)
      #            print(debug)
      #            sink()
      ## replace typestring codepoints with orig. glyphs to avoid
      ## indexing errors below
      missing <- !typ %in% c("B", "M", "D", "T", "|")
      typ[missing] <- chr[missing]
      warning(paste("Unfamiliar glyph components.", "Phone:", i,
                    "Codepoint:", codepoints(typ[missing])),
              call.=FALSE, immediate.=TRUE)
    }
    typstr <- paste(typ, collapse="")
    ## move tones to end
    if (stringi::stri_detect_fixed(typstr, "T")) {
      ix <- stringi::stri_locate_first_fixed(typstr, "T")[1]
      rightedge <- typ[ix:length(typ)]
      while (ix < length(chr) && !all(rightedge %in% "T")) {
        if (ix == 1) neworder <- c(2:length(typ), 1)
        else if (ix < length(typ)) neworder <- c(1:(ix-1),
                                                 (ix+1):length(typ), ix)
        chr <- chr[neworder]
        typ <- typ[neworder]
        typstr <- paste(typ, collapse="")
        ix <- stringi::stri_locate_first_fixed(typstr, "T")[1]
        rightedge <- typ[ix:length(typ)]
      }
    }
    ## If a diacritic comes right after a modifier letter, swap their order
    while (stringi::stri_detect_fixed(typstr, "MD")) {
      ix <- stringi::stri_locate_first_fixed(typstr, "MD")[1]
      if (ix == 1) neworder <- c(2, 1, 3:length(chr))
      else if (ix == length(chr)-1) neworder <- c(1:(ix-1), ix+1, ix)
      else neworder <- c(1:(ix-1), ix+1, ix, (ix+2):length(chr))
      chr <- chr[neworder]
      typ <- typ[neworder]
      typstr <- paste(typ, collapse="")
    }
    ## Put sequences of modifier letters in canonical order
    if (stringi::stri_detect_fixed(typstr, "MM")) {
      ixs <- stringi::stri_locate_all_regex(typstr, "M+")[[1]]
      for (row in seq_len(dim(ixs)[1])) {
        span <- ixs[row,1]:ixs[row,2]
        mods <- chr[span]
        chr[span] <- modifiers[modifiers %in% mods]
      }   }
    ## Put sequences of diacritics in canonical order
    if (stringi::stri_detect_fixed(typstr, "DD")) {
      ixs <- stringi::stri_locate_all_regex(typstr, "D+")[[1]]
      for (row in seq_len(dim(ixs)[1])) {
        span <- ixs[row,1]:ixs[row,2]
        dcrs <- chr[span]
        chr[span] <- diacritics[diacritics %in% dcrs]
      }   }
    paste(chr, collapse="")
  }, USE.NAMES=FALSE)
  ## restore asterisks
  chars <- stringi::stri_replace_all_fixed(chars, pattern="R",
                                           replacement="*R")
  chars <- stringi::stri_replace_all_fixed(chars, pattern="N",
                                           replacement="*N")
  chars
}
```

Conventionalize the Eurasian database's segment types and write a EA-IPA correspondences file for PHOIBLE aggregation
---------------------------------------------------------------------------------------------------------------------

``` r
# Load the EA CSV data and get a unique list of segments
ea <- read.csv("../data/raw/phono_dbase.csv", stringsAsFactors=FALSE, strip.white = T)
df <- distinct(select(ea, Segment))
colnames(df) <- c("Segment")
df$Segment <- sort(df$Segment)
glimpse(df)
```

    ## Observations: 1,484
    ## Variables: 1
    ## $ Segment <chr> "(ã)", "(bʲ)", "(ɕ)", "(dʲ)", "(dz̪)", "(e̞ː)", "(ə)", …

``` r
# Populate phoneme column and do replacements
df$Phoneme <- df$Segment
```

``` r
# Apply the rule-specific replacements
ea.ipa <- read.table("../data/formatted/Handmade-EA_IPA_correspondences.tsv", sep="\t", header=T, stringsAsFactors=FALSE)
glimpse(ea.ipa)
```

    ## Observations: 103
    ## Variables: 3
    ## $ Segment <chr> " ʑ", "ⁿp", "ⁿt", "ⁿc", "ⁿk", "ⁿq", "ⁿpʰ", "ⁿtʰ", "ⁿcʰ",…
    ## $ IPA     <chr> "ʑ", "mp", "nt", "ɲc", "ŋk", "ɴq", "mpʰ", "ntʰ", "ɲcʰ", …
    ## $ Notes   <chr> "test", "conventionalized", "conventionalized", "convent…

``` r
# ea.ipa$Segment %in% df$Segment
df <- merge(df, ea.ipa, by="Segment", all.x=TRUE)
glimpse(df)
```

    ## Observations: 1,484
    ## Variables: 4
    ## $ Segment <chr> "(ã)", "(bʲ)", "(ɕ)", "(dʲ)", "(dz̪)", "(e̞ː)", "(ə)", …
    ## $ Phoneme <chr> "(ã)", "(bʲ)", "(ɕ)", "(dʲ)", "(dz̪)", "(e̞ː)", "(ə)", …
    ## $ IPA     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ Notes   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
df <- within(df, Phoneme <- ifelse(!is.na(IPA), IPA, Phoneme))
df$IPA <- NULL
```

``` r
# Apply the general replacements
df$Phoneme <- sub(" ", "", df$Phoneme)
df$Phoneme <- sub("\\(", "\\<", df$Phoneme)
df$Phoneme <- sub("\\)", "\\>", df$Phoneme)
# df$Phoneme <- sub("<", "", df$Phoneme)
# df$Phoneme <- sub(">", "", df$Phoneme)
df$Phoneme <- sub("g", "ɡ", df$Phoneme)
df$Phoneme <- sub("tʃ", "t̠ʃ", df$Phoneme)
df$Phoneme <- sub("dʒ", "d̠ʒ", df$Phoneme)
df$Phoneme <- sub("dʒ", "d̠ʒ", df$Phoneme)
df$Phoneme <- sub("dʒ", "d̠ʒ", df$Phoneme)
df$Phoneme <- sub("ɿ", "z̩", df$Phoneme)
df$Phoneme <- sub("ʅ", "ʐ̩", df$Phoneme)
df$Phoneme <- sub("ŝ", "ɕ", df$Phoneme)
df$Phoneme <- sub("ẑ", "ʑ", df$Phoneme)
glimpse(df)
```

    ## Observations: 1,484
    ## Variables: 3
    ## $ Segment <chr> "(ã)", "(bʲ)", "(ɕ)", "(dʲ)", "(dz̪)", "(e̞ː)", "(ə)", …
    ## $ Phoneme <chr> "<ã>", "<bʲ>", "<ɕ>", "<dʲ>", "<dz̪>", "<e̞ː>", "<ə>", …
    ## $ Notes   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
# Order the IPA (make sure to order the IPA in the phoible feature matrix above)
df$Phoneme <- stri_trans_general(df$Phoneme, "nfd")
df$Phoneme <- orderIPA(df$Phoneme)
```

    ## Warning: Unfamiliar glyph components. Phone: <ã> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ã> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <bʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <bʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɕ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɕ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <dʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <dʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <dz̪> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <dz̪> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <e̞ː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <e̞ː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ə> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ə> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɛ̃> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɛ̃> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɛː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɛː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɡʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɡʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <iː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <iː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <i̯e̞> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <i̯e̞> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <lʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <lʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <nʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <nʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <œ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <œ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <œː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <œː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɔ̃> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɔ̃> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɔː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɔː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <sʲ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <sʲ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ʃ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ʃ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <uː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <uː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɯ̯ɤ̞> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɯ̯ɤ̞> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ɯi̯> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ɯi̯> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <yː> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <yː> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <z> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <z> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ʑ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ʑ> Codepoint: 003E

    ## Warning: Unfamiliar glyph components. Phone: <ʒ> Codepoint: 003CUnfamiliar
    ## glyph components. Phone: <ʒ> Codepoint: 003E

``` r
df$Phoneme <- trimws(df$Phoneme)
```

``` r
# Load phoible segment-feature matrix and order IPA
features <- read.csv("https://raw.githubusercontent.com/phoible/dev/master/raw-data/FEATURES/phoible-segments-features.tsv", sep="\t", stringsAsFactors=FALSE)
features$segment
```

    ##    [1] "m"           "k"           "i"           "a"          
    ##    [5] "j"           "p"           "u"           "w"          
    ##    [9] "n"           "s"           "t"           "b"          
    ##   [13] "o"           "e"           "l"           "h"          
    ##   [17] "ɡ"           "ŋ"           "d"           "ɲ"          
    ##   [21] "f"           "ʔ"           "ɛ"           "ɔ"          
    ##   [25] "ʃ"           "r"           "t̠ʃ"          "z"          
    ##   [29] "ɾ"           "iː"          "v"           "aː"         
    ##   [33] "d̠ʒ"          "uː"          "˦"           "˨"          
    ##   [37] "t̪"           "ts"          "ə"           "oː"         
    ##   [41] "eː"          "ĩ"           "ã"           "ũ"          
    ##   [45] "ɨ"           "x"           "d̪"           "kʰ"         
    ##   [49] "kp"          "ɡb"          "n̪"           "pʰ"         
    ##   [53] "ɪ"           "ʊ"           "kʷ"          "ʒ"          
    ##   [57] "õ"           "c"           "ɣ"           "ɓ"          
    ##   [61] "mb"          "ẽ"           "ɟ"           "˧"          
    ##   [65] "ŋɡ"          "nd"          "β"           "tʰ"         
    ##   [69] "ɛː"          "ɔː"          "ɗ"           "o̞"          
    ##   [73] "e̞"           "l̪"           "dz"          "ɛ̃"          
    ##   [77] "tʃ"          "s̪"           "ɔ̃"           "q"          
    ##   [81] "kʼ"          "ʈ"           "ɡʷ"          "ɖ"          
    ##   [85] "t̪ʰ"          "tʼ"          "˦˨"          "ɸ"          
    ##   [89] "æ"           "pʼ"          "ɯ"           "t̠ʃʰ"        
    ##   [93] "t̠ʃʼ"         "ɨ̃"           "χ"           "ɑ"          
    ##   [97] "ʂ"           "n̠d̠ʒ"         "˨˦"          "ɽ"          
    ##  [101] "ɳ"           "cç"          "ɬ"           "ŋʷ"         
    ##  [105] "ç"           "ɟʝ"          "ʌ"           "ʎ"          
    ##  [109] "ɭ"           "ð"           "əː"          "tsʼ"        
    ##  [113] "tsʰ"         "ŋmɡb"        "ŋm"          "ɪ̃"          
    ##  [117] "θ"           "y"           "ʊ̃"           "kʲ"         
    ##  [121] "z̪"           "mː"          "ɦ"           "ʊː"         
    ##  [125] "ĩː"          "ɪː"          "nː"          "ũː"         
    ##  [129] "ʈʰ"          "t̪s̪"          "ə̃"           "ɨː"         
    ##  [133] "r̪"           "xʷ"          "õ̞"           "˥"          
    ##  [137] "kː"          "hʷ"          "ʁ"           "lː"         
    ##  [141] "qʼ"          "ãː"          "tʲ"          "ai"         
    ##  [145] "ɡʲ"          "mʷ"          "ŋɡʷ"         "t̪ʼ"         
    ##  [149] "ħ"           "ʈʂ"          "bʷ"          "b̤"          
    ##  [153] "˩"           "cʰ"          "˥˩"          "bː"         
    ##  [157] "ø"           "ndz"         "sː"          "ẽ̞"          
    ##  [161] "ʝ"           "ɑː"          "ʉ"           "au"         
    ##  [165] "ɱv"          "dʒ"          "nz"          "pː"         
    ##  [169] "kʷʼ"         "tː"          "ɤ"           "ɡʱ"         
    ##  [173] "n̠"           "cçʰ"         "qʷ"          "m̥"          
    ##  [177] "ʄ"           "ɡː"          "qʰ"          "pʲ"         
    ##  [181] "œ"           "mʲ"          "ɮ"           "õː"         
    ##  [185] "fʷ"          "ɥ"           "æː"          "dʲ"         
    ##  [189] "a̟"           "jː"          "pʷ"          "ʕ"          
    ##  [193] "d̪z̪"          "ɹ"           "χʷ"          "ɐ"          
    ##  [197] "ŋk"          "d̪̤"           "nt"          "ɖ̤"          
    ##  [201] "ɒ"           "ʐ"           "ɻ"           "rː"         
    ##  [205] "ɘ"           "˩˥"          "kʷʰ"         "bʲ"         
    ##  [209] "*R̪"          "pf"          "t̪s̪ʰ"         "dː"         
    ##  [213] "sʷ"          "æ̃"           "ẽː"          "ɲː"         
    ##  [217] "ɲɟ"          "mp"          "n̥"           "n̪d̪"         
    ##  [221] "tʷ"          "ɰ"           "ɯ̃"           "ʋ"          
    ##  [225] "lʲ"          "ɾ̪"           "ɛ̃ː"          "˨˧"         
    ##  [229] "wː"          "w̰"           "j̰"           "o̞ː"         
    ##  [233] "ʍ"           "fː"          "ɔ̃ː"          "ɯː"         
    ##  [237] "ŋː"          "ɢ"           "˧˨"          "ɺ"          
    ##  [241] "ɜ"           "ɑ̃"           "ɬ̪"           "hʲ"         
    ##  [245] "t̠ʃː"         "ʃː"          "e̞ː"          "nʲ"         
    ##  [249] "\u2c71"      "ʃʷ"          "æ̞̃"           "ŋ̥"          
    ##  [253] "ɕ"           "m̰"           "β̞"           "ɟ̤ʝ"         
    ##  [257] "l̥"           "ia"          "ui"          "ɲ̥"          
    ##  [261] "tɬʼ"         "t̪s̪ʼ"         "t̠ʃʷ"         "tɕ"         
    ##  [265] "b̰"           "cʼ"          "ɣʷ"          "nʷ"         
    ##  [269] "a̰"           "j̥"           "sʲ"          "øː"         
    ##  [273] "yː"          "d̠ʒː"         "t̠"           "bv"         
    ##  [277] "ʔʷ"          "ʔʲ"          "m̤"           "ḭ"          
    ##  [281] "ns"          "o̤"           "fʲ"          "ua"         
    ##  [285] "zː"          "ɗ̪"           "dʷ"          "ei"         
    ##  [289] "qʷʼ"         "oi"          "ɠ"           "ʈʂʰ"        
    ##  [293] "ɡ̤"           "lʷ"          "ə̃ː"          "i̤"          
    ##  [297] "e̤"           "ḛ"           "ɱ"           "tʃʰ"        
    ##  [301] "o̰"           "w̃"           "n̠t̠ʃ"         "d̠ʒʷ"        
    ##  [305] "ṵ"           "ɾʲ"          "ɲʷ"          "ʔː"         
    ##  [309] "n̰"           "a̤"           "ʌː"          "ʌ̃"          
    ##  [313] "ṳ"           "l̤"           "tʃʼ"         "˦˧"         
    ##  [317] "uə"          "t̪ː"          "t̪ʲ"          "m̩"          
    ##  [321] "d̤"           "˥˦"          "ou"          "ɔ̤"          
    ##  [325] "zʷ"          "ɲ̟"           "mbʷ"         "ʁʷ"         
    ##  [329] "s̻"           "sʼ"          "ɵ"           "tɬ"         
    ##  [333] "jʷ"          "l̰"           "ɟː"          "vʲ"         
    ##  [337] "rʲ"          "dʑ"          "k̰"           "ɔi"         
    ##  [341] "kǃ"          "d̠ʒ̤"          "u̞"           "bʱ"         
    ##  [345] "kʼʲ"         "p̰"           "l̠"           "ʑ"          
    ##  [349] "ɗː"          "kʼː"         "ʈː"          "ɱf"         
    ##  [353] "ʀ"           "n̩"           "t̠ʃʼː"        "ie"         
    ##  [357] "r̥"           "ɛ̤"           "t̰"           "ʏ"          
    ##  [361] "t̪ɬ̪ʼ"         "a̟ː"          "rʷ"          "l̪̥"          
    ##  [365] "ɓː"          "n̪̥"           "d̰"           "iə"         
    ##  [369] "ɑ̃ː"          "ˀj"          "kǀ"          "tʼː"        
    ##  [373] "ɓ̥"           "ʒː"          "ˀm"          "lˠ"         
    ##  [377] "ə̆"           "hː"          "d̪ː"          "ʈʂʼ"        
    ##  [381] "tɬʰ"         "l̪ˠ"          "r̃"           "↓˦"         
    ##  [385] "ɽ̤"           "n̪̰"           "ɒː"          "xː"         
    ##  [389] "ɯ̞"           "j̃"           "ˀn"          "ɵ̞"          
    ##  [393] "œː"          "tsʷ"         "*R"          "ŋ̩"          
    ##  [397] "ɤ̞"           "l̪ʲ"          "ʉː"          "n̤"          
    ##  [401] "t̠ʃʲ"         "n̪̤"           "iu"          "dʱ"         
    ##  [405] "βʲ"          "˩˨"          "ɖʐ"          "˧˩"         
    ##  [409] "qʷʰ"         "jˀ"          "s̪ʲ"          "ɢʷ"         
    ##  [413] "ɨ̃ː"          "ʒʷ"          "ˀw"          "\u1d91"     
    ##  [417] "ʃʼ"          "ʃʲ"          "˦˥"          "ndʲ"        
    ##  [421] "ɤː"          "qː"          "vʷ"          "cː"         
    ##  [425] "ɐː"          "d̪ʲ"          "ã̰"           "nts"        
    ##  [429] "ŋɡʲ"         "pʲʰ"         "ɒ̃"           "ɖː"         
    ##  [433] "kǃʰ"         "t̪ɬ̪"          "s̪ʼ"          "ʐ͇"          
    ##  [437] "ɺ̠"           "kǁ"          "ie̞"          "uo"         
    ##  [441] "ŋkʷ"         "pʼː"         "tsʲ"         "ã̟"          
    ##  [445] "zʲ"          "əi"          "ŋǃ"          "d̪ʱ"         
    ##  [449] "wʲ"          "t̠ʃ̰"          "kʲʼ"         "ɗ̥"          
    ##  [453] "l̪̰"           "ĭ"           "sʰ"          "sˀ"         
    ##  [457] "sˤ"          "kǀʰ"         "ˀb"          "kˀ"         
    ##  [461] "˧˦"          "i̤ː"          "ɯa"          "χː"         
    ##  [465] "\u1d05"      "ndʷ"         "e̞i"          "ɤ̃"          
    ##  [469] "ḭ̃"           "q̰"           "cɕ"          "qχ"         
    ##  [473] "cʷ"          "r̤"           "ṳː"          "mʱ"         
    ##  [477] "mˀ"          "a̤ː"          "kʲʰ"         "nˀ"         
    ##  [481] "ɨi"          "nʱ"          "wˀ"          "dˤ"         
    ##  [485] "d̠"           "tɕʰ"         "ea"          "ɯə"         
    ##  [489] "iˑ"          "ɯ̤"           "oˑ"          "ă"          
    ##  [493] "aˑ"          "s̪ː"          "tr"          "ɛ̤ː"         
    ##  [497] "n̪t̪"          "tˤ"          "ɔ̰"           "o̞i"         
    ##  [501] "d̠ʒʲ"         "t̻"           "ɸʲ"          "ŭ"          
    ##  [505] "ɡǀ"          "ɡǃ"          "t̠ʃʷʰ"        "uˑ"         
    ##  [509] "pˀ"          "l̃"           "ˀl"          "n̪ʲ"         
    ##  [513] "tsː"         "lˤ"          "ts̰"          "zˤ"         
    ##  [517] "dr"          "æ̃ː"          "ɔ̤ː"          "ŋʲ"         
    ##  [521] "n̻"           "c̟"           "vː"          "əu"         
    ##  [525] "t̪θ"          "˨˦˨"         "ʕː"          "t̠ʃˀ"        
    ##  [529] "mbʲ"         "i̥"           "d̃"           "iɛ"         
    ##  [533] "o̤ː"          "eˑ"          "ħː"          "ɖʱ"         
    ##  [537] "t̪θʼ"         "kǀ\u0353"    "eu"          "ɜ̃"          
    ##  [541] "kǁʰ"         "dzʷ"         "dzː"         "ʰt"         
    ##  [545] "ʰk"          "aˤ"          "ɳɖ"          "ɛ̰"          
    ##  [549] "kxʰ"         "ɮ̪"           "oa"          "ø̞"          
    ##  [553] "tx"          "k͉"           "t̻s̻"          "ŋmkp"       
    ##  [557] "d̠ʒʱ"         "o̞u"          "t͉"           "kx"         
    ##  [561] "u̥"           "t̠ʃʷʼ"        "b̃"           "p͉"          
    ##  [565] "t̪ɬ̪ʰ"         "kpʷ"         "l̻"           "ɳɖʐ"        
    ##  [569] "ŋǀ\u0353"    "d̪ɮ̪"          "pfʰ"         "e̝"          
    ##  [573] "ɘː"          "h̃"           "˦˨˦"         "↓"          
    ##  [577] "ŋ̰"           "ao"          "e̤ː"          "ɲʒ"         
    ##  [581] "e̞ˤ"          "ndr"         "ɲ̤"           "n̠ʃ"         
    ##  [585] "ŋǁ"          "ŋǀ"          "ʔˤ"          "ɯ̃ː"         
    ##  [589] "t̪ˤ"          "t̪s̪ʲ"         "d̪̰"           "n̠d̠"         
    ##  [593] "t̠ʃʲʰ"        "ɐ̃"           "r̠"           "ð̞"          
    ##  [597] "k̰ʷ"          "t̻ʰ"          "ɽ̃"           "ɽʱ"         
    ##  [601] "kǁ\u0353"    "ɪ̈"           "ɨ̞"           "dɮ"         
    ##  [605] "zˤː"         "ɻʲ"          "z̻"           "d̻"          
    ##  [609] "w̤"           "ɥ̃"           "βʷ"          "kǃ̠"         
    ##  [613] "õ̰"           "t̪θʰ"         "β̃"           "xʼ"         
    ##  [617] "ë"           "ɑ̤"           "ao̞"          "ɤ̞̃"          
    ##  [621] "kǀʼ"         "tsʼː"        "ʰp"          "ɴ"          
    ##  [625] "ʱm"          "ɛi"          "ɠɓ"          "aɛ"         
    ##  [629] "ɤ̤"           "cçʼ"         "ɟʝʷ"         "ae"         
    ##  [633] "kʷː"         "k̟"           "tʲʰ"         "kʰː"        
    ##  [637] "kǂ"          "mpʰ"         "kpʲ"         "ɡǁ"         
    ##  [641] "dˤː"         "tˤː"         "ps"          "uˤ"         
    ##  [645] "ɡ̰"           "ɬʲ"          "ue"          "kpʰ"        
    ##  [649] "sˤː"         "ˀɲ"          "ɵ̞ː"          "aɪ"         
    ##  [653] "ɾ̤"           "ɾʷ"          "ʊ̤"           "t̠ʃʰː"       
    ##  [657] "ə˞"          "t̠ʃ͉"          "v̤"           "ɟʷ"         
    ##  [661] "ae̞"          "ʈʼ"          "n̠ʒ"          "ṵ̃"          
    ##  [665] "ɲ̰"           "ntʷ"         "rˤ"          "ɡbʲ"        
    ##  [669] "d̪ˤ"          "t̪̰"           "l̪̤"           "t̪s̪ː"        
    ##  [673] "t̪ˀ"          "õ̞ː"          "t̪ʷ"          "ŋ̥ǃ"         
    ##  [677] "r̩"           "˧˦˧"         "d̪ð"          "kʰʷ"        
    ##  [681] "ẽ̞ː"          "ŋkʰ"         "ɡǃx"         "ʌ̃ː"         
    ##  [685] "ʕʷ"          "ɗʲ"          "ʕ̝"           "nɗ"         
    ##  [689] "nɡ"          "ɡ̤ǃ"          "cɕʰ"         "ɨ̆"          
    ##  [693] "ɡ̤ʷ"          "i̞"           "ï"           "ɻʷ"         
    ##  [697] "iˠ"          "iˤ"          "s̺"           "d̤z"         
    ##  [701] "ɨ˞"          "ɡ\u1da3"     "ʁ̞"           "ʰt̠ʃ"        
    ##  [705] "çʷ"          "ɯi"          "iaː"         "e̯"          
    ##  [709] "xʲ"          "kǃʼ"         "ʈr"          "ɴq"         
    ##  [713] "eo"          "kǃx"         "ɯ̰"           "ɖr"         
    ##  [717] "ɳʈ"          "ja"          "a˞"          "ʱɾ"         
    ##  [721] "ö"           "aɯ"          "o̝"           "r̠ʲ"         
    ##  [725] "ɛ̆"           "kxʼ"         "q̰ʷ"          "z̪ˤ"         
    ##  [729] "cçʷ"         "oe"          "z̪ʲ"          "e̞o̞"         
    ##  [733] "ɮʲ"          "n̠d"          "s̪ˤ"          "kǀ\u0353ʰ"  
    ##  [737] "ḛ̃"           "s̪ʰ"          "s̪ʷ"          "\u1d91ʼ"    
    ##  [741] "o̞ˤ"          "ɺ̪"           "tˀ"          "tːs"        
    ##  [745] "üː"          "ɔ̝"           "tʲʼ"         "ɔ̆"          
    ##  [749] "fʼ"          "kǁʼ"         "æi"          "t̺"          
    ##  [753] "ɡǂ"          "ŋǁ\u0353"    "ʄ̥"           "nsʷ"        
    ##  [757] "mʷˠ"         "*R̪̰"          "˦↓˦"         "ɵː"         
    ##  [761] "bz"          "ɬʼ"          "ɡ̟"           "œ̃"          
    ##  [765] "n̪d̪z̪"         "ŋ̥ǀ\u0353ˀ"   "ˀd"          "ɟ̤"          
    ##  [769] "ɪ̃ː"          "lʼ"          "d̻z̻"          "l̩"          
    ##  [773] "t̪s̪ʷʰ"        "t̪s̪ʷʼ"        "ɵ̞̆"           "ø̞ː"         
    ##  [777] "ndʑ"         "ŋɣ"          "e̞u"          "cʼː"        
    ##  [781] "ɟ̰"           "ɲc"          "ə̰"           "tç"         
    ##  [785] "r̪ʲ"          "c̤"           "c̰"           "qχʷ"        
    ##  [789] "ɟʲ"          "ŋǃ̠"          "ɬ̪ː"          "tsʷʰ"       
    ##  [793] "cʲ"          "ɡbː"         "˥̰"           "ɡbʷ"        
    ##  [797] "bʷˠ"         "dʑ\u1da3"    "ʊ̙ː"          "l̪ː"         
    ##  [801] "ʁʷˤ"         "ɲ\u1da3"     "t̪ɦ"          "˥˧"         
    ##  [805] "uai"         "nzʷ"         "r̝"           "ɣʲ"         
    ##  [809] "ʈʰː"         "ʉ̃"           "ɔ̃ĩ"          "˨˦˧"        
    ##  [813] "ŋ̥ʷ"          "ŋ̥ʲ"          "n̠d̠ʒʷ"        "æ̤ː"         
    ##  [817] "uaː"         "mɓ"          "ŋ̤ǃ"          "t̪s"         
    ##  [821] "ṳˑ"          "mʼ"          "ʱn̪"          "ŋkʲ"        
    ##  [825] "n̪ʷ"          "ɽ̩"           "d̤̥"           "dl"         
    ##  [829] "˨˩"          "dx"          "ɪˤ"          "ʃ͈ː"         
    ##  [833] "n̠t̠ʃʼ"        "n̪ː"          "l̥ˠ"          "ɾ̥ˠ"         
    ##  [837] "a̤ˑ"          "l̥ʲ"          "ɗʒ"          "t͈ː"         
    ##  [841] "ɗʷ"          "ɕ\u1da3"     "nˠ"          "e̤ˑ"         
    ##  [845] "ʎ̟"           "ɨa"          "nʼ"          "io"         
    ##  [849] "nɟ"          "ɪ̤"           "ɪ̙"           "wʱ"         
    ##  [853] "b̤̥"           "dɾ"          "t̪n̪"          "dʼ"         
    ##  [857] "kǁx"         "dˠ"          "ɡǂx"         "i̜"          
    ##  [861] "dˀ"          "ʙ"           "mbv"         "ɡ̤̥"          
    ##  [865] "ɒ̤"           "ɨə"          "ɒ̆"           "w̥"          
    ##  [869] "qχʷʼ"        "nh"          "nl"          "iɔ"         
    ##  [873] "b̤ʲ"          "nr"          "ɥ̥"           "ħʷ"         
    ##  [877] "eˠ"          "ʃʷː"         "iã"          "ɡǀ\u0353"   
    ##  [881] "eɪ"          "i̤ˑ"          "ɡǁx"         "ə̰̃"          
    ##  [885] "çʲ"          "˩̰"           "ɡ̃"           "tʰʷ"        
    ##  [889] "kǀx"         "ʁː"          "ʁˤ"          "e̠"          
    ##  [893] "ĕ"           "n̥ʲ"          "n̪t̪s̪"         "ʂ͇"          
    ##  [897] "n̥ˠ"          "s̰"           "ɤ̞ː"          "ɜː"         
    ##  [901] "s͉"           "iau"         "ɡǀx"         "m̤b̤"         
    ##  [905] "ɯ\u0353"     "ɠ̥"           "z͇̪"           "dzʱ"        
    ##  [909] "β̞ː"          "ʰw"          "oˤ"          "ʰd"         
    ##  [913] "ʰn"          "ju"          "ʌi"          "ä"          
    ##  [917] "z̞̩"           "t̻s̻ʼ"         "ʒʲ"          "ɲɟʑ"        
    ##  [921] "aʊ"          "o̯"           "dz̤"          "kǂx"        
    ##  [925] "k\u1da3"     "ntsʼ"        "ɛ̝"           "ɟʝː"        
    ##  [929] "s̻ː"          "cçː"         "ɛə"          "kǂʰ"        
    ##  [933] "ẽ̞ĩ"          "ɞ"           "aj"          "aw"         
    ##  [937] "ay"          "kʷˀ"         "ɳʈʂ"         "aʲ"         
    ##  [941] "æ̤"           "tn"          "tl"          "ɛ̤ˑ"         
    ##  [945] "ɺʲ"          "ŏ̞"           "k̃"           "kǁxʼ"       
    ##  [949] "k̙"           "k͈"           "kʘ"          "ỹ"          
    ##  [953] "aɨ"          "kˤ"          "ɔu"          "cʷʰ"        
    ##  [957] "æˤ"          "kɦ"          "˧˥"          "tɾ"         
    ##  [961] "ɵ̤"           "mpʲ"         "ɔˤ"          "ɪ̈ː"         
    ##  [965] "tˠ"          "kŋ"          "t̪s̪ʼː"        "\u2c71̟"     
    ##  [969] "t͈"           "d̠ʒʼ"         "o̞a"          "lˤː"        
    ##  [973] "ɔɪ"          "ɸʷˠ"         "ɸʼ"          "ɸʷ"         
    ##  [977] "kf"          "u̜"           "aˤː"         "ɔ̝ː"         
    ##  [981] "k͈ː"          "s͈ː"          "bˀ"          "˥˧˥"        
    ##  [985] "ɦ̤"           "χʷˤ"         "ɦʷ"          "bβ"         
    ##  [989] "tz̤̥"          "χʷː"         "kǀxʼ"        "i̞ː"         
    ##  [993] "pm"          "uɔ"          "pʷˠʰ"        "˧˨˥"        
    ##  [997] "˧˨˧"         "ɓʲ"          "ɓʷ"          "b̪"          
    ## [1001] "uʌ"          "ɡv"          "ŋ̥ǃˀ"         "dʲʷ"        
    ## [1005] "ʊ̤ː"          "n̤d̤ɮ̤"         "p͈"           "ˀŋ"         
    ## [1009] "ë̞"           "sʼː"         "ɬː"          "pɦ"         
    ## [1013] "ɛ̝ː"          "ʊ̃ː"          "ɡʼ"          "uj"         
    ## [1017] "qχʼː"        "ɡɣ"          "ŋ̥ǀ\u0353ʰ"   "ʒˠ"         
    ## [1021] "k̟ʰ"          "tɕ\u1da3"    "ŋ̥ǁ\u0353ˀ"   "ɭ̥"          
    ## [1025] "ˀr"          "ˀp"          "ts̪"          "ɰ̰"          
    ## [1029] "ʰkʷ"         "lˀ"          "kǃxʼ"        "kpː"        
    ## [1033] "lʱ"          "ts͇"          "n̤d̤"          "s\u1da3"    
    ## [1037] "õ̞ĩ"          "χˤ"          "kǁ\u0353ʰ"   "ũĩ"         
    ## [1041] "ɘ̃"           "ʃˠ"          "n̪t̪ʰ"         "ðʲ"         
    ## [1045] "ɾ̥"           "wə"          "ɹ̪"           "ɑi"         
    ## [1049] "ɾˠ"          "ɾˤ"          "ɾˀ"          "ɾʱ"         
    ## [1053] "ʃ͉"           "ø̞ˤ"          "ŋ̥ǃ̠ˀ"         "z͇"          
    ## [1057] "z̤"           "ʊ̙"           "kǃ̠ʰ"         "ʊ̈"          
    ## [1061] "n̪z̪"          "ãˤ"          "ɔ̤ˑ"          "u̞ː"         
    ## [1065] "wa"          "˨˥˩"         "ɲj"          "ʊi"         
    ## [1069] "ə̯"           "ə̤"           "ndɾ"         "ŋ̤"          
    ## [1073] "ʌ̤"           "ɤ̟"           "pʰː"         "ʊɪ"         
    ## [1077] "ɾ̥ʲ"          "nd̪"          "ɗʱ"          "ʊˤ"         
    ## [1081] "ntʰ"         "n̠̥"           "kʼʷ"         "ɲʱ"         
    ## [1085] "ɡʲʷ"         "t̪ʰː"         "qχʼ"         "ɟʑ"         
    ## [1089] "ṽ"           "ɟʱ"          "ĕ̞"           "vʱ"         
    ## [1093] "ʊu"          "ɬ̪ʼ"          "ɬ̪ʲ"          "ʊa"         
    ## [1097] "l̠˞"          "ŋǂ"          "ʈɳ"          "ɲ̩"          
    ## [1101] "kǂxʼ"        "ntʼ"         "ĩˠ"          "˨˩̤"         
    ## [1105] "ɐˤ"          "ɡbˤ"         "d̪ʷ"          "˨˧˨"        
    ## [1109] "˨˧˩"         "tçʰ"         "ɣ̩"           "ao̞ˤ"        
    ## [1113] "ɣ̤"           "t̰s̰"          "t̪ɬ̪ʷʰ"        "ɑ̈"          
    ## [1117] "ṳə̤"          "ɣ̰"           "t̪ɬ̪ʷʼ"        "rʱ"         
    ## [1121] "rʼ"          "kǃ̠xʰ"        "ɟ̩̰"           "ɯ̃ə"         
    ## [1125] "rˀ"          "pʃʼ"         "pʃʰ"         "bvʷ"        
    ## [1129] "bʷː"         "ĩə"          "ɡbɾ"         "ʰs̪"         
    ## [1133] "ntɾ"         "ɑ̃ɔ̃"          "ˀʍ"          "l̪̩"          
    ## [1137] "hv"          "ɡǃxʼ"        "sʲʷ"         "d̪ɦ"         
    ## [1141] "ŋ̤ʱ"          "ɶ̝"           "ɖʐʷ"         "e̥ː"         
    ## [1145] "õ̞ˤ"          "ṳa"          "ŋkx"         "ɛ̃ə"         
    ## [1149] "l̪ˤ"          "n̠̤d̠̤ʒ"         "ʉi"          "ʁ̞̰ʷ"         
    ## [1153] "ɡm"          "χʷˤː"        "t̪ʃ"          "ɪ̯"          
    ## [1157] "nzɾ"         "mv"          "ɴqʰ"         "t̪ʙ"         
    ## [1161] "˥˨"          "d̪̚"           "ʔ̬"           "tɬ̰"         
    ## [1165] "t̪s̪ɦ"         "ɣˀ"          "ɔ̰ˑ"          "ɔ̰ː"         
    ## [1169] "ɣː"          "ŋ̥ǂ"          "ŋ̥ǁ"          "ŋ̥ǀ"         
    ## [1173] "ɣ̥"           "oe̞"          "ʱɾ̪"          "r̰"          
    ## [1177] "ɡb̩"          "r̺"           "ŋ̤ǂ\u0353ˡ"   "ɛ̯"          
    ## [1181] "uʰ"          "m̪"           "ˀn̪̥"          "ʉ̟"          
    ## [1185] "ʉ̞"           "m͉"           "ŋ̥ǀ\u0353xˀ"  "ʉ̆"          
    ## [1189] "ɪ̠"           "ɐu"          "ɔ̤i̤"          "ŋ̥ʘ"         
    ## [1193] "h\u1da3"     "ɐi"          "ˀl̪̥"          "ɡbr"        
    ## [1197] "x͈ː"          "d̠ʒxʼ"        "ŋh\u1da3"    "ŋʲʷ"        
    ## [1201] "x͈ʷ"          "n̠t̠ʃɾ"        "ɡʘkxʼ"       "ɱð"         
    ## [1205] "ɲ̤ʱ"          "d̪x"          "ɫ"           "d̪ⁿ"         
    ## [1209] "uã"          "ɡ̤ǁ"          "ʕ̰ʷ"          "ŋkɾ"        
    ## [1213] "t̻ː"          "ts͇ʰ"         "t̻ʲ"          "t̻ʼ"         
    ## [1217] "kʰʲ"         "mɦ"          "au̯"          "ɡʘqʰ"       
    ## [1221] "mɾ"          "dz̪"          "a\u1d31"     "ŋ̤ǀ"         
    ## [1225] "ŋ̤ǁ"          "ŋ̤ǂ"          "t̪x"          "ãĩ"         
    ## [1229] "ɤ̃ː"          "ɡʘx"         "mˤ"          "mˠ"         
    ## [1233] "a̰ː"          "a̰ˑ"          "\u1d91ʼː"    "ṳa̤"         
    ## [1237] "kʘxʼ"        "ʈɹ̠̥"          "*R̪̥"          "ɪʌ"         
    ## [1241] "ɘ̃ː"          "dⁿ"          "ʒ̺"           "ɪˠ"         
    ## [1245] "o̞̟"           "˨ː"          "nθ"          "n͉"          
    ## [1249] "ˀj̥"          "ʎʼ"          "õ̞ũ"          "ʎʱ"         
    ## [1253] "n̠t̠ʃʷ"        "l̥ː"          "ɡǃkxʼ"       "əɨ̙"         
    ## [1257] "ɛ̙ː"          "ʕʼ"          "we"          "wo"         
    ## [1261] "ɨ̘"           "wi"          "ɪɛ"          "ɨ̞̃"          
    ## [1265] "ŋ̥ǂxˀ"        "l͈ː"          "ɳɖr̠"         "u\u1d31"    
    ## [1269] "ʎ̥"           "t̠ʃʱ"         "mbɾ"         "iæ"         
    ## [1273] "nˤ"          "mpfʼ"        "t̪ˠʰ"         "t̠ʃˠ"        
    ## [1277] "ʕ̰"           "ɗ̠"           "ɡ‼"          "ɪ̝"          
    ## [1281] "nɬ"          "nɮ"          "ŋ̥ǂ\u0353ˡʰ"  "nɦ"         
    ## [1285] "nɾ"          "ɪ̆"           "mbː"         "ˀʃʷ"        
    ## [1289] "ˀj̰"          "ɡ̤ǂ"          "ɡ̤ǀ"          "ŋ̥ǂ\u0353ˡˀ" 
    ## [1293] "kǃʰʼ"        "t̠ʃɾ"         "ɨ̰"           "cɕʼ"        
    ## [1297] "ɮ̪ʲ"          "ɡ͈ː"          "ŋɡː"         "ɡǂ\u0353ˡx" 
    ## [1301] "θ̰"           "wʼ"          "n̠t̠ʃʰ"        "t͈sʷ"        
    ## [1305] "ɨ̥"           "ɑ̈ː"          "ɨ̜"           "ai̯"         
    ## [1309] "n̤s"          "wˤ"          "t͈s"          "t̪s̪ˀ"        
    ## [1313] "ʰts"         "ã̟ĩ"          "ɛ̰̃"           "w˞"         
    ## [1317] "tsʰʷ"        "ɾ̪ˠ"          "i̤a̤"          "i\u0353"    
    ## [1321] "i̤ɛ̤"          "˧˩̤"          "kǁʼʰ"        "ɾ̪ʲ"         
    ## [1325] "pkʰ"         "ɡ̤ː"          "ɑ̃õ"          "ɪi"         
    ## [1329] "i̙"           "i̘"           "tsʰː"        "pʼkxʼ"      
    ## [1333] "ɴɢǁqʰ"       "ŋmˤ"         "d̩"           "ʊ̈ː"         
    ## [1337] "d̺"           "mbz"         "õ̞ãˤ"         "d̞"          
    ## [1341] "nð"          "ɡǃ̠"          "iʰ"          "d̪l̪"         
    ## [1345] "ŋmʲ"         "ˀw̰"          "d͈"           "ɒ̝"          
    ## [1349] "ɡ̰ǂ\u0353ˡx"  "iʊ"          "iʌ"          "qʼʷ"        
    ## [1353] "iɨ"          "x̟ʲ"          "ɔ̃ˤː"         "kʘx"        
    ## [1357] "ëː"          "fʷː"         "ẅ"           "ɛ̰ː"         
    ## [1361] "ɛ̰ˑ"          "w̞"           "qχʷˤ"        "d̠ʒ̤ː"        
    ## [1365] "b̤ː"          "ɺ̠̰"           "t̻ʰː"         "ŋɡ̟"         
    ## [1369] "l\u1da3"     "o̤ˑ"          "ɨˑ"          "eˤ"         
    ## [1373] "t͉s"          "ɱvɾ"         "kǃ\u0353"    "x͈"          
    ## [1377] "eˀ"          "æẽ̯"          "χˤː"         "x̥"          
    ## [1381] "ia̟"          "ɣ̟ʲ"          "kxʷʰ"        "t̪ʼkxʼ"      
    ## [1385] "ɡ̩ʷ"          "ɢǃ"          "kǃ̪"          "ɣ͈ː"         
    ## [1389] "ˀʍʲ"         "k‼ʼ"         "k‼ʰ"         "ɜi"         
    ## [1393] "ow"          "ɴː"          "ˀc"          "mw"         
    ## [1397] "ɖ̰"           "mːʷˠ"        "eə"          "eɘ"         
    ## [1401] "çː"          "mˤː"         "ɡǂkxʼ"       "ʁ̞ʷ"         
    ## [1405] "qʘʰ"         "l̻ː"          "tɕʷ"         "ɡǃʱ"        
    ## [1409] "l̪ʷ"          "lʲʷ"         "l̻̰"           "j̥ʷ"         
    ## [1413] "ɖr̠\u0353"    "ɪa"          "ˀkp"         "dzʲ"        
    ## [1417] "kʘʼ"         "qʘʼ"         "kʘʰ"         "jeu̯"        
    ## [1421] "a̤i̤"          "ḛː"          "ɪã"          "t̠ːʃ"        
    ## [1425] "oʲ"          "tʰʲ"         "ç̟"           "tʰʼ"        
    ## [1429] "ɡǀʱ"         "f͈ː"          "ɑu"          "e̥"          
    ## [1433] "ɑe"          "ɑo"          "iɛː"         "ɖɦ"         
    ## [1437] "o˞"          "st"          "e̙"           "ɖɽ"         
    ## [1441] "ɯʌ"          "tsʲʰ"        "u˞"          "n̥ʃ"         
    ## [1445] "qˤʰ"         "qˤʼ"         "s̪ʷː"         "kǀ̪"         
    ## [1449] "m̤b̤v̤"         "ŋ̥ǃˠˀ"        "ɪ̙ˠ"          "ɾ̪̊ʰ"         
    ## [1453] "ɪ̙ː"          "pːʷˠ"        "k‼"          "ej"         
    ## [1457] "r͈ː"          "õʲ"          "ɐ̹̆"           "z͈ː"         
    ## [1461] "ɑ̝"           "k‼x"         "qʼː"         "jai̯"        
    ## [1465] "ɡǀqʰ"        "n̠ʃʷ"         "õˤ"          "t͉ʲ"         
    ## [1469] "ʰj"          "s͈"           "m̤pʰ"         "xh"         
    ## [1473] "n̠̩d̠ʒ"         "ɡǁkxʼ"       "ŋ̤ɡ̤"          "ĩẽ̞"         
    ## [1477] "tʷʰ"         "ɑʊ"          "kɡ"          "o̤i̤"         
    ## [1481] "iˀ"          "e̘ː"          "sˠ"          "wai̯"        
    ## [1485] "ɯ̆"           "ɡǀ\u0353x"   "ˀw̥"          "ŋ̩ʷ"         
    ## [1489] "d̻ʲ"          "ɡǁ\u0353"    "kǀʷ"         "ʂʼ"         
    ## [1493] "ɐ̆"           "˥˧̰"          "ɤ̤ː"          "d̻ː"         
    ## [1497] "sɾ"          "psʷʰ"        "ˀkʷ"         "ŋɣʲ"        
    ## [1501] "ɖⁿ"          "oʊ"          "n̪s̪"          "ʟ"          
    ## [1505] "β̞ʲ"          "ɱfʷ"         "ɱfʲ"         "ʋː"         
    ## [1509] "dzʼ"         "oˀ"          "ʌ̤ː"          "dzˠ"        
    ## [1513] "ʰq"          "ʰb"          "ʰc"          "r̠̙"          
    ## [1517] "β̞ˠ"          "ʱl"          "ɡǀkxʼ"       "øy"         
    ## [1521] "psʰ"         "psʷ"         "ɳʈʂʰ"        "ɳ̥"          
    ## [1525] "ɳ̤"           "a̠"           "ḁ"           "ui̯"         
    ## [1529] "ɵ̤ː"          "jo"          "oɛ"          "ɡʘ"         
    ## [1533] "je"          "ɬʲʼ"         "ɛ̃ɔ̃"          "t̪ɬ̪ʼː"       
    ## [1537] "oɪ"          "r\u0353̪"     "å"           "ɬ̻"          
    ## [1541] "kʟ̥\u0353ʼ"   "ɹ̰ˤ"          "a̙"           "qǂʰ"        
    ## [1545] "aˠ"          "i̤ə̤"          "oɘ"          "qǂʼ"        
    ## [1549] "t̻s̻ʰ"         "aˀ"          "ɛw"          "ɛu"         
    ## [1553] "ŋmkpɾ"       "ɳʱ"          "ɛj"          "ɛo"         
    ## [1557] "ɲɟʝ"         "aʰ"          "jau̯"         "ʱŋ"         
    ## [1561] "rʼː"         "ɢˤ"          "dz̃"          "ŏ"          
    ## [1565] "o̟"           "pʷˠ"         "o̘"           "ɖ̤ː"         
    ## [1569] "o̞aˤ"         "hʲʷ"         "aɔ"          "m̃ʷ"         
    ## [1573] "ɡɾ"          "ɛ̆̃"           "ɛ̘ː"          "ɪe̞"         
    ## [1577] "a̰ṵ"          "õũ"          "↓˦˨"         "ɠ̥ʷ"         
    ## [1581] "ɳɦ"          "ɠ̥ʲ"          "ei̙"          "l̠̤"          
    ## [1585] "jʱ"          "eĩ"          "jʔ"          "qʷˀ"        
    ## [1589] "ɛ̙ˠ"          "ei̯"          "qʷː"         "ɡǂqʰ"       
    ## [1593] "ŏ̃"           "ɛ̙"           "t̠ʃʷː"        "ɛ̞"          
    ## [1597] "jˤ"          "n̤ɡ̤"          "ɯ̞̯"           "pʰʷ"        
    ## [1601] "oi̯"          "ʰʃ"          "kxʷ"         "d̪z̪̤"         
    ## [1605] "øɘ"          "ɢː"          "ɑ̃˞"          "ɬ̪ʲʼ"        
    ## [1609] "ˀm̥"          "oj"          "jɛ"          "jɔ"         
    ## [1613] "ɮ̤"           "kxː"         "ʼŋǀ"         "ʌə"         
    ## [1617] "ɲcɕ"         "ɡ̰ǃx"         "t̠ʃx"         "ãˤː"        
    ## [1621] "ẽ̞̰"           "ɡǁqʰ"        "ɡǂ\u0353ˡ"   "kǂʼ"        
    ## [1625] "ɛɪ"          "ɛɯ"          "õ̞ĩˤ"         "õ̞ã"         
    ## [1629] "b̤v̤"          "ɛˑ"          "ʌ̯"           "kǃxʰ"       
    ## [1633] "kʷʱ"         "fʰ"          "n̻ʲ"          "j̟"          
    ## [1637] "ɛˠ"          "n̻ː"          "qǃʰ"         "ø̠"          
    ## [1641] "qǃʼ"         "d̠͈ʒ"          "ɛʊ"          "s̻θ"         
    ## [1645] "ɲcʼ"         "ø̆"           "ɲcʲ"         "a̤ṳ"         
    ## [1649] "s̪ˀ"          "yø̞"          "ɛo̞"          "kǀ\u0353ˀ"  
    ## [1653] "qχʷˤʼ"       "ɺː"          "mpf"         "o̞̰"          
    ## [1657] "m̥ʷ"          "m̥ʰ"          "d̠ⁿ"          "o̞̥"          
    ## [1661] "ɹ̪̥"           "yə"          "ö̞"           "ʐ̰"          
    ## [1665] "d̠̤ʒ̤"          "kʷʼː"        "yʼ"          "\u1d91ː"    
    ## [1669] "o̰ː"          "o̰ˑ"          "ɴɢǀqʰ"       "ʔw"         
    ## [1673] "k̻"           "lɦ"          "o\u1d31"     "ɡǃqʱ"       
    ## [1677] "a̟̙ː"          "n͈ː"          "ɪ̝ː"          "ʐʷ"         
    ## [1681] "y̤"           "t̠͈ʃʷ"         "bzʷ"         "ɡ̤ǂ\u0353ˡ"  
    ## [1685] "t̠ʃ̺ʰ"         "y̆"           "ˀt"          "ʑ\u1da3"    
    ## [1689] "ɑ̤ː"          "ʼn"          "æɪ"          "ʼm"         
    ## [1693] "ˀo̯"          "ɢǀ"          "ɢǁ"          "ɢǂ"         
    ## [1697] "ɴɢ"          "ɔ̙"           "ɾʲʷ"         "o̙"          
    ## [1701] "d̤z̤"          "ɔy"          "xʷː"         "ḛˑ"         
    ## [1705] "uːʌ"         "æʉ"          "ɺ̥"           "ɡ̰ǂx"        
    ## [1709] "xʷʼ"         "kɬ"          "kǁ\u0353xʰ"  "kɾ"         
    ## [1713] "n̪d̪ʷ"         "n̪d̪ʲ"         "ɘi"          "ɢʱ"         
    ## [1717] "ɵ̃"           "n̠̥t̠ʃ"         "eʰ"          "tɦ"         
    ## [1721] "eʲ"          "ɢʘ"          "k̟ʲʰ"         "ye"         
    ## [1725] "mpɾ"         "q͈ʷ"          "ɸ̃"           "eʊ"         
    ## [1729] "ɸ̞"           "f̰"           "d̠̰"           "kǀ\u0353x"  
    ## [1733] "mpʷ"         "m̥p"          "mpʼ"         "kʷɾ"        
    ## [1737] "ɣv"          "t̺s̺"          "ɔ̯"           "ʍʲ"         
    ## [1741] "ɱfɾ"         "ʕʷʼ"         "qǃ"          "tʱ"         
    ## [1745] "f͉"           "k̟ʲʼ"         "qχˤː"        "ʡ"          
    ## [1749] "ŋɡmb"        "kːʷ"         "ɔˑ"          "ɴɢǃqʰ"      
    ## [1753] "æu"          "d̠ʒˠ"         "qǁʼ"         "jãː"        
    ## [1757] "uːa"         "ɱvʲ"         "t̃"           "n̺"          
    ## [1761] "qχˤʼ"        "ɯ̤ː"          "ɲ̟ʝ̟"          "ɳɖɽ"        
    ## [1765] "bˤː"         "qǁʰ"         "ɔɛ"          "ɔə"         
    ## [1769] "ɠː"          "ɡʼkxʼ"       "ɸː"          "nɡɾ"        
    ## [1773] "rˤː"         "t̠ʰ"          "fɾ"          "ɸʰ"         
    ## [1777] "ɻː"          "\u1d05̪̰"      "ɡ̤ǀ\u0353"    "ə̘ː"         
    ## [1781] "km"          "kl"          "i̙ː"          "æe"         
    ## [1785] "ðː"          "u̙"           "u̘"           "ü"          
    ## [1789] "bʼ"          "iːɛ"         "k͈ʷ"          "ðˠ"         
    ## [1793] "ðˤ"          "bˤ"          "œy"          "bˠ"         
    ## [1797] "u\u0353"     "œi"          "eɔ"          "t̪ʷʰ"        
    ## [1801] "ntlʼ"        "uˤː"         "ãõ̞ˤ"         "˦˥̰"         
    ## [1805] "ʈʂ͇"          "z̞̩̃ˠ"          "d̪ʼkxʼ"       "˧˨̤"         
    ## [1809] "k̃ʷ"          "*Rʲ"         "bɦ"          "o̞ˤː"        
    ## [1813] "a̰ḭ"          "bɡ"          "bɾ"          "ŋɣ\u1da3"   
    ## [1817] "ŋǁʼ"         "d̺ː"          "pt"          "o̘ː"         
    ## [1821] "t̪ʲʰ"         "uɛ"          "t̪ʼː"         "ɯ̤i̤"         
    ## [1825] "χʷʼ"         "ɬ̪ʷː"         "ɲ̤cʰ"         "˧˨ˤ"        
    ## [1829] "n̤d̤z̤"         "uɨ"          "uɪ"          "ʈʂʷ"        
    ## [1833] "k‼ʰʼ"        "ɔõ̯"          "b̩"           "wei̯"        
    ## [1837] "fʃ"          "ð͉"           "ɡⁿ"          "b̻"          
    ## [1841] "˦˥˦"         "ʑː"          "tʼkxʼ"       "kpˤ"        
    ## [1845] "mʷː"         "z̞̩ˠ"          "ˀt̪s"         "ŋ̤kʰ"        
    ## [1849] "nˤː"         "uˀ"          "kʷʰː"        "ħ͈ː"         
    ## [1853] "kǂ\u0353ˡ"   "b͈"           "b͉"           "d͈ː"         
    ## [1857] "ʒ̤"           "õ̞̰"           "oːu"         "ŋ‼ʰ"        
    ## [1861] "iːe"         "ŋ̥ǂˀ"         "p̃"           "ɤ̞̟̃"          
    ## [1865] "ŋ̥ǂʰ"         "˧˩̰"          "p̻"           "ʒ͇"          
    ## [1869] "ʃ͈ʷ"          "p̪"           "kǀ\u0353ˠʰ"  "qǀʼ"        
    ## [1873] "ieː"         "qǀʰ"         "mbˠ"         "b̃ʷ"         
    ## [1877] "ɑ̃ĩ"          "˥˨˧"         "dlʷ"         "ɡ̩"          
    ## [1881] "br"          "bⁿ"          "ɯ̞̆"           "k͉ʷ"         
    ## [1885] "ɬʷ"          "ɡ̙"           "ɡˡ"          "ɡ‼xʼ"       
    ## [1889] "d̠ʒɾ"         "œ̤"           "pɸ"          "pɾ"         
    ## [1893] "iẽ"          "kǀʰʼ"        "uy"          "kǀʰʷ"       
    ## [1897] "ɾ̪̊"           "ɯ̤ə̤"          "ɡɦ"          "ŭ̃"          
    ## [1901] "m̤ʱ"          "ŋːʷ"         "ˀb͉"          "ɡɓ"         
    ## [1905] "pˤ"          "z̪ː"          "t̪ɬ̪ˀ"         "ˀt̪ɬ"        
    ## [1909] "t̪ɬ̪ː"         "pʱ"          "ʒˀ"          "ɭ̤"          
    ## [1913] "tsɦ"         "ˀk"          "ɭ̩"           "ˀs"         
    ## [1917] "ˀq"          "ʛ̥"           "p͉ʲ"          "ˀy"         
    ## [1921] "ŋ̥m̥"          "eu̯"          "ɨ̃ĩ"          "ɰ̃"          
    ## [1925] "χ͈ʷ"          "ɭ\u0353"     "ŋ̥ǁ\u0353ʰ"   "kpɾ"        
    ## [1929] "uei"         "tsˀ"         "tsˠ"         "ɳʈr̠̥"        
    ## [1933] "ʃt"          "ŋɣʷ"         "ˀɡb"         "ŋǂ\u0353ˡ"  
    ## [1937] "œu"          "lʔ"          "˧˥˨"         "χ͈"          
    ## [1941] "ɧ"           "t̠n̠"          "tsʱ"         "ũə"         
    ## [1945] "l͉"           "ɡ̰ǀ\u0353x"   "tsʷʼ"        "cçʷʰ"       
    ## [1949] "χʼ"          "kǂ\u0353ˡʰ"  "ɔ̙ː"          "ɡ̟ʲ"         
    ## [1953] "aː̃"          "k‼xʼ"        "ŋ̥ǂ\u0353ˡxˀ" "æe̯"         
    ## [1957] "ɲ̤ɟ̤"          "ẽĩ"          "ʔɾ"          "d\u1da3"    
    ## [1961] "t̪s̪ʷː"        "ɭʷ"          "ɤ̤i̤"          "kǁ\u0353ˀ"  
    ## [1965] "ou̯"          "z̪ʷ"          "ɭː"          "ɜɪ̯"         
    ## [1969] "oũ"          "ʼŋǃ"         "ou̙"          "b̤z̤"         
    ## [1973] "ɕʼ"          "dʼkxʼ"       "ʃˀ"          "zˠ"         
    ## [1977] "ˀɖ"          "ˀɗ"          "ˀɡʷ"         "ˀɟ"         
    ## [1981] "ld"          "ˀɡ"          "zˀ"          "ũˤ"         
    ## [1985] "˨˥"          "q̚"           "ˀd̪"          "kpr"        
    ## [1989] "ndzʷ"        "ndzʲ"        "ṳi̤"          "ɕː"         
    ## [1993] "ʃʰ"          "m͈ː"          "ə̃˞"          "zɾ"         
    ## [1997] "cɲ"          "p͈ː"          "ɾ\u0353"     "↓˦↓˦"       
    ## [2001] "d̤ɮ̤"          "ae̞ˤ"         "ɾ̰"           "ɹˤ"         
    ## [2005] "ŋ̤ɡǃ"         "iˤː"         "ɾ̠"           "ʟ̥\u0353"    
    ## [2009] "ɾ̃"           "ɹ̩"           "e̞̠"           "ɾː"         
    ## [2013] "ɹ̝"           "o̞e̞"          "ŋǀʰ"         "i̤ṳ"         
    ## [2017] "ɳɖr"         "ŋ̤ǀ\u0353"    "\u1d05̪"      "ˀd̠ʒ"        
    ## [2021] "ɔ̘ː"          "ʃ͈"           "ʃ͇"           "a̙ˠ"         
    ## [2025] "b͈ː"          "pfʼ"         "t̠͈ʃː"         "a̙ː"         
    ## [2029] "e̘"           "z̩"           "z̥"           "ŋ̥ǃ̠ʰ"        
    ## [2033] "z̺"           "˦˩"          "˦ː"          "uẽ"         
    ## [2037] "z̃"           "kǂ\u0353ˡx"  "ʃ̺"           "uãː"        
    ## [2041] "ẽ̞ũ"          "ãʲ"          "ɯ̤a"          "ʊ̆"          
    ## [2045] "ʒ͈ː"          "ãẽ̞ˤ"         "ʊ̯"           "ɔo̯"         
    ## [2049] "ndː"         "n̪t̪s̪ʰ"        "ŋɾ"          "e̞a"         
    ## [2053] "kǃ̠ˀ"         "qm"          "qn"          "n̤tʰ"        
    ## [2057] "ŋʱ"          "ŋʘ"          "ɓ̥ː"          "ŋˤ"         
    ## [2061] "ŋkʼ"         "a̤ɯ̤"          "ə̟"           "nʲʷ"        
    ## [2065] "aʲː"         "ŋˀ"          "ə̥"           "ɤ˞"         
    ## [2069] "ŋˑ"          "ɖr̠"          "ɤ̯"           "kʟ̥ʼ"        
    ## [2073] "əˑ"          "ɡ‼ʱ"         "ʈr̥̠"          "ŋ̞"          
    ## [2077] "ɱ̥f"          "ʊɔ"          "n̠t"          "kxʼː"       
    ## [2081] "t̺ː"          "lʼː"         "ʔʲː"         "n̠t̠"         
    ## [2085] "i̤a"          "d̪̤̥"           "hʼ"          "a̟i"         
    ## [2089] "ã̆"           "əɯ"          "d̤ʒ̤"          "ŋhʲ"        
    ## [2093] "ŋhʷ"         "iou"         "ʂ͉"           "qǁ"         
    ## [2097] "qǀ"          "qǂ"          "n̤ʱ"          "ŋ‼"         
    ## [2101] "n̤ʷ"          "hˀ"          "r̪ˤ"          "r̪ˠ"         
    ## [2105] "n̠̤"           "tlʱ"         "r̪ː"          "ɟɦ"         
    ## [2109] "ŋb"          "ɤi"          "e̞ə"          "ŋh"         
    ## [2113] "ɲʄ"          "ɲʼ"          "ʰɟ"          "nsɾ"        
    ## [2117] "qʘ"          "ɢʷˤ"         "ŋǃʱ"         "ŋǃʼ"        
    ## [2121] "qχˤ"         "qʷˤʼ"        "io̞"          "ã̟ː"         
    ## [2125] "qʷˤʰ"        "qχː"         "ˀt̪"          "d̪̤ː"         
    ## [2129] "v̩"           "ãi"          "qˀ"          "ʈ͉"          
    ## [2133] "ndl"         "ɱ̤f"          "ɬʟ̥\u0353"    "e̞̰"          
    ## [2137] "a̟˞"          "t̠͈ʃ"          "e̞̥"           "ɡǀxʼ"       
    ## [2141] "ɒ̃ː"          "ɡǁxʼ"        "q͈"           "ʈˀ"         
    ## [2145] "ioː"         "r\u1da3"     "ʊe"          "ɬ̪ʷ"         
    ## [2149] "d̪z̪ʲ"         "ʝ̟"           "vɾ"          "əy"         
    ## [2153] "r̪̰"           "o̞iˤ"         "r̪̥"           "ɡ‼x"        
    ## [2157] "a̠ː"          "ʈɽ"          "ḭː"          "ḭˑ"         
    ## [2161] "cçʲ"         "j̤"

``` r
features$segment <- orderIPA(features$segment)
glimpse(features)
```

    ## Observations: 2,162
    ## Variables: 38
    ## $ segment                <chr> "m", "k", "i", "a", "j", "p", "u", "w", "…
    ## $ tone                   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ stress                 <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ syllabic               <chr> "-", "-", "+", "+", "-", "-", "+", "-", "…
    ## $ short                  <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ long                   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ consonantal            <chr> "+", "+", "-", "-", "-", "+", "-", "-", "…
    ## $ sonorant               <chr> "+", "-", "+", "+", "+", "-", "+", "+", "…
    ## $ continuant             <chr> "-", "-", "+", "+", "+", "-", "+", "+", "…
    ## $ delayedRelease         <chr> "0", "-", "0", "0", "0", "-", "0", "0", "…
    ## $ approximant            <chr> "-", "-", "+", "+", "+", "-", "+", "+", "…
    ## $ tap                    <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ trill                  <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ nasal                  <chr> "+", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ lateral                <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ labial                 <chr> "+", "-", "-", "-", "-", "+", "+", "+", "…
    ## $ round                  <chr> "-", "0", "0", "0", "0", "-", "+", "+", "…
    ## $ labiodental            <chr> "-", "0", "0", "0", "0", "-", "-", "-", "…
    ## $ coronal                <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ anterior               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ distributed            <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ strident               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ dorsal                 <chr> "-", "+", "+", "+", "+", "-", "+", "+", "…
    ## $ high                   <chr> "0", "+", "+", "-", "+", "0", "+", "+", "…
    ## $ low                    <chr> "0", "-", "-", "+", "-", "0", "-", "-", "…
    ## $ front                  <chr> "0", "-", "+", "-", "+", "0", "-", "-", "…
    ## $ back                   <chr> "0", "-", "-", "-", "-", "0", "+", "+", "…
    ## $ tense                  <chr> "0", "0", "+", "0", "+", "0", "+", "+", "…
    ## $ retractedTongueRoot    <chr> "0", "0", "-", "-", "0", "0", "-", "0", "…
    ## $ advancedTongueRoot     <chr> "0", "0", "-", "-", "0", "0", "-", "0", "…
    ## $ periodicGlottalSource  <chr> "+", "-", "+", "+", "+", "-", "+", "+", "…
    ## $ epilaryngealSource     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ spreadGlottis          <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ constrictedGlottis     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ fortis                 <chr> "-", "-", "0", "0", "-", "-", "0", "-", "…
    ## $ raisedLarynxEjective   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ loweredLarynxImplosive <chr> "-", "-", "-", "-", "-", "-", "-", "-", "…
    ## $ click                  <chr> "-", "-", "0", "0", "-", "-", "0", "-", "…

``` r
# Check for EA segments already defined in phoible segment-feature matrix ##
df$in.phoible <- df$Phoneme %in% features$segment
head(df)
```

    ##   Segment Phoneme Notes in.phoible
    ## 1     (ã)     <ã>  <NA>      FALSE
    ## 2    (bʲ)    <bʲ>  <NA>      FALSE
    ## 3     (ɕ)     <ɕ>  <NA>      FALSE
    ## 4    (dʲ)    <dʲ>  <NA>      FALSE
    ## 5    (dz̪)    <dz̪>  <NA>      FALSE
    ## 6    (e̞ː)    <e̞ː>  <NA>      FALSE

``` r
# which trues
length(which(df$in.phoible))
```

    ## [1] 715

``` r
# which falses
length(which(!df$in.phoible))
```

    ## [1] 769

``` r
# Populate the OK phoible segments
# df$Phoneme <- ifelse(df$in.phoible==TRUE, df$Segment, NA)
# head(df)
```

``` r
# Write the output to a table
rownames(df) <- NULL
write.table(df, "../data/formatted/EA_IPA_correspondences.tsv", quote=F, row.names=F, sep="\t")
```
