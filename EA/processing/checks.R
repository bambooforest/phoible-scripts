# Run some data checks on data from: http://eurasianphonology.info/
# Create EA-IPA correspondences
# Steven Moran <steven.moran@uzh.ch>

library(dplyr)

# Check that the number of inventories in the EA csv dump (from JSON file) is same as in PHOIBLE dev

phoible <- read.csv(url("https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true"))
ea.raw <- read.csv(url("https://github.com/bambooforest/phoible-scripts/raw/master/EA/data/raw/phono_dbase.csv"))

ea.raw.counts <- ea.raw %>% group_by(LanguageCode, LanguageName) %>% summarize(count=n())
phoible.ea.counts <- phoible %>% filter(Source=="ea") %>% group_by(InventoryID, LanguageName) %>% summarize(count=n())

d <- left_join(ea.raw.counts, phoible.ea.counts, by=c("LanguageName"="LanguageName"))
d$diff <- d$count.x == d$count.y
d %>% filter(!diff)


##########

# check phoible's contents against list of iso codes
phoible <- url("https://github.com/phoible/dev/raw/master/data/phoible-by-phoneme.RData")
load(phoible)
data <- final.data
glimpse(data)

phoible.codes <- distinct(select(data, LanguageCode))
dim(phoible.codes)

# EA database
ea <- read.csv(url("https://raw.githubusercontent.com/uzling/typdb/master/sources/eurasian-phonologies/phono_dbase.csv?token=ABBhroSF8kJYNg-qo4gTRRL9NMF3lUp9ks5Wn7IxwA%3D%3D"))
# ea.codes <- distinct(select(ea, LanguageCode))
ea.codes <- distinct(select(ea, LanguageCode, Phoneme))
dim(ea.codes)
glimpse(ea.codes)
rownames(ea.codes) <- NULL
ea.codes

distinct(select(ea.codes, Phoneme)) # 1486 total segments

# EA inventories not in phoible (157 -- 2 without codes)
ea.codes$missing <- ea.codes$LanguageCode %in% phoible.codes$LanguageCode
rownames(ea.codes) <- NULL
head(ea.codes)
ea.codes %>% filter(missing==F)

ea.not.in.phoible <- ea.codes %>% filter(missing==F)
ea.not.in.phoible.segments <- distinct(select(ea.not.in.phoible, Phoneme)) #1178
dim(ea.not.in.phoible.segments)
head(ea.not.in.phoible.segments)

# get inventories not in phoible and EA segments not in phoible features

# features in phoible
setwd("~/Dropbox/Github/phoible/raw-data/FEATURES")
features <- read.csv("phoible-segments-features.tsv", sep="\t")
glimpse(features)

ea.not.in.phoible.segments$missing <- ea.not.in.phoible.segments$Phoneme %in% features$segment
table(ea.not.in.phoible.segments$missing) #640 segments missing from phoible



# if needed, uniquify
unique.segments <- unique(segments)
rownames(unique.segments) <- NULL
dim(unique.segments)

# unique.segments$segment %in% features$segment
unique.segments$missing <- unique.segments$segment %in% features$segment
table(unique.segments$missing)

unique.segments %>% filter(missing==F)
x <- unique.segments %>% filter(missing==F)
write.csv(x, "ea_missing_segments.csv")



glimpse(data)



# phoible fo NAs
phoible <- url("https://github.com/phoible/dev/raw/master/data/phoible-by-phoneme.RData")
load(phoible)
data <- final.data
glimpse(data)

# check for NAs
sum(is.na(data$LanguageCode))




mappings <- url("https://raw.githubusercontent.com/bambooforest/phoible/jipa/mappings/InventoryID-ISO-gcode-Bibkey-Source.tsv")
mappings <- read.csv(mappings, sep="\t")
head(mappings)

x <- select(mappings, LanguageCode, Glottocode)
y <- arrange(x, LanguageCode)
z <- distinct(y) %>% group_by(LanguageCode, Glottocode)
z %>% group_by(Glottocode) %>% filter(n()>1)



write.csv(z, "codes.gcodes.csv")

iso.codes <- unique(mappings$LanguageCode)
head(iso.codes)
gcodes <- unique(mappings$Glottocode)
head(gcodes)
codes$missing <- codes$Id %in% iso639.3$Id
codes

select(mappings, LanguageCode, Glottocode) %>% filter(LanguageCode=="tuo")
x <- distinct(select(mappings, LanguageCode, Glottocode))
head(x)

x %>% group_by(LanguageCode, Glottocode) %>% filter()


select(mappings, LanguageCode, Glottocode)
distinct(select(mappings, LanguageCode))
distinct(select(mappings, Glottocode))

x <- distinct(mappings %>% group_by(LanguageCode, Glottocode) %>% select(LanguageCode, Glottocode))
summary(x)
