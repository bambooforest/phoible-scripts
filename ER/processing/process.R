# Prepare Erich Round's Australian languages inventories for PHOIBLE
# Steven Moran <steven.moran@uzh.ch>

# gara1261 is not a real glottolog code
# many of these gcodes are from dialects, so they don't match language codes
# gara1261 --> gara1269
# djiw1239 --> djiw1241
# kani1267 --> kani1276
# angu1240 --> angu1242
# woro1255 --> woro1258

library(dplyr)
library(ggplot2)
library(data.table)

# Data for release (no ISO codes!)
df <- read.table("../data/raw/Australian_phonemes_for_PHOIBLE_20170501.txt", sep="\t", quote="\"", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
glimpse(df); dim(df)
head(df)
table(df$Glottolog_code, exclude=FALSE)
table(df$Glottolog_nearest, exclude=FALSE)

# Replace Glottolog code with nearest Glottolog code to get the ISO codes
df <- mutate(df, Glottolog_code = ifelse(!is.na(df$Glottolog_nearest), df$Glottolog_nearest, df$Glottolog_code))
head(df)
table(df$Glottolog_nearest, exclude=FALSE)
table(df$Glottolog_code, exclude=FALSE)
dim(df)

# Add inventory IDs by group
DT <- data.table(df)
DT[, InventoryID := .GRP, by = list(Glottolog_code, Variety_name)]
DT$InventoryID <- DT$InventoryID+2628
DT

# Get glottolog mappings (see also the Python script that generates this csv file -- may need to be updated)
gc2iso <- read.csv("../data/raw/gc2iso.csv", header=F)
glimpse(gc2iso)
colnames(gc2iso) <- c("Glottolog_code", "LanguageCode")

# Join in the ISO codes
glimpse(DT)
p <- left_join(DT, gc2iso)

# Create PHOIBLE data format for aggregation script
p.df <- p %>% select(IPA_for_PHOIBLE, Variety_name, InventoryID, LanguageCode)
head(p.df)
p.df.csv <- p.df[,c(3,4,2,1)]
colnames(p.df.csv) <- c("InventoryID", "LanguageCode", "LanguageName", "Phoneme")
dim(p.df.csv) # 9098
glimpse(p.df.csv)

write.table(p.df.csv, file="../data/formatted/ER_inventories.tsv", sep="\t", quote=FALSE, row.names=FALSE)



################ SCRATCH ##################

# Transform the data for phoible
for.p <- df %>% select(Glottolog_code, Source_ref) %>% distinct()


# To add to phoible inventory index
for.csv <- df %>% select(Glottolog_code, Source_ref) %>% distinct()
dim(for.csv); head(for.csv)
id <- as.numeric(rownames(for.csv))
id
for.csv$id <- id+2628
for.csv$LanguageCode <- NA
glimpse(for.csv)
for.csv$Source <- "ER"
for.csv2 <- for.csv[,c(3,4,1,2,5)]

colnames(for.csv2) <- c('InventoryID','LanguageCode','Glottocode','BibtexKey','Source')
head(for.csv2)
write.table(for.csv2, file="metadata-for-phobie.tsv", sep="\t", quote=F, row.names=F)



length(unique(df$Glottolog_code)) # 284 unique g-codes; some language varieties are NONE
length(unique(df$Variety_name)) # 343 Variety_name's

# No labiodental fricatives
filter(df, grepl("p", IPA_for_PHOIBLE)) # test
filter(df, grepl("f", IPA_for_PHOIBLE))
filter(df, grepl("v", IPA_for_PHOIBLE))
filter(df, grepl("ɱ", IPA_for_PHOIBLE))
filter(df, grepl("ⱱ", IPA_for_PHOIBLE))
filter(df, grepl("ʋ", IPA_for_PHOIBLE))

# Phonemes
qplot(table(df$IPA_for_PHOIBLE))

table(df$Glottolog_code)

# get phoible + ER inventories

# Some languages have NONE g-code
df %>% select(Glottolog_code, Variety_name, IPA_for_PHOIBLE) %>% filter(IPA_for_PHOIBLE=="β") %>% unique
df %>% select(Glottolog_code, Variety_name,IPA_for_PHOIBLE) %>% filter(Glottolog_code =="NONE")



# Get language families from Glottolog?
# Add them to ER data
# Add generic function to BibtexIDEtcFile

# Language families to BDPROTO

