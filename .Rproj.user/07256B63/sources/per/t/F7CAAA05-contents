
# DEDUPLICATING

source <- readRDS("data/source.rds")


## Deduplicatins estimatives
### Missing NAs and TI
table(is.na(source$TI))
table(is.na(source$SR))
table(is.na(source$DI))


### Removing duplicates first by DOI, than by title
source %>% 
  distinct(tolower(TI), .keep_all = TRUE) -> source2
nrow(source2)
##Nas
table(is.na(source2$TI))
table(is.na(source2$SR))
table(is.na(source2$DI))
nrow(source2[which(is.na(source2$DI) & is.na(source2$SR)),])


### Creatining missing SRs to input
source2$SR[is.na(source2$SR)] <- paste(
  toupper(gsub("^(.*?),.*", "\\1", source2$AU[is.na(source2$SR)])),#1?
      source2$PY[is.na(source2$SR)],
      toupper(gsub('\\.', "", source2$JI[is.na(source2$SR)], perl=TRUE)))
nrow(source2[which(is.na(source2$SR)),])
source3 <- source2 %>% 
  distinct(tolower(SR), .keep_all = TRUE)
nrow(source3)

nrow(source3[which(is.na(source3$DI)),])
source3$DI[which(is.na(source3$DI))] <- source3$SR[which(is.na(source3$DI))]
nrow(source3[which(is.na(source3$DI)),])

### Imputing SRs into missing DOI
## Deduplicating data
source3 %>% 
  distinct(tolower(DI), .keep_all = TRUE) -> data # REVISAR, isso removeu outras bases
nrow(data)

## Saving final data
saveRDS(data, "data/data.rds")
rm(list = ls())
