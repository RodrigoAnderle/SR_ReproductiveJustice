
# DATA MANAGEMENT


# Packages ----------------------------------------------------------------
require(bibliometrix)
require(tidyverse)

# files -------------------------------------------------------------------

files = dir("raw")

## Bibtex
files[str_detect(files, ".bib")]
WOS1 <- convert2df(paste0("raw/",files[str_detect(files, ".bib")][1]), 
                  dbsource = "wos", format = "bibtex")
WOS2 <- convert2df(paste0("raw/",files[str_detect(files, ".bib")][2]), 
                   dbsource = "wos", format = "bibtex")
SCO <- convert2df(paste0("raw/",files[str_detect(files, ".bib")][3]), 
                   dbsource = "scopus", format = "bibtex")

### Merging
WOS1 %>% 
  full_join(WOS2) %>% 
  full_join(SCO)  -> M

nrow(WOS1) + nrow(WOS2) + nrow(SCO)
nrow(M)
rm(WOS1,WOS2,SCO)

## csv
PUB <- read.csv(paste0("raw/",files[str_detect(files, "pubmed")]),
                 header = T)
BVS <- read.csv(paste0("raw/",files[str_detect(files, "BVS")]),
                header = T)
SCI <- read.csv(paste0("raw/",files[str_detect(files, "scielo")]),
                header = T)
nrow(PUB)
nrow(BVS)
nrow(SCI)
### Ajusting names
#### PUBMED
names(PUB) <- 
  c( "PM", "TI", "AU", "CR", "AF", "JI", "PY", "PD","PMCID", "NIHMS.ID","DI" )
PUB$DB <- "PUBMED"
PUB$PY <- as.numeric(PUB$PY)
#### BVS
names(BVS) <- 
  c("BVS_ID", "TI", "AU", "SO", "JI", "DB", "DT", "LA", "PY","ID", "DE", 
    "Country", "URL", "AB", "PD","VL", "IS", "DI", "SN", "Accession.number","PMCID")
BVS$VL <- as.character(BVS$VL)
BVS$PY <- as.numeric(BVS$PY)

#### scielo
names(SCI) <- c("scielo_ID", "TI","AU", "SO", "JI","LA", "PY",
                "URL" )
SCI$DB <- "SCIELO"
SCI$PY <- as.numeric(SCI$PY)

## Merging

nrow(M)
M %>% 
  full_join(PUB) %>% 
  full_join(BVS) %>% 
  full_join(SCI) -> M
nrow(PUB) + nrow(BVS) + nrow(SCI)
nrow(M)

## Adjusting SR information
#M$SR[is.na(M$SR)] <- 
#  paste0(gsub(",.*$", "", gsub(",.*$", "", toupper(M$AU[is.na(M$SR)]))),
#       ", ",M$PY[is.na(M$SR)])
#nrow(M)

# Database ----------------------------------------------------------------

saveRDS(M, "data/source.rds")
