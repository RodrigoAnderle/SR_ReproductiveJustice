
# 05_New_recategorization

#Aim: To updates in categories and "Ejes", remove the CULTART category
# Input: Previous recategorization output data.frame file with 1244 documents.
# Output: A final data.frame file with 1171 documents, plus an excel file to
# manual revision.

# packages ----------------------------------------------------------------

require(openxlsx)
require(tidyverse)
require(bibliometrix)
require(igraph)
require(stringr)

# Update categories -------------------------------------------------------
## main file
data <- readRDS("data/data_ejes_results.rds")

## new categories
catupdate <- read.xlsx("raw/1244 obras RE-RE-RECATEGORIZADAS COM EIXOS.xlsx")
catupdate <- catupdate %>% 
  select(Referencia, Autor, Título, Categorías, "EJES.(EIXOS)") %>% 
  rename(Ejes = "EJES.(EIXOS)")

## Merging
data %>% 
  select(!c(Autor, Título, Categorías, LCS, Ejes)) %>% 
  left_join(catupdate, by = c("SR" = "Referencia")) -> joined_df

nrow(joined_df)


# Remove category CULTART -------------------------------------------------
table(joined_df$Categorías) %>% length
joined_df %>% 
  filter(Categorías != "CULTART") -> data
nrow(data)
table(data$Categorías) %>% length

# Redoing co-citation -----------------------------------------------------
data$DB[1] <- "ISI"
histResults <- histNetwork(data)
histResults$histData %>% 
  select(Title, LCS) %>% 
  rename(Título = Title) -> LCSdata
data <- merge(data, LCSdata, by.x = "TI" ,by.y = "Título") 
nrow(data)
## Information without references
table(is.na(data$CR))
## Type of document
table(data$DT)

saveRDS(data, "data/data_Recategorization.rds")
nrow(data)

data %>% 
  rename(Referencia = SR,
         Año = PY,
         País = AU1_CO,
         "Co-Citas" = LCS,
         Citas = TC,
         Tipo = DT) %>% 
  select(Referencia, Autor,	Título,	Categorías,	Ejes,	Año,	
         País,	"Co-Citas",	Citas, Tipo) %>% 
  write.xlsx("data/Lista_Artigos.xlsx")

rm(list = ls())
gc()