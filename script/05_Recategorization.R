
## 05_Recategorization

# Aim: Update the categories and adding the axes (ejes), and removing 3
# duplicates.
# Input: Previous data.frame file with 1247 documents and more complete
# information.
# Output: An update data.frame file with 1244 documents with adjusted categories
# and axes added.


# packages ----------------------------------------------------------------

require(openxlsx)
require(tidyverse)

# Update categories -------------------------------------------------------
## main file
data <- readRDS("data/data_results.rds")

## new categories
catupdate <- read.xlsx("raw/tabla 1247 obras RECATEGORIZADAS.xlsx")
catupdate <- catupdate %>% 
  select(Referencia, Autor, Título, EXCLUIR.REPETIDO, Categorías, "EJES.(EIXOS)")

## Merging
data %>% 
  left_join(catupdate, by = c("SR" = "Referencia")) -> joined_df


## Inconsistencies
###Categorias
joined_df$Categorías %>% table 
### Categories Adjusts
ids_posparto <- which(joined_df$Categorías == "CICLOEPP - PÓSTPARTO")
joined_df$Categorías[ids_posparto] <- "POSPARTO"
ids_coerrepro <- which(joined_df$Categorías == "COREPROD")
joined_df$Categorías[ids_coerrepro] <- "COERREPRO"
ids_cicloep <- which(joined_df$Categorías == "CICLOEPP")
joined_df$Categorías[ids_cicloep] <- "CICLOEP"
ids_ppublicdes <- which(joined_df$Categorías == "PPUBLICDESENVOL")
joined_df$Categorías[ids_ppublicdes] <- "PPUBLICADES"

## EJES
joined_df$`EJES.(EIXOS)` %>% table ## "1 - NO TENER" != "1 -NO TENER"
joined_df$`EJES.(EIXOS)`[which(joined_df$`EJES.(EIXOS)` == "1 -NO TENER")] <- "1 - NO TENER"
joined_df$`EJES.(EIXOS)` %>% table
### Rename EJES
joined_df <- rename(joined_df, Ejes = `EJES.(EIXOS)`)
### Rename TEORIA & PRÁTICA
ids_teoriapratica <- which(joined_df$Ejes == "TEORÍA Y PRÁCTICA")
joined_df$Ejes[ids_teoriapratica] <- "5 - TEORÍA Y PRÁCTICA"
table(joined_df$Ejes)

##EXCLUIR
nrow(joined_df)
joined_df$EXCLUIR.REPETIDO %>% table
ids_to_exclude <- which(joined_df$EXCLUIR.REPETIDO == "EXCLUIR")
joined_df <- joined_df[-ids_to_exclude,]
nrow(joined_df)
joined_df$EXCLUIR.REPETIDO %>% table


# Saving ------------------------------------------------------------------
saveRDS(joined_df,"data/data_ejes_results.rds")
nrow(joined_df)
rm(list = ls())
gc()