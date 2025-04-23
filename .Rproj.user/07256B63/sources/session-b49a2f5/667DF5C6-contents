# Clustering description and co-citation analysis

##packages
require(tidyverse)
require(bibliometrix)
require(igraph)
require(stringr)
require(dplyr)
require(readxl)


data <- readRDS("data/manualcategories_data.rds")


# Co-citação --------------------------------------------------------------
# Função para verificar se um número está em uma lista de caracteres
contains_number <- function(column, number) {
  sapply(strsplit(as.character(column), ", "), function(x) number %in% x)
}


#Abordagem alternativa: identificar principais artigos por meio de co-citações

data$DB[1] <- "ISI"
histResults <- histNetwork(data)
histResults$histData %>% 
  select(DOI, LCS) %>% 
  rename(DI = DOI) %>% 
  merge(data) -> data

saveRDS(data, "data/data_co-citacoes.rds")

data %>% 
  select(CT, LCS, TC, SR, TI, DT) %>% 
  rename(Categoria = CT, "co-Citações" = LCS, Citações = TC,
         Ref = SR, Título = TI, Tipo = DT) -> data_summarized
  
write.csv2(data_summarized, "data/Artigos por categorias e co-citações.csv",
           row.names = F, fileEncoding = "UTF-8")


