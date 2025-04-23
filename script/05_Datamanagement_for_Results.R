


## Additional datamanagement for Results

## Packages
require(openxlsx)
require(tidyverse)
require(bibliometrix)
require(countrycode)
require(stringdist)
require(stringdist)


## upload data
data <- readRDS("data/data_co-citacoes.rds")

# Tempo -------------------------------------------------------------------
table(is.na(data$PY))
data[is.na(data$PY),]
ids.na <- is.na(data$PY)
data$DI[ids.na] <- "10.1177/0886109920954409"
data$PY[ids.na] <- 2021
data$AB[ids.na] <- data$PD[ids.na]
data$TI[ids.na] <- toupper(data$AU[ids.na])
data$AU[ids.na] <- toupper(data$SO[ids.na])
data$DE[ids.na] <- data$Country[ids.na] 
data$SO[ids.na] <- "Affilia: Feminist Inquiry in Social Work"
data$DT[ids.na] <- "Artículo"

#Ajuste adicional de ano
data$TI[data$DI == "10.1080/13691058.2018.1447687"]
data$PY[data$DI == "10.1080/13691058.2018.1447687"] <- 2018
saveRDS(data, "data/data_ajustestempo.rds")

# País --------------------------------------------------------------------
##extraindo da base
data$DB[is.na(data$DB)] <- "ISI"
data = metaTagExtraction(data, Field = "AU1_CO", sep = ";")
table(data$AU1_CO)
table(is.na(data$AU1_CO))
## Coletando manualmente
ids.na <- is.na(data$AU1_CO)
data$TI[ids.na][1]
data$AU1_CO[ids.na][1] <- "USA"
data$TI[ids.na][2] # PARECE SER UM LIVRO EDITADOR POR HARWARD
data$AU1_CO[ids.na][2] <- "USA"
i = 3
data$TI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 4
data$TI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 5
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 6
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 7
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 8
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 9
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 10
data$DI[ids.na][i] #revisar
data$AU1_CO[ids.na][i] <- "INDIA"
i = 11
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "CANADA"
i = 12
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 13
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 14
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 15
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 16
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 17
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 18
data$DI[ids.na][i] #LANCET COM BEM POUCAS MENÇÕES
data$AU1_CO[ids.na][i] <- "USA"
i = 19
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 20
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 21
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 22
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 23
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 24
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 25
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 26
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 27
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "CANADA"
i = 28
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 29
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 30
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 31
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 32
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 33
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 34
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 35
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 36
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 37
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 38
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 39
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 40
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 41
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 42
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 43
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 44
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 45
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 46
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 47
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 48
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 49
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 50
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 51
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 52
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 53
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 54
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 55
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 56
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 57
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "INDIA"
i = 58
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 59
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 60
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 61
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 62
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 63
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "AUSTRALIA"
i = 64
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "NEW ZEALAND"
i = 65
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 66
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 67
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 68
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 69
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 70
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "NEW ZEALAND"
i = 71
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "IRELAND"
i = 72
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 73
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 74
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 75
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 76
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 77
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 78
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 79
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "IRELAND"
i = 80
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "SPAIN"
i = 81
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 82
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 83
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 84
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 85
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 86
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "ARGENTINA"
i = 87
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "CANADA"
i = 88
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 89
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 90
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA" ##NÃO IDENTIFIQUEI O LIVRO NA INTERNET
i = 91
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 92
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 93
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 94
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA" #PARECE COM PROBLEMA
i = 95
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA" #PARECE COM PROBLEMA
i = 96
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 97
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 98
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 99
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "BRAZIL"
i = 100
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 101
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA" #NOT SURE
i = 102
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 103
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 104
data$DI[ids.na][i]
data$AU1_CO[ids.na][i] <- "USA"
i = 105
data$DI[ids.na][i]
data$PA[ids.na][i]
data$AU1_CO[ids.na][i] <- "UNITED KINGDOM"
i = 106
data$DI[ids.na][i]
data$PA[ids.na][i]
data$AU1_CO[ids.na][i] <- "MEXICO"
i = 107
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"
i = 108
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"
i = 109
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"
i = 110
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"
i = 111
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"
i = 112
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "URUGUAY"
i = 113
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "CANADA"
i = 114
ifelse(is.na(data$PA[ids.na][i]),data$TI[ids.na][i],data$PA[ids.na][i])
data$AU1_CO[ids.na][i] <- "USA"

# Traduzir nomes para espanhol
countries_spanish <- countrycode(data$AU1_CO, origin = "country.name", destination = "cldr.short.es")
data$AU1_CO <- countries_spanish

# Áreas do Conhecimento -----------------------------------------------------------

table(is.na(data$SC))

#Identificando artigos sem Áreas temáticas e os agrupoando por fonte (SO) para
#fazer a identificação manualmente através do subgrupo add_SC
data[is.na(data$SC),] %>% 
  group_by(SO) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd)) %>% 
  ungroup() %>% 
  mutate(total = cumsum(qtd)) -> add_SC
add_SC

# categorias para buscar
# Dividir categorias múltiplas em linhas separadas
data_separated <- 
  data %>%
  unique %>%  
  separate_rows(SC, sep = "; ") %>%
  separate_rows(SC, sep = ";") %>%
  group_by(SC) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

#Sistema de verificação manual, verifica a fonte (SO), pesquisa área temática na
#internet e faz a busca de palavra-chave entre as já existentes na base de
#dados. Identifica as que cabem e adiciona manualmente.
i = 1
add_SC[[1]][i]
palavra_chave <- "REPRODUCTIV"
data_separated %>%
  mutate(similaridade = stringdist(SC, toupper(palavra_chave), method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5)
data$SC[data$SO == add_SC[[1]][i]] <- "LITERATURE;CULTURAL STUDIES;HISTORY;ETHICS;SOCIOLOGY"
i = 2
add_SC[[1]][i]
data$SC[data$SO == add_SC[[1]][i]] <- "SOCIOLOGY;RELIGION"
i = 3
add_SC[[1]][i]
data$SC[data$SO == add_SC[[1]][i]] <- "REPRODUCTIVE BIOLOGY"
i = 4
add_SC[[1]][i]
data$SC[data$SO == add_SC[[1]][i]] <- "REPRODUCTIVE BIOLOGY;OBSTETRICS & GYNECOLOGY"
i = 5
add_SC[[1]][i]
palavra_chave <- "general"
  data_separated %>%
  mutate(similaridade = stringdist(SC, palavra_chave, method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5)
data$SC[data$SO == add_SC[[1]][i]] <- NA
i = 6
add_SC[[1]][i]
palavra_chave <- "REPRODUCTIVE"
data_separated %>%
  mutate(similaridade = stringdist(SC, palavra_chave, method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5) # Limite de similaridade ajustável
data$SC[data$SO == add_SC[[1]][i]] <- "ANTHROPOLOGY;REPRODUCTIVE BIOLOGY;OBSTETRICS & GYNECOLOGY;SCIENCE \\& TECHNOLOGY - OTHER TOPICS"
i = 7
add_SC[[1]][i]
palavra_chave <- "HEALTH"
data_separated %>%
  mutate(similaridade = stringdist(SC, palavra_chave, method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5) # Limite de similaridade ajustável
data$SC[data$SO == add_SC[[1]][i]] <- "ETHNIC STUDIES;HEALTH CARE SCIENCES \\& SERVICES"
i = 8
add_SC[[1]][i]
palavra_chave <- "PSYCHO"
data_separated %>%
  mutate(similaridade = stringdist(SC, toupper(palavra_chave), method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5) # Limite de similaridade ajustável
data$SC[data$SO == add_SC[[1]][i]] <- "PSYCHOLOGY"
i = 10 #9 é NA
add_SC[[1]][i]
palavra_chave <- "ethics"
data_separated %>%
  mutate(similaridade = stringdist(SC, toupper(palavra_chave), method = "jaccard")) %>%
  arrange(similaridade) %>%
  filter(similaridade <= 0.5) # Limite de similaridade ajustável
data$SC[data$SO == add_SC[[1]][i]] <- "MEDICAL ETHICS"

data_separated <- data %>%
  select(SC) %>% 
  unique() %>% 
  separate_rows(SC, sep = "; ") %>%
  separate_rows(SC, sep = ";") %>%
  group_by(SC) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))
table(is.na(data$SC))



# Idioma ------------------------------------------------------------------
# Adicionar uma nova coluna à base de dados com o idioma identificado
data$Idioma <- ifelse(grepl("\\bJustice\\b", data$TI, ignore.case = TRUE), "Inglés",
                      ifelse(grepl("\\bJustiça\\b", data$TI, ignore.case = TRUE), "Portugués",
                             ifelse(grepl("\\bJusticia\\b", data$TI, ignore.case = TRUE), "Español", NA)))

# Ver os primeiros resultados
table(data$Idioma)
table(data$Idioma) %>% sum
table(is.na(data$AB[is.na(data$Idioma)]))

ids.na <- is.na(data$Idioma)
data$Idioma[ids.na] <- ifelse(grepl("\\bJustice\\b", data$AB[ids.na], 
                                    ignore.case = TRUE), "Inglés",
                      ifelse(grepl("\\bJustiça\\b", data$AB[ids.na], 
                                   ignore.case = TRUE), "Portugués",
                             ifelse(grepl("\\bJusticia\\b", data$AB[ids.na],
                                          ignore.case = TRUE), "Español", NA)))

table(data$Idioma)
table(data$Idioma) %>% sum
table(is.na(data$Idioma))
ids.na <- is.na(data$Idioma)
data$Idioma[ids.na] <- ifelse(grepl("\\bReproductive\\b", data$TI[ids.na],
                                    ignore.case = TRUE), "Inglés",
                      ifelse(grepl("\\bReprodutiva\\b", data$TI[ids.na], 
                                   ignore.case = TRUE), "Portugués",
                             ifelse(grepl("\\bReproductiva\\b", data$TI[ids.na],
                                          ignore.case = TRUE), "Español", NA)))
table(data$Idioma)
table(data$Idioma) %>% sum
table(is.na(data$Idioma))
ids.na <- is.na(data$Idioma)
data$Idioma[ids.na] <- ifelse(grepl("\\bReproductive\\b", data$AB[ids.na], 
                                    ignore.case = TRUE), "Inglés",
                              ifelse(grepl("\\bReprodutiva\\b", data$AB[ids.na], 
                                           ignore.case = TRUE), "Portugués",
                                     ifelse(grepl("\\bReproductiva\\b", data$AB[ids.na],
                                                  ignore.case = TRUE), "Español", NA)))
ids.na <- is.na(data$Idioma)
data$Idioma[ids.na] <- ifelse(grepl("\\bReproductive\\b", data$DE[ids.na],
                                    ignore.case = TRUE), "Inglés",
                              ifelse(grepl("\\bReprodutiva\\b", data$DE[ids.na], 
                                           ignore.case = TRUE), "Portugués",
                                     ifelse(grepl("\\bReproductiva\\b", 
                                                  data$DE[ids.na],
                                                  ignore.case = TRUE), "Español", NA)))

ids.na <- is.na(data$Idioma)
data$TI[ids.na]
data$Idioma[ids.na][33] <- "Español"
data$Idioma[ids.na][35] <- "Portugués"
data$Idioma[ids.na][37] <- "Portugués"
ids.na <- is.na(data$Idioma)
data$Idioma[ids.na] <- "Inglés"




# Tipo de Documento -------------------------------------------------------
### Revisar tipos de documentos
table(is.na(data$DT))
ids.na <- is.na(data$DT) 
data$DT[ids.na][1] <- "Artículo"
data$DT[ids.na][2] <- "Artículo"
data$DT[ids.na][3] <- "Editorial" #esperando o que Patrícia acha
data$DT[ids.na][4] <- "Artículo"
data$DT[ids.na][5] <- "Libro o capítulo de libro"
data$DT[ids.na][6] <- "Artículo"
data$DT[ids.na][7] <- "Artículo"
data$DT[ids.na][8] <- "Artículo"
data$DT[ids.na][9] <- "Artículo"
data$DT[ids.na][10] <- "Artículo"
data$DT[ids.na][11] <- "Artículo"
data$DT[ids.na][12] <- "Artículo"
data$DT[ids.na][13] <- "Artículo"
data$DT[ids.na][14] <- "Artículo"
data$DT[ids.na][15] <- "Artículo"
data$DT[ids.na][16] <- "Artículo"

data %>% 
  mutate(DT = 
           case_when(
             grepl("editorial", DT, ignore.case = TRUE) ~ "Editorial",
             grepl("book|chapter", DT,ignore.case = TRUE) ~ "Libro o capítulo de libro",
             grepl("review", DT,ignore.case = TRUE) ~ "Reseña",
             grepl("article", DT,ignore.case = TRUE) ~ "Artículo",
             TRUE ~ DT)) -> data

table(data$DT)
sum(table(data$DT))


# Manual Adjustments ------------------------------------------------------
title <- toupper("Reproductive Justice and Migrant Women in Great Britain")
data$AU[data$TI == title] <- "LONERGAN G"
data$SR[data$TI == title] <- "LONERGAN G, 2012, WOMEN"


## Upadating categories --------------------------------------------------

data$CT[data$TI == "LISTENING TO WOMEN: UNDERSTANDING AND CHALLENGING SYSTEMS OF POWER TO ACHIEVE REPRODUCTIVE JUSTICE IN SOUTH CAROLINA"] <- "MATSAREPRO"
data$CT[data$TI == "ABORTION AS GENDER TRANSGRESSION: REPRODUCTIVE JUSTICE, QUEER THEORY, AND ANTI-CRISIS PREGNANCY CENTER ACTIVISM"] <- "POLIT"


## Upadating language ------------------------------------------------------
data %>% filter(
  AU1_CO %in% c(
    "Argentina",
    "Brasil",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Ecuador",
    "España",
    "Filipinas",
    "México",
    "Portugal",
    "Uruguay"
  )
) %>% select(SR, TI, DI) %>% write.xlsx("data/GlobalSur.xlsx")



# Saving dataset ----------------------------------------------------------
saveRDS(data,"data/data_results.rds")

