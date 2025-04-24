


## Categorizing

##packages
require(tidyverse)
require(bibliometrix)
require(igraph)
require(readxl)
require(readr)

## Upload data
data <- readRDS("data/data.rds")
categorized_data <- read_xlsx(
  "raw/análise categorias temáticas 1200 artigos 19-02.xlsx", sheet = 1)
categorized_data <- categorized_data[,1:8]
categorized_data2 <- read_xlsx(
  "raw/ANÁLISE 1084 ARTIGOS EM CATEGORIAS TEMÁTICAS (CT).xlsx", sheet = 1)

## Corrigindo encoding
# Em seguida, converte os dados para o encoding Latin1
categorized_data <- as.data.frame(lapply(categorized_data,
                           function(x) if (is.character(x)) iconv(x, from = "UTF-8", to = "latin1") else x))

# Agora, você pode usar a variável 'df' com os dados convertidos
categorized_data2 <- as.data.frame(lapply(categorized_data2,
                                          function(x) if (is.character(x)) iconv(x, from = "UTF-8", to = "latin1") else x))


# Exclussions -------------------------------------------------------
categorized_data %>% 
  select(DI, SR,LCS,TI, AB, CT, EXCLUIR) -> categorized_data

## Adding exclusions from previous data
names(categorized_data2)
categorized_data2 %>% 
  select(DI, EXCLUIR.) %>% 
  rename(EXCLUIR = EXCLUIR.) %>% 
  filter(!is.na(EXCLUIR)) -> categorized_data2

categorized_data2$DI[categorized_data2$EXCLUIR == "EXCLUIR"]
categorized_data$DI[categorized_data$DI %in% categorized_data2$DI]
categorized_data$EXCLUIR[categorized_data$DI %in% categorized_data2$DI]
categorized_data$EXCLUIR[categorized_data$DI %in% categorized_data2$DI] <- "EXCLUIR"
rm(categorized_data2)

## Excluding
table(categorized_data$EXCLUIR)
## Excludeds
categorized_data %>% 
  filter(EXCLUIR == "EXCLUIR") %>% 
  select(DI, SR) %>% nrow

## Removing merged articles
(a = nrow(data))
data$DI[(data$DI %in% 
           categorized_data$DI[!is.na(categorized_data$EXCLUIR)])] -> selected
length(selected)
data <- data[!(data$DI %in% 
                 categorized_data$DI[categorized_data$EXCLUIR == "EXCLUIR"]),]
(b = nrow(data))
a - b

### Searching the ones didn't match
ids <- !categorized_data$DI[!is.na(categorized_data$EXCLUIR)] %in% selected
length(ids)
(missings <- categorized_data$DI[!is.na(categorized_data$EXCLUIR)][ids])

# Função para encontrar a string mais próxima com base na distância de edição
encontrar_mais_proxima <- function(target, strings) {
  distancias <- adist(target, strings)
  indice_minimo <- which.min(distancias)
  return(indice_minimo)
}
# Encontrar a string mais próxima
found <- vector()
for(i in 1:length(missings)){
found <- c(found, encontrar_mais_proxima(missings[i], data$DI))
}
data$DI[found]
missings

## Excluding missings
(a = nrow(data))
data <- data[-found,]
length(found)
(b = nrow(data))
a - b


# Categorizing ------------------------------------------------------------
nrow(categorized_data)
categorized_data %>% 
  filter(is.na(EXCLUIR)) -> categorized_data_cleaned
nrow(categorized_data_cleaned) - b
print("############################################################# /n
    !! categorized_data tem duas linhas a mais. Devem ser as com erros !! /n
    ############################################################# /n")


## Categorizing
categorized_data_cleaned %>% 
  select(DI, CT) %>% 
  right_join(data, by = "DI") -> data
nrow(data)
b
table(is.na(data$CT))
  
### Missings (16)
data$DI[(data$DI %in% 
           categorized_data_cleaned$DI)] -> selectedCT
length(selectedCT)
idsCT <- !(categorized_data_cleaned$DI %in% selectedCT)
sum(idsCT)

#### Encontrando missings com base no DI
id_prox <- integer()
for(i in 1:sum(idsCT)){
  print(i)
  print(categorized_data_cleaned$TI[which(idsCT)[i]])
  id_prox <- encontrar_mais_proxima(categorized_data_cleaned$TI[idsCT][i], data$TI)
  print(data$TI[id_prox])
  categorized_data_cleaned$CT[which(idsCT)[i]]
  data$CT[id_prox] <- categorized_data_cleaned$CT[which(idsCT)[i]]  
}

## Verificar
#Não identificadoas
print("/n ############################################################# /n",
    " Documentos sem pares identificados para conferência manual: /n",
    categorized_data_cleaned$TI[which(idsCT)[10]], "/n",
    categorized_data_cleaned$TI[which(idsCT)[14]],"/n",
    categorized_data_cleaned$TI[which(idsCT)[18]],"/n",
    "############################################################## /n")
    

i = 10
categorized_data_cleaned[which(idsCT)[i],]
id_prox <- encontrar_mais_proxima(categorized_data_cleaned$AB[idsCT][i], data$AB)
data$AB[id_prox]
data$CT[id_prox] <- categorized_data_cleaned$CT[which(idsCT)[i]]
i = 14
categorized_data_cleaned[which(idsCT)[i],]
id_prox <- encontrar_mais_proxima(categorized_data_cleaned$SR[idsCT][i], data$SR)
data$SR[id_prox]
data$CT[id_prox] <- categorized_data_cleaned$CT[which(idsCT)[i]]
i = 18
categorized_data_cleaned[which(idsCT)[i],]
print("/n ############################################################# /n
    i = 18 tem vários artigos juntos, parece ser problema na leitura do excel./n
    Desconsiderar para categorização, ajuste será feito manualmente. /n
    ############################################################## /n")

table(is.na(data$CT))
b

### Checking Categories
nrow(data)
table(data$CT)
data %>% 
  filter(is.na(CT)) %>% 
  select(SR, TI, AB, CT) 


# Categorias revisadas ----------------------------------------------------

data$CT[data$TI == "LIVING ENVIRONMENTALISMS: COALITION POLITICS, SOCIAL REPRODUCTION, AND ENVIRONMENTAL JUSTICE"] <- 
  "POLIT"
data$CT[data$TI == "REPRODUCTIVE JUSTICE AND THE PACE OF CHANGE: SOCIOECONOMIC TRENDS IN US INFANT DEATH RATES BY LEGAL STATUS OF ABORTION, 1960-1980"] <- 
  "ABOR"  
data$CT[data$TI == "ON FORBIDDEN WOMBS AND TRANSNATIONAL REPRODUCTIVE JUSTICE"] <- 
  "TEORIA"  
data$CT[data$TI == "REPRODUCTIVE JUSTICE: A POLICY WINDOW FOR SOCIAL WORK ADVOCACY"] <- 
  "SAUDE"  
data$CT[data$TI == "ARTICULATING REPRODUCTIVE JUSTICE THROUGH REPARATIVE JUSTICE: CASE STUDIES OF ABORTION IN GREAT BRITAIN AND SOUTH AFRICA"] <- 
  "JURIS"  
data$CT[data$TI == "EXPANDING REPRODUCTIVE JUSTICE THROUGH A SUPPORTABILITY REPARATIVE JUSTICE FRAMEWORK: THE CASE OF ABORTION IN SOUTH AFRICA"] <- 
  "TEORIA"  
data$CT[data$TI == "REPRODUCTIVE JUSTICE, ABORTION RIGHTS AND SOCIAL WORK"] <- 
  "SAUDE"  


# Traduzir categorias -----------------------------------------------------
data %>% 
  group_by(CT) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd))

library(dplyr)

# Tradução das siglas e dos nomes completos
data <- 
  data %>%
  mutate(
    CT_full_spanish = case_when(
      CT == "ABOR" ~ "Aborto",
      CT == "SAREPRO" ~ "Maternidad y salud reproductiva",
      CT == "PLREP" ~ "Planificación reproductiva",
      CT == "POLIT" ~ "Aspectos políticos y activismos",
      CT == "SAUDE" ~ "Salud y Formación Profesional",
      CT == "DESIG" ~ "Desigualdades y violencia",
      CT == "GPOPS" ~ "Grupos de población",
      CT == "TEORIA" ~ "Discusiones teóricas, filosóficas y bioéticas",
      CT == "JUV" ~ "Jóvenes, Juventud y Educación Sexual",
      CT == "CULTUR" ~ "Cultura y Arte",
      CT == "TECREP" ~ "Tecnologías reproductivas",
      CT == "JURIS" ~ "Discusiones legales, jurídicas",
      CT == "EPIDEM" ~ "Epidemias y pandemias",
      CT == "AMB" ~ "Cuestiones medioambientales",
      CT == "BARALUG" ~ "Subrogación",
      TRUE ~ CT  # Mantém os termos não traduzidos como estão
    ),
    CT = case_when(
      CT == "ABOR" ~ "ABOR",
      CT == "SAREPRO" ~ "MATSAREPRO",
      CT == "PLREP" ~ "PLAREPRO",
      CT == "POLIT" ~ "POLIT",
      CT == "SAUDE" ~ "SALUDFORM",
      CT == "DESIG" ~ "DESVIOL",
      CT == "GPOPS" ~ "POPS",
      CT == "TEORIA" ~ "TEORIA",
      CT == "JUV" ~ "JUV",
      CT == "CULTUR" ~ "CULTART",
      CT == "TECREP" ~ "TECREPRO",
      CT == "JURIS" ~ "JURIS",
      CT == "EPIDEM" ~ "EPIPAND",
      CT == "AMB" ~ "AMB",
      CT == "BARALUG" ~ "SUBRO",
      TRUE ~ CT  # Mantém os termos não traduzidos como estão
    )
  )


# saving data -------------------------------------------------------------

saveRDS(data, "data/manualcategories_data.rds")


