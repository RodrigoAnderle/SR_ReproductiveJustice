
## Categorizing

##packages
require(tidyverse)
require(bibliometrix)
require(igraph)
require(stringr)
require(dplyr)
require(readxl)




## Upload data
data <- readRDS("data/data.rds")


### analyzing languages
#table(data$LA)
#data$TI[which(data$LA %in% c("Inglês, Português",
#                             "Português", "Português, Inglês"))]
#data$TI[which(data$LA %in% c("Espanhol"))]
#ids_pt <- which(data$LA %in% c("Inglês, Português",
#                               "Português", "Português, Inglês"))
#ids_sp <- which(data$LA %in% c("Espanhol"))

## extracting title terms with 3 words
data <-  termExtraction(data,#data[c(-ids_pt, - ids_sp),], 
                        Field="TI",   
                     ngrams = 2,
                     stemming = FALSE,
                     language = "english",
                     remove.numbers = TRUE,
                     remove.terms = NULL,
                     keep.terms = NULL,
                     synonyms = NULL,
                     verbose = TRUE)



# Generating data ---------------------------------------------------------

### Arquivo de artigos com termos
write.csv2(data[,c("SR","TI","TI_TM")],row.names = F, 
           file = "data/Artigos_Termos.csv")

### Arquivo de termos
# Contando a frequência de cada termo 
term_freq <- unlist(strsplit(data$TI_TM, ";")) %>% 
  table
# Criando um data.frame com termo e frequência 
df <- data.frame(Term = names(term_freq), Frequency = as.integer(term_freq)) 
# Ordenando o data.frame pela frequência em ordem decrescente 
df <- df[order(df$Frequency, decreasing = TRUE), ]
write.csv2(df, "data/termos_2palavras.csv", row.names = F)


## Title Co-ocurrences network
NetMatrix <- biblioNetwork(data, 
                           analysis = "co-occurrences", 
                           network = "titles", sep = ";",
                           remove.terms = c("REPRODUCTIVE JUSTICE", 
                                            "JUSTICIA REPRODUCTIVA",
                                            "JUSTIÇA REPRODUTIVA"),
                           synonyms = c("WOMEN;WOMENS;MUJERES",
                                        "CONTRACEPTIVE;CONTRACEPTION;CONTRACEPTIVES",
                                        "BLACK;RACE;INDIGENOUS;RACIAL;RACISM;ANTI-BLACK",
                                        "INFANT;CHILDREN; CHILD",
                                        "GENDER;GENDERED",
                                        "ADOLESCENT;ADOLESCENTS;YOUTH",
                                        "ABORTION;ANTI-ABORTION",
                                        "INEQUALITIES;INEQUALITY",
                                        "FAMILY;FAMILIES",
                                        "BRAZIL;BRAZILIAN",
                                        "LATINX;LATIN",
                                        "RISK;RISKS",
                                        "DERECHOS;RIGHTS"))

#### ploting
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 70,
                Title = "Title terms Co-occurrences", 
                type = "fruchterman", size=T, edgesize = 5,labelsize=0.5,
                community.repulsion = 0.01)


# First Check -------------------------------------------------------------

## uploading manual check
synon <- read.csv2("raw/análise software R - termos_2palavras.csv", 
                   header = TRUE, 
                   fileEncoding = "Latin1")
names(synon) <- c("termos", "obs", "excluir", "synon", "freq")

##obs Special Caracters
table(synon$obs)
synon %>% 
  filter(obs == "CARACTER ESPECIAL")
ids_spec_character <- which(synon$obs == "CARACTER ESPECIAL")
synon[ids_spec_character, "termos"]
df[ids_spec_character, "Term"]
#No meu computador está correto, deve ser o encoding do pc da Patrícia
#

## getting synonyms
synon %>% 
  filter(synon != "" ) %>% 
  group_by(synon) %>%
  summarise(termos_combined = str_c(termos, collapse = ";")) %>% 
  select(termos_combined) %>% 
  unlist() %>% 
  as.vector -> syn_vect

## getting remove.terms
synon %>% 
  filter(excluir == 1) %>% 
  select(termos) %>% 
  unlist() %>% 
  as.vector() -> rem.terms_vect

## Refazendo gráfico

## Title Co-ocurrences network
NetMatrix <- biblioNetwork(data, 
                           analysis = "co-occurrences", 
                           network = "titles", sep = ";",
                           remove.terms = rem.terms_vect,
                           synonyms = syn_vect)

#### ploting
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 380,
                Title = "Title 2-word terms after first manual check", 
                type = "fruchterman", size=T, edgesize = 5,labelsize = 0.5,
                label.n = 10,
                community.repulsion = 0.1)


# Second Check ------------------------------------------------------------

## uploading manual check
synon2 <- read_excel(
  "raw/ANÁLISE COM CORREÇÃO DE CARACTERES ESPECIAIS - 2 TERMOS .xlsx")
names(synon2) <- c("termos", "obs", "excluir", "synon", "freq")

##obs Special Caracters
table(synon2$obs)
synon2 %>% 
  filter(obs == "CARACTER ESPECIAL")
ids_spec_character <- which(synon2$obs == "CARACTER ESPECIAL")
synon2[ids_spec_character, "termos"]
df[ids_spec_character, "Term"]
#No meu computador está correto, deve ser o encoding do pc da Patrícia
#

## getting synonyms
synon2 %>% 
  filter(!is.na(synon)) %>% 
  group_by(synon) %>%
  summarise(termos_combined = str_c(termos, collapse = ";")) %>% 
  select(termos_combined) %>% 
  unlist() %>% 
  as.vector -> syn_vect2 #ajustar para primeira palavra ser a indicada como synon

## getting remove.terms
synon2 %>% 
  filter(excluir == 1) %>% 
  select(termos) %>% 
  unlist() %>% 
  as.vector() -> rem.terms_vect2

## Refazendo gráfico

## Title Co-ocurrences network
NetMatrix <- biblioNetwork(data, 
                           analysis = "co-occurrences", 
                           network = "titles", sep = ";",
                           remove.terms = rem.terms_vect2,
                           synonyms = syn_vect2)

#### ploting
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 70,
                Title = "Title 2-word terms after first manual check", 
                type = "fruchterman", size=T, edgesize = 5,labelsize = 0.6,
                #label.n = 20, degree = 10, 
                community.repulsion = 0.15)

# Third Check -------------------------------------------------------------

## uploading manual check
synon3 <- read_excel(
  "raw/ANÁLISE COM CORREÇÃO DE CARACTERES ESPECIAIS - 2 TERMOS -3.xlsx")
names(synon3) <- c("termos", "obs", "excluir", "synon", "freq")


## getting synonyms
synon3 %>% 
  filter(!is.na(synon)) %>% 
  group_by(synon) %>%
  summarise(termos_combined = str_c(c(synon, termos), collapse = ";")) %>%
  select(termos_combined) %>% 
  unlist() %>% 
  as.vector -> syn_vect3 #ajustar para primeira palavra ser a indicada como synon

## getting remove.terms
synon3 %>% 
  filter(excluir == 1) %>% 
  select(termos) %>% 
  unlist() %>% 
  as.vector() -> rem.terms_vect3

## Refazendo gráfico

## Title Co-ocurrences network
NetMatrix <- biblioNetwork(data, 
                           analysis = "co-occurrences", 
                           network = "titles", sep = ";",
                           remove.terms = rem.terms_vect3,
                           synonyms = syn_vect3)

#### ploting
set.seed(2)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 100,
                Title = "Title 2-word terms after third manual check", 
                type = "fruchterman", size=T, edgesize = 5,labelsize = 0.7,
                #cluster = 7, label = F,
                community.repulsion = 0.15)


# Clustering --------------------------------------------------------------

# Converter a matriz em objeto igraph 
network <- graph_from_adjacency_matrix(as.matrix(NetMatrix), 
                           mode="undirected", weighted=TRUE)

## Análise de Cluster
#cluster <- cluster_walktrap(network) #resulta em 614 clusters maioria com poucos termos

## Extraindo os clusters
#clusters <- membership(cluster)
# Extrair os clusters com corte no dendrograma para obter 5 clusters 
#clusters <- cut_at(cluster, no = 5)

# Remover isolados da rede
network <- delete_vertices(network, which(degree(network) == 0))

# Calcular a matriz de distâncias novamente
dist_matrix <- distances(network)

# Substituir NA, NaN ou Inf por um valor grande, como o máximo da matriz de distâncias
dist_matrix[is.infinite(dist_matrix)] <- max(dist_matrix[is.finite(dist_matrix)], na.rm = TRUE)
dist_matrix[is.na(dist_matrix)] <- max(dist_matrix[is.finite(dist_matrix)], na.rm = TRUE)
dist_matrix[is.nan(dist_matrix)] <- max(dist_matrix[is.finite(dist_matrix)], na.rm = TRUE)

# Aplicar K-means para obter 5 clusters
set.seed(123)  # Definir uma semente para reprodutibilidade
kmeans_result <- kmeans(dist_matrix, centers = 6)
terms <- rownames(dist_matrix)
cluster_data <- data.frame(Term = terms, Cluster = kmeans_result$cluster)
write.csv2(cluster_data, "data/TermsbyClusters.csv")

# Função para verificar todos os clusters que estão contidos no título
find_clusters <- function(title, clusters) {
  title_clusters <- c()
  for (cluster_id in unique(clusters)) {
    cluster_titles <- names(clusters[clusters == cluster_id])
    if (any(sapply(cluster_titles, function(ct) str_detect(title, fixed(ct, ignore_case = TRUE))))) {
      title_clusters <- unique(c(title_clusters, cluster_id))
    }
  }
  if (length(title_clusters) == 0) {
    return("None")  # Usar um valor distintivo para títulos sem clusters
  }
  return(paste(title_clusters, collapse = ", "))
}

# Aplicar a função para cada título no dataset
data$Clusters <- sapply(data$TI, find_clusters, clusters = kmeans_result$cluster)
table(data$Clusters)

saveRDS(data,"data/data_clustered.rds")
write.csv2(data, "data/data_clustered.csv")
