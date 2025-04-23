

#NÃO DEVEREMOS UTILIZAR.
# Conceptual strucutre ----------------------------------------------------


# Função para verificar se um número está em uma lista de caracteres
contains_number <- function(column, number) {
  sapply(strsplit(as.character(column), ", "), function(x) number %in% x)
}

## Quantidade de artigos por cluster
nrow(data)
length(data$Clusters[contains_number(data$Clusters, "1")])
length(data$Clusters[contains_number(data$Clusters, "2")])
length(data$Clusters[contains_number(data$Clusters, "3")])
length(data$Clusters[contains_number(data$Clusters, "4")])
length(data$Clusters[contains_number(data$Clusters, "5")])
length(data$Clusters[contains_number(data$Clusters, "None")])


CS1 <- conceptualStructure(data[contains_number(data$Clusters, "1"),], field="TI", 
                           method="CA", minDegree=4, 
                           clust="auto", stemming=FALSE, labelsize=10, documents=2)

CS2 <- conceptualStructure(data[contains_number(data$Clusters, "2"),], field="TI", 
                           method="CA", minDegree=4, 
                           clust="auto", stemming=FALSE, labelsize=10, documents=2)

CS3 <- conceptualStructure(data[contains_number(data$Clusters, "3"),], field="TI", 
                           method="CA", minDegree=4, 
                           clust="auto", stemming=FALSE, labelsize=10, documents=2)

CS4 <- conceptualStructure(data[contains_number(data$Clusters, "4"),], field="TI", 
                           method="CA", minDegree=4, 
                           clust="auto", stemming=FALSE, labelsize=10, documents=2)

CS5 <- conceptualStructure(data[contains_number(data$Clusters, "5"),], field="TI", 
                           method="CA", minDegree=4, 
                           clust="auto", stemming=FALSE, labelsize=10, documents=2)
