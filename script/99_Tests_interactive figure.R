# Instale e carregue os pacotes necessários
install.packages("bibliometrix")
install.packages("visNetwork")
install.packages("igraph")
library(bibliometrix)
library(visNetwork)
library(igraph)

# Supondo que você já tenha um objeto 'M' com seus dados bibliométricos
# Exemplo de criação do objeto M para testar o código
M <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")
M <- convert2df(M, dbsource = "isi", format = "bibtex")

# Crie a matriz de rede
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Transforme a matriz de rede em um objeto igraph
g <- graph_from_adjacency_matrix(NetMatrix, mode = "undirected", diag = FALSE)

# Função para converter igraph para visNetwork
igraph2vis <- function(g, curved = FALSE, labelsize = 1, 
                       opacity = 0.7, type = "fruchterman", shape = "circle") {
  vn <- toVisNetworkData(g)
  vn$nodes$label <- V(g)$name
  vn$edges$num <- 1
  # Verifique se 'color' está definido, caso contrário, defina uma cor padrão
  if (is.null(vn$nodes$color)) {
    vn$nodes$color <- "blue"  # Cor padrão (pode ser alterada)
  }
  # Verifique se 'color' está definido para as arestas, caso contrário, defina uma cor padrão
  if (is.null(vn$edges$color)) {
    vn$edges$color <- "gray"  # Cor padrão (pode ser alterada)
  }
  # Verifique se 'font.size' está definido, caso contrário, defina um tamanho padrão
  if (is.null(vn$nodes$font.size)) {
    vn$nodes$font.size <- rep(10, nrow(vn$nodes))  # Tamanho de fonte padrão
  }
  
  vn$nodes$color <- adjustcolor(vn$nodes$color, alpha = min(c(opacity + 0.2, 1)))
  vn$edges$color <- adjustcolor(vn$edges$color, alpha = opacity)
  vn$nodes$font.size <- (vn$nodes$font.size - min(vn$nodes$font.size)) / (max(vn$nodes$font.size) - min(vn$nodes$font.size)) * 10 * labelsize + 10
  VIS <- visNetwork(nodes = vn$nodes, edges = vn$edges) %>%
    visNodes(shape = shape, font = list(color = "black")) %>%
    visIgraphLayout(layout = type) %>%
    visEdges(smooth = curved) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
    visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE)
  return(VIS)
}



# Aplicando a função
VIS <- igraph2vis(g)
VIS

# Instale e carregue os pacotes necessários
install.packages("bibliometrix")
install.packages("visNetwork")
install.packages("igraph")
library(bibliometrix)
library(visNetwork)
library(igraph)

# Supondo que você já tenha um objeto 'M' com seus dados bibliométricos
# Exemplo de criação do objeto M para testar o código
M <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")
M <- convert2df(M, dbsource = "isi", format = "bibtex")

# Crie a matriz de rede
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Transforme a matriz de rede em um objeto igraph
g <- graph_from_adjacency_matrix(NetMatrix, mode = "undirected", diag = FALSE)

# Função para converter igraph para visNetwork
igraph2vis <- function(g, curved = FALSE, labelsize = 1, opacity = 0.7, type = "fruchterman", shape = "circle") {
  vn <- toVisNetworkData(g)
  vn$nodes$label <- V(g)$name
  vn$edges$num <- 1
  
  # Verifique se 'color' está definido para os nós, caso contrário, defina uma cor padrão
  if (is.null(vn$nodes$color)) {
    vn$nodes$color <- "blue"  # Cor padrão (pode ser alterada)
  }
  # Verifique se 'color' está definido para as arestas, caso contrário, defina uma cor padrão
  if (is.null(vn$edges$color)) {
    vn$edges$color <- "gray"  # Cor padrão (pode ser alterada)
  }
  
  # Verifique se 'font.size' está definido, caso contrário, defina um tamanho padrão
  if (is.null(vn$nodes$font.size)) {
    vn$nodes$font.size <- rep(10, nrow(vn$nodes))  # Tamanho de fonte padrão
  }
  
  vn$nodes$color <- adjustcolor(vn$nodes$color, alpha = min(c(opacity + 0.2, 1)))
  vn$edges$color <- adjustcolor(vn$edges$color, alpha = opacity)
  vn$nodes$font.size <- (vn$nodes$font.size - min(vn$nodes$font.size)) / (max(vn$nodes$font.size) - min(vn$nodes$font.size)) * 10 * labelsize + 10
  VIS <- visNetwork(nodes = vn$nodes, edges = vn$edges) %>%
    visNodes(shape = shape, font = list(color = "black")) %>%
    visIgraphLayout(layout = type) %>%
    visEdges(smooth = curved) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
    visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE)
  return(VIS)
}

# Aplicando a função
VIS <- igraph2vis(g)
VIS

# Instale e carregue os pacotes necessários
install.packages("bibliometrix")
install.packages("visNetwork")
install.packages("igraph")
library(bibliometrix)
library(visNetwork)
library(igraph)

# Supondo que você já tenha um objeto 'M' com seus dados bibliométricos
# Exemplo de criação do objeto M para testar o código
M <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")
M <- convert2df(M, dbsource = "isi", format = "bibtex")

# Crie a matriz de rede
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Transforme a matriz de rede em um objeto igraph
g <- graph_from_adjacency_matrix(NetMatrix, mode = "undirected", diag = FALSE)

# Calcule as posições dos nós usando layout_with_fr
layout <- layout_with_fr(g)

# Função para converter igraph para visNetwork
igraph2vis <- function(g, layout, curved = FALSE, labelsize = 1, opacity = 0.7, shape = "circle") {
  vn <- toVisNetworkData(g)
  vn$nodes$label <- V(g)$name
  vn$edges$num <- 1
  
  # Verifique se 'color' está definido para os nós, caso contrário, defina uma cor padrão
  if (is.null(vn$nodes$color)) {
    vn$nodes$color <- "blue"  # Cor padrão (pode ser alterada)
  }
  # Verifique se 'color' está definido para as arestas, caso contrário, defina uma cor padrão
  if (is.null(vn$edges$color)) {
    vn$edges$color <- "gray"  # Cor padrão (pode ser alterada)
  }
  
  # Verifique se 'font.size' está definido, caso contrário, defina um tamanho padrão
  if (is.null(vn$nodes$font.size)) {
    vn$nodes$font.size <- rep(10, nrow(vn$nodes))  # Tamanho de fonte padrão
  }
  
  vn$nodes$color <- adjustcolor(vn$nodes$color, alpha = min(c(opacity + 0.2, 1)))
  vn$edges$color <- adjustcolor(vn$edges$color, alpha = opacity)
  vn$nodes$font.size <- (vn$nodes$font.size - min(vn$nodes$font.size)) / (max(vn$nodes$font.size) - min(vn$nodes$font.size)) * 10 * labelsize + 10
  
  # Adicione as posições dos nós
  vn$nodes$x <- layout[, 1]
  vn$nodes$y <- layout[, 2]
  
  VIS <- visNetwork(nodes = vn$nodes, edges = vn$edges) %>%
    visNodes(shape = shape, font = list(color = "black")) %>%
    visEdges(smooth = curved) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
    visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE)
  return(VIS)
}

# Aplicando a função
VIS <- igraph2vis(g, layout)
VIS

