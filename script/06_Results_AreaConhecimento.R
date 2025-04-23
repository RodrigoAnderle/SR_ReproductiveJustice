


# Áreas do conhecimento ---------------------------------------------------------

## Packages
require(tidyverse)
require(openxlsx)
require(treemapify)


## data
data <- readRDS("data/data_results.rds")


# Exploring ---------------------------------------------------------------

data$SC %>% 
  unique %>% 
  length()

#Identificando jornais sem área temática específicada e gerando excel para
#verificação manual
data %>% 
  filter(is.na(SC)) %>% 
  select(SO) %>% 
  unique %>% 
  #separate_rows(SC, sep = "; ") %>%
  #separate_rows(SC, sep = ";") %>%
  group_by(SO) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd)) %>% 
  # Exportando para Excel
  write.xlsx("data/Fontes sem área temática.xlsx", 
             colNames = TRUE, rowNames = FALSE, decimal.mark = ",")

## Qtd de áreas do conhecimento?
data %>% 
  select(SC) %>% 
  unique %>% 
  separate_rows(SC, sep = "; ") %>%
  separate_rows(SC, sep = ";") %>%
  group_by(SC) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd)) %>% nrow #67


# Rede de áreas por categoria ---------------------------------------------

data %>% 
  select(SC, CT) %>% 
  unique %>% 
  separate_rows(SC, sep = "; ") %>%
  separate_rows(SC, sep = ";") %>%
  group_by(SC,CT) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd))

library(igraph)
require(ggraph)
library(tidyverse)



# Criação do grafo
# Criação do grafo baseado apenas em SC com CT como links
graph_data <- data %>% 
  select(SC, CT) %>% 
  unique %>% 
  separate_rows(SC, sep = "; ") %>%
  separate_rows(SC, sep = ";") %>%
  group_by(SC, CT) %>% 
  summarise(qtd = n(), .groups = "drop") %>% 
  graph_from_data_frame(directed = FALSE)

# Visualização com layout mais espaçado
ggraph(graph_data, layout = "fr") +  # Layout com força
  geom_edge_link(aes(width = E(graph_data)$qtd), alpha = 0.8, color = "gray") +  # Largura da aresta com base na frequência
  geom_node_point(size = 5, color = "skyblue") +  # Nós representando áreas de conhecimento
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +  # Rótulos das áreas
  theme_void() +
  labs(title = "Rede de Áreas de Conhecimento por Categoria")



# Adicionando atributos aos nós (SC e CT)
V(graph_data)$size <- degree(graph_data)
V(graph_data)$color <- ifelse(V(graph_data)$name %in% data$SC, "skyblue", "orange")

# Visualização do grafo
plot(graph_data,
     vertex.label = V(graph_data)$name,
     vertex.size = sqrt(V(graph_data)$size) * 5, # Ajuste do tamanho dos nós
     edge.width = E(graph_data)$qtd / 2,        # Largura das arestas proporcional à frequência
     vertex.color = V(graph_data)$color,
     main = "Rede de Áreas Temáticas por Categoria")

# Visualização com ggraph
ggraph(graph_data, layout = "fr") +  # Layout com força
  geom_edge_link(aes(width = qtd), color = "gray") +
  geom_node_point(aes(size = sqrt(size)), color = V(graph_data)$color) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() 

library(tidyverse)

library(tidyverse)

# Transformação dos dados para criar conexões entre áreas de conhecimento
edges <- data %>%
  select(SC, CT) %>%
  separate_rows(SC, sep = ";\\s*") %>%  # Dividir múltiplas áreas por categoria
  group_by(CT) %>%
  summarise(pairs = list(combn(unique(SC), 2, simplify = FALSE)), .groups = "drop") %>% 
  unnest(pairs) %>%  # Expandir os pares em várias linhas
  mutate(SC_1 = map_chr(pairs, 1),  # Extrair o primeiro elemento do par
         SC_2 = map_chr(pairs, 2)) %>%
  select(SC_1, SC_2) %>%
  group_by(SC_1, SC_2) %>%
  summarise(qtd = n(), .groups = "drop") %>% 
  filter(!is.na(SC_1) & !is.na(SC_2)) %>% 
  arrange(desc(qtd))

# Verificando a saída
print(edges)

# Criando o grafo com SC conectados
graph_data <- graph_from_data_frame(d = edges, directed = FALSE)
V(graph_data)$strength <- degree(graph_data)
clusters <- cluster_louvain(graph_data)
# Adicionar cores aos nós com base nos clusters
V(graph_data)$cluster <- membership(clusters)  # Atribuir cluster a cada nó
gradient_palette <- colorRampPalette(c("purple", "blue", "red"))
V(graph_data)$color <- gradient_palette(length(unique(V(graph_data)$cluster)))[V(graph_data)$cluster]


# Visualização da rede com conexões diretas entre áreas
ggraph(graph_data, layout = "kk") +  
  geom_edge_link(color = "gray", alpha = 0.05) +
  geom_node_point(aes(color = as.factor(cluster), size = strength)) +  # Cor por cluster
  geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
  scale_color_manual(values = V(graph_data)$color, guide = "none") +
  scale_size(guide = "none") + 
  theme_void() -> a

jpeg("data/Rede de areas do conhecimento.jpg", 
     width = 5000, height = 4000, res = 400)
print(a)
dev.off()




