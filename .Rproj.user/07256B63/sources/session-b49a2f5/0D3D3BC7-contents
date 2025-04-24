


## Dimensão Geográfica
##packages
require(tidyverse)
require(bibliometrix)
require(kableExtra)
require(RColorBrewer) 
require(patchwork)
require(openxlsx)
require(reshape2)
require(maps)
require(ggrepel)
require(rnaturalearthdata)
require(rnaturalearth)
require(sf)
require(cowplot)
require(treemapify)
require(ggwordcloud)
require(wordcloud2)
require(wordcloud)

data <- readRDS("data/data_results.rds")

# Plots Tempo -------------------------------------------------------------


data %>% 
  group_by(AU1_CO) %>% 
  summarise(qtd = n()) -> dados
    
           
         
world <- ne_countries(scale = "medium", returnclass = "sf")
world$name <- world$name_es
world <- world %>%
  mutate(name = case_when(
    name == "Estados Unidos" ~ "EE. UU.",
    TRUE ~ name  # Mantém os nomes que não correspondem a "Estados Unidos"
  ))
world_dados <- merge(world, dados, by.x = "name", by.y = "AU1_CO", all.x = TRUE)
world_dados <- world_dados %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(longitude = st_coordinates(centroid)[, 1], # Extrai a longitude
         latitude = st_coordinates(centroid)[, 2])  # Extrai a latitude
world_dados <- world_dados %>%
  mutate(
    longitude = ifelse(name == "Francia", 2.2137, longitude), # Longitude correta da França
    latitude = ifelse(name == "Francia", 46.2276, latitude)   # Latitude correta da França
  )



ggplot(data = world) +
  geom_sf(alpha = 0.1, color = "grey") + # Países mais translúcidos
  geom_point(data = world_dados, 
             aes(x = longitude, y = latitude, size = log(qtd)), 
             color = "purple", alpha = 0.7) +
  geom_text(data = world_dados, 
            aes(x = longitude, y = latitude, label = qtd), 
            size = 3, vjust = -1) + # Adiciona os números das publicações
  geom_rect(aes(xmin = -10,, xmax = 30, ymin = 35, ymax = 70), 
            color = "black", fill = NA, size = 0.1, alpha = 0.7) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), # Remove os títulos dos eixos
        axis.text = element_blank(),  # Remove os valores nos eixos
        axis.ticks = element_blank(), # Remove os ticks dos eixos
        panel.grid = element_blank()) -> mapa_mundi
mapa_mundi

# Filtra apenas os países da Europa no seu dataset
europa_dados <- world_dados %>% filter(subregion %in% 
                                         c("Western Europe","Southern Europe",
                                           "Northern Europe"))
# Cria o gráfico com o zoom
ggplot(data = europa_dados) +
  geom_sf(alpha = 0.1, color = "grey") +
  geom_point(aes(x = longitude, y = latitude, size = log(qtd)), 
             color = "purple", alpha = 0.7) +
  geom_text(aes(x = longitude, y = latitude, label = qtd), 
            size = 3, vjust = -1) + # Adiciona os nomes
  coord_sf(xlim = c(-10, 40), ylim = c(35, 70)) + # Limite da área exibida (foco na Europa)
  theme_minimal() +
  #labs(title = "Produção de Artigos na Europa", size = "Artigos") +
  theme(legend.position = "none",
    axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) -> mapa_europa
mapa_europa

## adicionando bordas
mapa_mundi <- mapa_mundi +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) # Borda em volta do mapa_mundi

mapa_europa <- mapa_europa +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1))


figura_completa <- (mapa_mundi + mapa_europa) +
  plot_layout(ncol = 2)


jpeg("data/Publicações_mapa.jpeg", res = 300, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(figura_completa)
dev.off()


# por continents ----------------------------------------------------------
world_dados %>%
  filter(!is.na(qtd)) %>%
  select(continent, name_es, qtd) %>% 
  mutate(name_es = case_when(
    name_es == "Estados Unidos" ~ "EE. UU.",
    TRUE ~ name_es),
    continent = case_when(
      continent == "Africa" ~ "África",
      continent == "Antarctica" ~ "Antártida",
      continent == "Asia" ~ "Asia",
      continent == "Europe" ~ "Europa",
      continent == "North America" ~ "América del Norte",
      continent == "Oceania" ~ "Oceanía",
      continent == "Seven seas (open ocean)" ~ "Siete mares (océano abierto)",
      continent == "South America" ~ "América del Sur",
      TRUE ~ continent
    )) %>% 
  ggplot(aes(area = qtd, fill = continent, label = name_es, subgroup = continent)) +
  geom_treemap() +                          # Criar os retângulos
  geom_treemap_subgroup_border() +          # Bordas para organizar por continente
  geom_treemap_text(colour = "white",       # Texto nos retângulos (países)
                    place = "centre",
                    grow = TRUE) +
  theme_minimal() -> Continentes
jpeg("data/Publicações_porcontinentes.jpeg", res = 300, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(Continentes)
dev.off()

# Nuvem de palavras
nuvem_dados <- world_dados %>%
  filter(!is.na(qtd)) %>%
  select(name, qtd, continent) %>%
  rename(País = name, Quantidade = qtd, Continente = continent) 

# Definir cores baseadas nos continentes
cores_continentes <- c(
  "Africa" = "#FF7F0E",
  "Asia" = "#1F77B4",
  "Europe" = "#2CA02C",
  "North America" = "#D62728",
  "Oceania" = "#9467BD",
  "South America" = "#FFD700"
)
# Criar o vetor de cores para cada país com base em seus continentes
cores <- sapply(nuvem_dados$Continente, function(cont) cores_continentes[cont])

# Gerar a nuvem de palavras com cores corretas
jpeg("data/nuvem_palavras.jpg", width = 1200, height = 800, res = 300)
wordcloud(words = nuvem_dados$País, freq = nuvem_dados$Quantidade,
          min.freq = 1, random.order = FALSE, 
          colors = cores, ordered.colors = T)

dev.off()


# Categorias na geografia -------------------------------------------------
data %>% 
  group_by(CT, AU1_CO) %>% 
  summarise(qtd = n()) %>% 
ggplot(aes(label = AU1_CO, size = qtd, color = CT)) +
  geom_text_wordcloud() +                  # Gera as palavras como nuvem
  scale_size_area(max_size = 10) +         # Ajusta o tamanho máximo das palavras
  facet_wrap(~CT, scales = "free") +       # Uma nuvem para cada categoria
  theme_minimal() + 
  labs(
    #title = "Nuvens de Palavras por Categoria",
    x = NULL,
    y = NULL,
    color = "Categoria (CT)"
  ) -> cat_geo
jpeg("data/nuvem_palavras_países_cat.jpg", 
     width = 3000, height = 2000, res = 300)
print(cat_geo)
dev.off()

### sem EUA
data %>% 
  filter(AU1_CO != "EE. UU.") %>% 
  group_by(CT, AU1_CO) %>% 
  summarise(qtd = n()) %>% 
  ggplot(aes(label = AU1_CO, size = qtd, color = CT)) +
  geom_text_wordcloud() +                  # Gera as palavras como nuvem
  scale_size_area(max_size = 8) +         # Ajusta o tamanho máximo das palavras
  facet_wrap(~CT, scales = "free") +       # Uma nuvem para cada categoria
  theme_minimal() + 
  labs(
    #title = "Nuvens de Palavras por Categoria",
    x = NULL,
    y = NULL,
    color = "Categoria (CT)"
  ) -> cat_geo_seua

jpeg("data/nuvem_palavras_países_cat_s_EUA.jpg", 
     width = 5000, height = 4000, res = 400)
print(cat_geo_seua)
dev.off()


