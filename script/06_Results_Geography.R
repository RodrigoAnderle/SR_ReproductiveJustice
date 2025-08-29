
## Geography

# Aim: To generate a world map with the number of publications by country
# Input: Previous recategorizantion data.frame object with 1171 documents.
# Output: A JPG image with the world map plot.



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

data <- readRDS("data/data_Recategorization.rds")


# Plots Tempo -------------------------------------------------------------


data %>%
  mutate(AU1_CO = ifelse(AU1_CO == "RU", "Reino Unido", AU1_CO)) %>%
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
    latitude = ifelse(name == "Francia", 46.2276, latitude),   # Latitude correta da França
    Global = ifelse(name %in% c(
      "Argentina",
      "Brasil",
      "Chile",
      "China",
      "Colombia",
      "Costa Rica",
      "Ecuador",
      "Filipinas",
      "India",
      "México",
      "Sudáfrica",
      "Tailandia",
      "Turquía",
      "Uruguay",
      "Kenia",
      "Líbano"), "Sur Global", "Norte Global")
  )


ggplot(data = world) +
  geom_sf(alpha = 0.1, color = "grey") + # Países mais translúcidos
  geom_point(data = world_dados, 
             aes(x = longitude, y = latitude, size = log(qtd), color = Global), 
             alpha = 0.7) +
  geom_text(data = world_dados, 
            aes(x = longitude, y = latitude, label = qtd), 
            size = 3, vjust = -1) + # Adiciona os números das publicações
  geom_rect(aes(xmin = -10,, xmax = 30, ymin = 35, ymax = 70), 
            color = "black", fill = NA, size = 0.1, alpha = 0.7) + 
  scale_color_manual(values = c(
    "Sur Global" = "darkgreen",
    "Norte Global" = "purple"
  )) +
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

rm(list = ls())
gc()