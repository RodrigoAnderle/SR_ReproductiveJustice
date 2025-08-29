
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


