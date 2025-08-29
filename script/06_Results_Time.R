

## Dimensão Temporal

# Aim: To generate one plot of documents published by year and its interaction
# with other categories such as country, axes (ejes) and categories. For the
# final version, was choosed a simplified version of this plot.
# Input: Previous recategorization data.frame file with 1171 documents.
# Output: A combined plot with publications by year, and an additional excel
# file for descriptive analysis of categories and axes (ejes) by year.


##packages
require(tidyverse)
require(bibliometrix)
require(kableExtra)
require(RColorBrewer) 
require(patchwork)
require(openxlsx)
require(reshape2)

data <- readRDS("data/data_Recategorization.rds")

# Plots Tempo -------------------------------------------------------------

## Produção anual (a) ------------------------------------------------------
data %>% 
  select(PY) %>% 
  group_by(PY) %>% 
  summarise(qtd. = n()) %>% 
  #mutate(scale = log10(qtd.)) %>% ggplot(aes(x = PY, y = scale)) +geom_line()
  ggplot(aes(x = PY, y = qtd.)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) + # Gráfico de barras
  geom_text(aes(label = qtd.), vjust= -0.2, size = 5) +
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + # Mostrar cada ano no eixo x
  labs(
    x = NULL,#"Año de publicación", # Rótulo do eixo x em espanhol
    y = NULL,#"Publicaciones" # Rótulo do eixo y em espanhol
    title = "Cantidad total de publicaciones anuales"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)))-> a
jpeg("data/Publicações_no_tempo.jpeg", res = 380, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(a)
dev.off()

data %>% 
filter(
  AU1_CO %in% c(
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
    "Líbano")) %>%
  select(PY) %>% 
  group_by(PY) %>% 
  summarise(qtd. = n()) %>% 
  #mutate(scale = log10(qtd.)) %>% ggplot(aes(x = PY, y = scale)) +geom_line()
  ggplot(aes(x = PY, y = qtd.)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) + # Gráfico de barras
  geom_text(aes(label = qtd.), vjust= -0.2, size = 5) +
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + # Mostrar cada ano no eixo x
  labs(
    x = NULL,#"Año de publicación", # Rótulo do eixo x em espanhol
    y = NULL, #"Publicaciones" # Rótulo do eixo y em espanhol
    title = "Cantidad de publicaciones anuales del Sur Global"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)))-> a_sg
jpeg("data/Publicações_no_tempo.jpeg", res = 380, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(a_sg)
dev.off()



## Produção Anual por Ejes (b) ---------------------------------------
# Criando uma paleta personalizada variando entre azul, vermelho e roxo
custom_contrast_palette <- colorRampPalette(c("red",  "yellow",
                                              "green", "blue",  
                                              "violet"))(5)

data %>% 
  select(PY, Ejes) %>% 
    group_by(PY, Ejes) %>% 
  summarise(qtd. = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = PY, y = qtd., fill = Ejes)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
  scale_fill_manual(values = custom_contrast_palette) + 
  labs(
    x = NULL, #"Año de publicación", # Rótulo do eixo x em espanhol
    y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
    fill = "Ejes" # Título da legenda em espanhol
  ) +
  theme_classic() +
  theme(
        legend.title = element_text(size = 10),
    axis.text.x = element_blank(), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))
  ) -> b
jpeg("data/Publicações_no_tempo_por_categoria.jpeg", res = 380, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(b)
dev.off()

## Ejes Sur Global
data %>% 
  filter(
  AU1_CO %in% c(
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
    "Líbano")) %>%
  select(PY, Ejes) %>%
  group_by(PY, Ejes) %>%
  summarise(qtd. = n(), .groups = 'drop') %>%
  drop_na() %>% 
  ggplot(aes(x = PY, y = qtd., fill = Ejes)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) +
  scale_fill_manual(values = custom_contrast_palette) +
  labs(x = NULL, #"Año de publicación", 
       y = NULL, 
       fill = "Ejes") +
       theme_classic() +
  theme(
        legend.title = element_text(size = 10),
        axis.text.x = element_blank(), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(size = 10, margin = margin(t = 10))) -> b_sg
       jpeg("data/Publicações_no_tempo_por_categoria_SG.jpeg",
         res = 380,
         width = 12,
         height = 6,
         units = "in")
       print(b_sg)
       dev.off()

    
## Tabela Resumo
  # Reorganizando os dados
  tabela_resumo <- data %>%
    mutate(Grupo_Ano = case_when(
      PY >= 2006 & PY <= 2012 ~ "2006-2012",
      PY >= 2013 & PY <= 2018 ~ "2013-2018",
      PY >= 2019 & PY <= 2024 ~ "2019-2024",
      TRUE ~ NA_character_  # Caso existam anos fora dessas faixas
    )) %>%
    group_by(Ejes, Categorías, Grupo_Ano) %>%
    summarise(qtd. = n(), .groups = "drop") %>%
    drop_na %>% 
    pivot_wider(names_from = Grupo_Ano, values_from = qtd., values_fill = 0) %>%
    mutate(`Total por Categoria` = rowSums(across(where(is.numeric)))) %>%
    bind_rows(
      summarise(., across(where(is.numeric), sum), Ejes = "Total", Categorías = "Total")
    ) %>%
    arrange(desc(`Total por Categoria`))    
  
  
  # Exportando para Excel
  write.xlsx(tabela_resumo, "data/tabela_categoriasano.xlsx", 
             colNames = TRUE, rowNames = FALSE, decimal.mark = ",")
  

## Produção Anual por países (c) -------------------------------------------

  TOP10Countries <- data %>% 
    group_by(AU1_CO) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>% 
    arrange(desc(qtd.)) %>% 
    slice_head(n = 10) %>% 
    pull(AU1_CO) 
  
  colors <- c(
    "EE. UU."  = "#3C3B6E",         # Azul do símbolo americano
    "RU" = "#FF4C4C", # Vermelho da Union Jack
    "Canadá" = "#FF0000",       # Vermelho da bandeira canadense
    "Nueva Zelanda" = "#00247D",  # Azul escuro da bandeira
    "Australia" = "#00AEEF",    # Azul característico australiano
    "Brasil" = "#002776",       # Azul da bandeira brasileira
    "Sudáfrica" = "#000000",   # Preto da bandeira sul-africana
    "Irlanda" = "#FF883E",       # Laranja da bandeira irlandesa
    "Suecia" = "#FECC00",
    "India" = "#FF9933",
    "Otros" = "gray"
  )  
                  
                             
  data %>% 
    mutate(AU1_CO = case_when(!(AU1_CO %in% TOP10Countries) ~ "Otros", 
                              TRUE ~ AU1_CO)) %>% 
    mutate(AU1_CO = factor(AU1_CO, 
                           levels = c(TOP10Countries, "Otros"))) %>% 
    group_by(PY, AU1_CO) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>%
    #filter(AU1_CO != "EE. UU.") %>% 
    ggplot(aes(x = PY, y = qtd., fill = AU1_CO)) + 
    geom_bar(stat = "identity", alpha = 0.8) + # Gráfico de barras com transparência ajustada
    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
    scale_fill_manual(values = colors) + 
    labs(
      x = "Año de publicación", # Rótulo do eixo x em espanhol
      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
      fill = "Países" # Título da legenda em espanhol
    ) +
    theme_classic() +
    theme(
      legend.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 12)
      #legend.key.size = unit(0.5, "cm")
    ) -> c
  jpeg("data/Publicações_no_tempo_por_país.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(c)
  dev.off()

  ## Sul Global
  TOP10Countries_SG <- data %>% 
    filter(
      AU1_CO %in% c(
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
        "Líbano")) %>% 
    group_by(AU1_CO) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>% 
    arrange(desc(qtd.)) %>% 
    slice_head(n = 10) %>% 
    pull(AU1_CO) 
  
  colors_sul_global <- c(
    "Brasil" = "#002776",       # Azul da bandeira brasileira
    "Sudáfrica" = "#000000",   # Preto da bandeira sul-africana
    "India" = "#FF9933",        # Laranja da bandeira indiana
    "China" = "#FF0000",        # Vermelho da bandeira chinesa
    "Argentina" = "#75AADB",    # Azul celeste da bandeira argentina
    "México" = "#006847",       # Verde escuro da bandeira mexicana
    "Uruguay" = "#0057B7",      # Azul das faixas da bandeira uruguaia
    "Chile" = "#0033A0",        # Azul profundo da bandeira chilena
    "Colombia" = "#FCD116",     # Amarelo da bandeira colombiana
    "Costa Rica" = "#D52B1E",    # Vermelho da faixa central da bandeira costarriquenha
    "Otros" = "gray"
  )
  
  data %>% 
    filter(
      AU1_CO %in% c(
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
        "Líbano")) %>% 
    mutate(AU1_CO = case_when(!(AU1_CO %in% TOP10Countries_SG) ~ "Otros", 
                              TRUE ~ AU1_CO)) %>% 
    mutate(AU1_CO = factor(AU1_CO, 
                           levels = c(TOP10Countries_SG, "Otros"))) %>% 
    group_by(PY, AU1_CO) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>%
    ggplot(aes(x = PY, y = qtd., fill = AU1_CO)) + 
    geom_bar(stat = "identity", alpha = 0.8) + # Gráfico de barras com transparência ajustada
    scale_x_continuous(breaks = seq(2006, 2024, by = 1)) + 
    scale_fill_manual(values = colors_sul_global) + 
    labs(
      x = "Año de publicación", # Rótulo do eixo x em espanhol
      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
      fill = "Países" # Título da legenda em espanhol
    ) +
    theme_classic() +
    theme(
      legend.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 12)
      #legend.key.size = unit(0.5, "cm")
    ) -> c_sg
  jpeg("data/Publicações_no_tempo_por_país.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(c_sg)
  
  dev.off()
  

# Produção Anual por categoria principal (d) ------------------------------
  top_categorias_por_eje <- data %>%
    drop_na(Ejes, Categorías) %>%
    group_by(Ejes, Categorías) %>%
    summarise(qtd = n(), .groups = 'drop') %>%
    group_by(Ejes) %>%
    slice_max(order_by = qtd, n = 2, with_ties = FALSE) %>% 
    pull(Categorías) 
    
  
    data %>% 
    mutate(Categorías = case_when(!(Categorías %in% top_categorias_por_eje) ~ "Otros", 
                              TRUE ~ Categorías)) %>% 
    mutate(Categorías = factor(Categorías, 
                           levels = c(top_categorias_por_eje, "Otros"))) %>% 
    group_by(PY, Categorías) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>%
    #filter(AU1_CO != "EE. UU.") %>% 
    ggplot(aes(x = PY, y = qtd., fill = Categorías)) + 
    geom_bar(stat = "identity", alpha = 0.8) + # Gráfico de barras com transparência ajustada
    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
    scale_fill_manual(values = c("darkred","red", "gold","yellow",
                                   "darkgreen","green", 
                                   "darkblue","blue",  
                                   "violet", "orchid", "gray")) + 
    labs(
      x = "Año de publicación", # Rótulo do eixo x em espanhol
      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
      fill = "Categorías" # Título da legenda em espanhol
    ) +
    theme_classic() +
    theme(
      legend.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 12)
      #legend.key.size = unit(0.5, "cm")
    ) -> d
  jpeg("data/Publicações_no_tempo_por_Categorias.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(d)
  dev.off()

### Categorías Sul Global
  top_categorias_por_eje <- data %>%
    filter(
      AU1_CO %in% c(
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
        "Líbano")) %>% 
    drop_na(Ejes, Categorías) %>%
    group_by(Ejes, Categorías) %>%
    summarise(qtd = n(), .groups = 'drop') %>%
    group_by(Ejes) %>%
    slice_max(order_by = qtd, n = 2, with_ties = FALSE) %>% 
    pull(Categorías) 
  
  
  data %>% 
    filter(
      AU1_CO %in% c(
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
        "Líbano")) %>% 
    mutate(Categorías = case_when(!(Categorías %in% top_categorias_por_eje) ~ "Otros", 
                                  TRUE ~ Categorías)) %>% 
    mutate(Categorías = factor(Categorías, 
                               levels = c(top_categorias_por_eje, "Otros"))) %>% 
    group_by(PY, Categorías) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>%
    #filter(AU1_CO != "EE. UU.") %>% 
    ggplot(aes(x = PY, y = qtd., fill = Categorías)) + 
    geom_bar(stat = "identity", alpha = 0.8) + # Gráfico de barras com transparência ajustada
    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
    scale_fill_manual(values = c("darkred","red", "gold","yellow",
                                 "darkgreen","green", 
                                 "darkblue","blue",  
                                 "violet", "orchid", "gray")) + 
    labs(
      x = "Año de publicación", # Rótulo do eixo x em espanhol
      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
      fill = "Categorías" # Título da legenda em espanhol
    ) +
    theme_classic() +
    theme(
      legend.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 12)
      #legend.key.size = unit(0.5, "cm")
    ) -> d_sg
  jpeg("data/Publicações_no_tempo_por_categoriasSG.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(d_sg)
  dev.off()
  

## Figura Tempo ------------------------------------------------------------
### organizando figura
figura_completa <- a + b + c + d +
  plot_layout(ncol = 2) # Organizar lado a lado (ou use nrow para empilhar)

# Mostrar a figura
jpeg("data/Publicações_no_tempo.jpeg", res = 380, width = 30, height = 15, 
     units = "in") # Unidades definidas como polegadas
print(figura_completa)
dev.off()

figura_completa_sg <- a_sg + b_sg + c_sg + d_sg +
  plot_layout(ncol = 2) # Organizar lado a lado (ou use nrow para empilhar)

# Mostrar a figura
jpeg("data/Publicações_no_tempoSG.jpeg", res = 380, width = 30, height = 15, 
     units = "in") # Unidades definidas como polegadas
print(figura_completa_sg)
dev.off()






