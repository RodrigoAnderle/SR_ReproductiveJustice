

## Dimensão Temporal
##packages
require(tidyverse)
require(bibliometrix)
require(kableExtra)
require(RColorBrewer) 
require(patchwork)
require(openxlsx)
require(reshape2)

data <- readRDS("data/data_results.rds")

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
    "Uruguay")) %>%
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



## Produção Anual por categorias (b) ---------------------------------------
# Criando uma paleta personalizada variando entre azul, vermelho e roxo
custom_contrast_palette <- colorRampPalette(c("red", "orange", "yellow", 
                                              "green", "blue", "darkblue", 
                                              "violet"))(15)

data %>% 
  select(PY, CT) %>% 
  group_by(PY, CT) %>% 
  summarise(qtd. = n(), .groups = 'drop') %>% 
  ggplot(aes(x = PY, y = qtd., fill = CT)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
  scale_fill_manual(values = custom_contrast_palette) + 
  labs(
    x = NULL, #"Año de publicación", # Rótulo do eixo x em espanhol
    y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
    fill = "Categorías" # Título da legenda em espanhol
  ) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
    axis.text.x = element_blank(), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))
  ) -> b
jpeg("data/Publicações_no_tempo_por_categoria.jpeg", res = 380, width = 12, height = 6, 
     units = "in") # Unidades definidas como polegadas
print(b)
dev.off()

## Categorias do Sul Global
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
    "Uruguay")) %>%
  select(PY, CT) %>%
  group_by(PY, CT) %>%
  summarise(qtd. = n(), .groups = 'drop') %>%
  ggplot(aes(x = PY, y = qtd., fill = CT)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) +
  scale_fill_manual(values = custom_contrast_palette) +
  labs(x = NULL, #"Año de publicación", 
       y = NULL, 
       fill = "Categorías") +
       theme_classic() +
         theme(
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 12),
           #legend.key.size = unit(0.3, "cm"),
           axis.text.x = element_blank(), # element_text(
             #size = 8,
             #angle = 45,
             #hjust = 1,
             #vjust = 0.5),
           #axis.title.x = element_text(size = 12, margin = margin(t = 10))
         ) -> b_sg
       jpeg(
         "data/Publicações_no_tempo_por_categoria_SG.jpeg",
         res = 380,
         width = 12,
         height = 6,
         units = "in"
       )
       print(b_sg)
       dev.off()

## Percentual
  data %>% 
    select(PY, CT) %>% 
    group_by(PY, CT) %>% 
    summarise(qtd. = n(), .groups = 'drop') %>% 
    group_by(PY) %>% 
    mutate(percentual = qtd. / sum(qtd.) * 100) %>%  # Cálculo do percentual
    ggplot(aes(x = PY, y = percentual, fill = CT)) + 
    geom_bar(stat = "identity", alpha = 0.8) +  # Gráfico de barras empilhadas com percentuais
    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Eixo Y em formato de porcentagem
    scale_fill_manual(values = custom_contrast_palette) + 
    labs(
      x = "Año de publicación", 
      y = "Porcentaje",  # Novo rótulo para o eixo Y
      fill = "Categorías"
    ) +
    theme_classic() +
    theme( legend.position = "none",
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_text(size = 10, margin = margin(t = 10))
    ) -> b2
  jpeg("data/Publicações_no_tempo_por_categoria_porcentagem.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(b2)
  dev.off()

## Heatmap
  data %>% 
    group_by(PY, CT) %>% 
    summarise(qtd. = n(), .groups = "drop") %>% 
    pivot_wider(names_from = CT, values_from = qtd., values_fill = 0) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = -PY, names_to = "Categoria", values_to = "Quantidade") %>% 
    ggplot(aes(x = PY, y = Categoria, fill = Quantidade)) +
    geom_tile(color = "white") +  # Tiles com bordas brancas
    scale_fill_gradient(low = "white", high = "purple") + # Gradiente de cor
    scale_x_continuous(
      breaks = seq(min(data$PY), max(data$PY), by = 1),  # Garantindo que o eixo mostre todos os anos da base
      limits = c(min(data$PY), max(data$PY))            # Limitando o eixo aos anos reais da base
    ) +
    labs(
      x = "Año de publicación", 
      y = "Porcentaje",  # Novo rótulo para o eixo Y
      fill = "Categorías"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12)
    ) -> b3
  jpeg("data/Publicações_no_tempo_por_categoria_heatmap.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(b3)
  dev.off()
  

    
## Tabela Resumo
  # Reorganizando os dados
  (tabela_resumo <- data %>% 
    group_by(CT, PY) %>% 
    summarise(qtd. = n(), .groups = "drop") %>% 
    pivot_wider(names_from = CT, values_from = qtd., values_fill = 0) %>% 
    mutate(`Total Anual` = rowSums(select(., -PY))) %>% 
    rename(Ano = PY) %>% 
    mutate(Ano = as.character(Ano)) %>% # Convertendo a coluna "Ano" para texto
    bind_rows(summarise(., across(-Ano, sum), Ano = "Total")) %>% 
    arrange(Ano))
  
  
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
    "Otros" = "purple"
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
        "Uruguay")) %>% 
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
    "Otros" = "purple"
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
        "Uruguay")) %>% 
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
  
  
## Produção Anual por área do conhecimento (d) ------------------------------------
  TOPAreas <- data %>% 
    select(SC) %>% 
    unique() %>% 
    separate_rows(SC, sep = "; ") %>%
    separate_rows(SC, sep = ";") %>%
    group_by(SC) %>%
    summarise(qtd. = n(), .groups = 'drop') %>% 
    arrange(desc(qtd.)) %>% 
    mutate(SC = toupper(SC)) %>% 
    mutate(SC = case_when(
      SC == "WOMEN'S STUDIES" ~ "Estudios de mujeres",
      SC == "PUBLIC, ENVIRONMENTAL \\& OCCUPATIONAL HEALTH" ~ "Salud pública, ambiental y ocupacional",
      SC == "OBSTETRICS \\& GYNECOLOGY" ~ "Obstetricia y ginecología",
      SC == "GOVERNMENT \\& LAW" ~ "Gobierno y derecho",
      SC == "SOCIAL SCIENCES - OTHER TOPICS" ~ "Ciencias sociales - otros temas",
      SC == "SOCIOLOGY" ~ "Sociología",
      SC == "BIOMEDICAL SOCIAL SCIENCES" ~ "Ciencias sociales biomédicas",
      SC == "PSYCHOLOGY" ~ "Psicología",
      SC == "HEALTH CARE SCIENCES \\& SERVICES" ~ "Ciencias y servicios de atención médica",
      SC == "SOCIAL ISSUES" ~ "Temas sociales",
      SC == "CULTURAL STUDIES" ~ "Estudios culturales",
      SC == "HISTORY" ~ "Historia",
      SC == "SOCIAL WORK" ~ "Trabajo social",
      SC == "LITERATURE" ~ "Literatura",
      SC == "NURSING" ~ "Enfermería",
      SC == "COMMUNICATION" ~ "Comunicación",
      SC == "MEDICAL ETHICS" ~ "Ética médica",
      SC == "ETHICS" ~ "Ética",
      SC == "FAMILY STUDIES" ~ "Estudios familiares",
      SC == "REPRODUCTIVE BIOLOGY" ~ "Biología reproductiva",
      SC == "ANTHROPOLOGY" ~ "Antropología",
      SC == "EDUCATION \\& EDUCATIONAL RESEARCH" ~ "Educación e investigación educativa",
      SC == "PEDIATRICS" ~ "Pediatría",
      SC == "RELIGION" ~ "Religión",
      SC == "ETHNIC STUDIES" ~ "Estudios étnicos",
      SC == "GENERAL \\& INTERNAL MEDICINE" ~ "Medicina general e interna",
      SC == "ENVIRONMENTAL SCIENCES \\& ECOLOGY" ~ "Ciencias ambientales y ecología",
      SC == "ARTS \\& HUMANITIES - OTHER TOPICS" ~ "Artes y humanidades - otros temas",
      SC == "LEGAL MEDICINE" ~ "Medicina legal",
      SC == "GEOGRAPHY" ~ "Geografía",
      SC == "OBSTETRICS & GYNECOLOGY" ~ "Obstetricia y ginecología",
      SC == "PSYCHIATRY" ~ "Psiquiatría",
      SC == "CRIMINOLOGY \\& PENOLOGY" ~ "Criminología y penología",
      SC == "INFORMATION SCIENCE \\& LIBRARY SCIENCE" ~ "Ciencia de la información y bibliotecología",
      SC == "SCIENCE \\& TECHNOLOGY - OTHER TOPICS" ~ "Ciencia y tecnología - otros temas",
      SC == "DEMOGRAPHY" ~ "Demografía",
      SC == "PHILOSOPHY" ~ "Filosofía",
      SC == "HISTORY \\& PHILOSOPHY OF SCIENCE" ~ "Historia y filosofía de la ciencia",
      SC == "AREA STUDIES" ~ "Estudios de área",
      SC == "BUSINESS \\& ECONOMICS" ~ "Negocios y economía",
      SC == "DEVELOPMENT STUDIES" ~ "Estudios de desarrollo",
      SC == "LINGUISTICS" ~ "Lingüística",
      SC == "REHABILITATION" ~ "Rehabilitación",
      SC == "RESEARCH \\& EXPERIMENTAL MEDICINE" ~ "Investigación y medicina experimental",
      SC == "ANESTHESIOLOGY" ~ "Anestesiología",
      SC == "BIOCHEMISTRY \\& MOLECULAR BIOLOGY" ~ "Bioquímica y biología molecular",
      SC == "CELL BIOLOGY" ~ "Biología celular",
      SC == "ENDOCRINOLOGY \\& METABOLISM" ~ "Endocrinología y metabolismo",
      SC == "GENETICS \\& HEREDITY" ~ "Genética y herencia",
      SC == "INTERNATIONAL RELATIONS" ~ "Relaciones internacionales",
      SC == "LIFE SCIENCES \\& BIOMEDICINE - OTHER TOPICS" ~ "Ciencias de la vida y biomedicina - otros temas",
      SC == "PHARMACOLOGY \\& PHARMACY" ~ "Farmacología y farmacia",
      SC == "ART" ~ "Arte",
      SC == "ASIAN STUDIES" ~ "Estudios asiáticos",
      SC == "CARDIOVASCULAR SYSTEM \\& CARDIOLOGY" ~ "Sistema cardiovascular y cardiología",
      SC == "EMERGENCY MEDICINE" ~ "Medicina de emergencia",
      SC == "INFECTIOUS DISEASES" ~ "Enfermedades infecciosas",
      SC == "INSTRUMENTS \\& INSTRUMENTATION" ~ "Instrumentos y instrumentación",
      SC == "METEOROLOGY \\& ATMOSPHERIC SCIENCES" ~ "Meteorología y ciencias atmosféricas",
      SC == "MICROBIOLOGY" ~ "Microbiología",
      SC == "ONCOLOGY" ~ "Oncología",
      SC == "SUBSTANCE ABUSE" ~ "Abuso de sustancias",
      SC == "SURGERY" ~ "Cirugía",
      SC == "THEATER" ~ "Teatro",
      SC == "TOXICOLOGY" ~ "Toxicología",
      SC == "TROPICAL MEDICINE" ~ "Medicina tropical",
      TRUE ~ SC  # Mantém os termos não traduzidos como estão
    )) %>%
    mutate(SC = str_to_title(SC)) %>% 
    slice_head(n = 10) %>% 
    pull(SC) 
  
  areas_colors <- colorRampPalette(c("red", "orange", "yellow", 
                                                "green", "blue", "darkblue", 
                                                "violet"))(11)
  
  data %>% 
    separate_rows(SC, sep = ";\\s*") %>%  # Separar por "; " ou ";"
    mutate(SC = toupper(SC)) %>% 
    mutate(SC = case_when(
      SC == "WOMEN'S STUDIES" ~ "Estudios de mujeres",
      SC == "PUBLIC, ENVIRONMENTAL \\& OCCUPATIONAL HEALTH" ~ "Salud pública, ambiental y ocupacional",
      SC == "OBSTETRICS \\& GYNECOLOGY" ~ "Obstetricia y ginecología",
      SC == "GOVERNMENT \\& LAW" ~ "Gobierno y derecho",
      SC == "SOCIAL SCIENCES - OTHER TOPICS" ~ "Ciencias sociales - otros temas",
      SC == "SOCIOLOGY" ~ "Sociología",
      SC == "BIOMEDICAL SOCIAL SCIENCES" ~ "Ciencias sociales biomédicas",
      SC == "PSYCHOLOGY" ~ "Psicología",
      SC == "HEALTH CARE SCIENCES \\& SERVICES" ~ "Ciencias y servicios de atención médica",
      SC == "SOCIAL ISSUES" ~ "Temas sociales",
      SC == "CULTURAL STUDIES" ~ "Estudios culturales",
      SC == "HISTORY" ~ "Historia",
      SC == "SOCIAL WORK" ~ "Trabajo social",
      SC == "LITERATURE" ~ "Literatura",
      SC == "NURSING" ~ "Enfermería",
      SC == "COMMUNICATION" ~ "Comunicación",
      SC == "MEDICAL ETHICS" ~ "Ética médica",
      SC == "ETHICS" ~ "Ética",
      SC == "FAMILY STUDIES" ~ "Estudios familiares",
      SC == "REPRODUCTIVE BIOLOGY" ~ "Biología reproductiva",
      SC == "ANTHROPOLOGY" ~ "Antropología",
      SC == "EDUCATION \\& EDUCATIONAL RESEARCH" ~ "Educación e investigación educativa",
      SC == "PEDIATRICS" ~ "Pediatría",
      SC == "RELIGION" ~ "Religión",
      SC == "ETHNIC STUDIES" ~ "Estudios étnicos",
      SC == "GENERAL \\& INTERNAL MEDICINE" ~ "Medicina general e interna",
      SC == "ENVIRONMENTAL SCIENCES \\& ECOLOGY" ~ "Ciencias ambientales y ecología",
      SC == "ARTS \\& HUMANITIES - OTHER TOPICS" ~ "Artes y humanidades - otros temas",
      SC == "LEGAL MEDICINE" ~ "Medicina legal",
      SC == "GEOGRAPHY" ~ "Geografía",
      SC == "OBSTETRICS & GYNECOLOGY" ~ "Obstetricia y ginecología",
      SC == "PSYCHIATRY" ~ "Psiquiatría",
      SC == "CRIMINOLOGY \\& PENOLOGY" ~ "Criminología y penología",
      SC == "INFORMATION SCIENCE \\& LIBRARY SCIENCE" ~ "Ciencia de la información y bibliotecología",
      SC == "SCIENCE \\& TECHNOLOGY - OTHER TOPICS" ~ "Ciencia y tecnología - otros temas",
      SC == "DEMOGRAPHY" ~ "Demografía",
      SC == "PHILOSOPHY" ~ "Filosofía",
      SC == "HISTORY \\& PHILOSOPHY OF SCIENCE" ~ "Historia y filosofía de la ciencia",
      SC == "AREA STUDIES" ~ "Estudios de área",
      SC == "BUSINESS \\& ECONOMICS" ~ "Negocios y economía",
      SC == "DEVELOPMENT STUDIES" ~ "Estudios de desarrollo",
      SC == "LINGUISTICS" ~ "Lingüística",
      SC == "REHABILITATION" ~ "Rehabilitación",
      SC == "RESEARCH \\& EXPERIMENTAL MEDICINE" ~ "Investigación y medicina experimental",
      SC == "ANESTHESIOLOGY" ~ "Anestesiología",
      SC == "BIOCHEMISTRY \\& MOLECULAR BIOLOGY" ~ "Bioquímica y biología molecular",
      SC == "CELL BIOLOGY" ~ "Biología celular",
      SC == "ENDOCRINOLOGY \\& METABOLISM" ~ "Endocrinología y metabolismo",
      SC == "GENETICS \\& HEREDITY" ~ "Genética y herencia",
      SC == "INTERNATIONAL RELATIONS" ~ "Relaciones internacionales",
      SC == "LIFE SCIENCES \\& BIOMEDICINE - OTHER TOPICS" ~ "Ciencias de la vida y biomedicina - otros temas",
      SC == "PHARMACOLOGY \\& PHARMACY" ~ "Farmacología y farmacia",
      SC == "ART" ~ "Arte",
      SC == "ASIAN STUDIES" ~ "Estudios asiáticos",
      SC == "CARDIOVASCULAR SYSTEM \\& CARDIOLOGY" ~ "Sistema cardiovascular y cardiología",
      SC == "EMERGENCY MEDICINE" ~ "Medicina de emergencia",
      SC == "INFECTIOUS DISEASES" ~ "Enfermedades infecciosas",
      SC == "INSTRUMENTS \\& INSTRUMENTATION" ~ "Instrumentos y instrumentación",
      SC == "METEOROLOGY \\& ATMOSPHERIC SCIENCES" ~ "Meteorología y ciencias atmosféricas",
      SC == "MICROBIOLOGY" ~ "Microbiología",
      SC == "ONCOLOGY" ~ "Oncología",
      SC == "SUBSTANCE ABUSE" ~ "Abuso de sustancias",
      SC == "SURGERY" ~ "Cirugía",
      SC == "THEATER" ~ "Teatro",
      SC == "TOXICOLOGY" ~ "Toxicología",
      SC == "TROPICAL MEDICINE" ~ "Medicina tropical",
      TRUE ~ SC  # Mantém os termos não traduzidos como estão
    )) %>% 
    mutate(SC = str_to_title(SC)) %>% 
    mutate(SC = case_when(
      !(SC %in% TOPAreas) ~ "Otros", 
      TRUE ~ SC)) %>% 
    mutate(SC = factor(SC, 
                       levels = c(TOPAreas, "Otros"))) %>% 
    filter(!is.na(SC)) %>% 
    group_by(PY, SC) %>%
    summarise(qtd. = n(), .groups = "drop") %>% 
    ggplot(aes(x = PY, y = qtd., fill = SC)) + 
    geom_bar(stat = "identity", alpha = 0.8) + 
    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
    scale_fill_manual(values = areas_colors) + 
    labs(
      x = "Año de publicación", # Rótulo do eixo x em espanhol
      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
      fill = "Top 10 Áreas de conocimiento" # Título da legenda em espanhol
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
      axis.title.x = element_text(size = 10, margin = margin(t = 10))
    ) -> d
  jpeg("data/Publicações_no_tempo_por_area.jpeg", res = 380, width = 12, height = 6, 
       units = "in") # Unidades definidas como polegadas
  print(d)
  dev.off()

  
### Por idioma (e)? ---------------------------------------------------------
#
#  data %>% 
#    mutate(Idioma = factor(Idioma, 
#                           levels = c("Inglés", 
#                                      "Español", 
#                                      "Portugués"))) %>%
#    group_by(PY, Idioma) %>% 
#    summarise(qtd. = n(), .groups = 'drop') %>%
#    filter(Idioma == "Portugués") 
#    ggplot(aes(x = PY, y = qtd., fill = Idioma)) + 
#    geom_bar(stat = "identity", alpha = 0.8) + # Gráfico de barras com transparência ajustada
#    scale_x_continuous(breaks = seq(min(data$PY), max(data$PY), by = 1)) + 
#    #scale_fill_manual(values = colors) + 
#    labs(
#      x = "Año de publicación", # Rótulo do eixo x em espanhol
#      y = NULL,#"Publicaciones", # Rótulo do eixo y em espanhol
#      fill = "Idioma" # Título da legenda em espanhol
#    ) +
#    theme_classic() +
#    theme(
#      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5), #element_text(size = 10, angle = 45, hjust = 1, vjust = 0.5),
#      axis.title.x = element_text(size = 10, margin = margin(t = 10))
#    ) -> e
#  jpeg("data/Publicações_no_tempo_por_idioma.jpeg", res = 380, width = 12, height = 6, 
#       units = "in") # Unidades definidas como polegadas
#  print(e)
#  dev.off()
#  


## Figura Tempo ------------------------------------------------------------
### organizando figura
figura_completa <- a + a_sg + b + b_sg + c + c_sg +
  plot_layout(ncol = 2) # Organizar lado a lado (ou use nrow para empilhar)

# Mostrar a figura
jpeg("data/Publicações_no_tempo.jpeg", res = 380, width = 30, height = 15, 
     units = "in") # Unidades definidas como polegadas
print(figura_completa)
dev.off()







