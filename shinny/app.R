# Carregar pacotes
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

# Carregar os dados
dados_raw <- readRDS("data_results.rds")

# Preparar os dados
data_mod <- dados_raw %>%
  select(LCS, TC, SR, TI, Ejes, Categorías, AU1_CO, PY, DI,  AU) %>%
  mutate(
    AU = str_to_title(AU),
    TI = str_to_title(TI)
  ) %>%
  rename(
    Referencia = SR,
    DOI = DI,
    `Co-Citas` = LCS,
    Citas = TC,
    Autor = AU,
    Título = TI,
    Año = PY,
    País = AU1_CO
  ) %>%
  mutate(
    Categorías = replace_na(Categorías, "Sin Categoría"),
    País = replace_na(País, "Sin País")
  ) %>%
  arrange(desc(`Co-Citas`))

# Interface
ui <- fluidPage(
  titlePanel("Visualización de Datos - Material Suplementario"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtros"),
      textInput("busca_geral", "Buscar un término general:", value = ""),
      selectInput("ejes", "Seleccionar Eje:",
                  choices = c("Todas", sort(unique(data_mod$Ejes))),
                  selected = "Todas", multiple = TRUE),
      selectInput("categoria", "Seleccionar Categoría:",
                  choices = c("Todas", sort(unique(data_mod$Categorías))),
                  selected = "Todas", multiple = TRUE),
      selectInput("pais", "Seleccionar País:",
                  choices = c("Todas", sort(unique(data_mod$País))),
                  selected = "Todas", multiple = TRUE),
      sliderInput("ano", "Seleccionar intervalo de años:",
                  min = min(data_mod$Año, na.rm = TRUE),
                  max = max(data_mod$Año, na.rm = TRUE),
                  value = range(data_mod$Año, na.rm = TRUE),
                  sep = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla",
                 textOutput("resumo"),
                 DTOutput("tabla")),
        tabPanel("Gráficos",
                 h4("Documentos por Ano"),
                 plotlyOutput("grafico_ano_barra"),
                 br(),
                 h4("Documentos por Eje"),
                 plotlyOutput("grafico_eje_barra"),
                 br(),
                 h4("Documentos por Categoría"),
                 plotlyOutput("grafico_categoria_barra"),
                 br(),
                 h4("Documentos por País"),
                 plotlyOutput("grafico_pais_barra"))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    dados <- data_mod %>%
      filter(
        (input$ejes == "Todas" | Ejes %in% input$ejes),
        (input$categoria == "Todas" | Categorías %in% input$categoria),
        (input$pais == "Todas" | País %in% input$pais),
        Año >= input$ano[1] & Año <= input$ano[2]
      )
    
    if (input$busca_geral != "") {
      busca <- tolower(input$busca_geral)
      dados <- dados %>%
        filter(if_any(c(Referencia, Autor, Título, Ejes, Categorías, País),
                      ~ str_detect(tolower(.), busca)))
    }
    
    dados
  })
  
  # Resumo
  output$resumo <- renderText({
    paste("Resultados encontrados:", nrow(datos_filtrados()))
  })
  
  # Tabela
  output$tabla <- renderDT({
    datatable(datos_filtrados(),
              extensions = "Buttons",
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'print'),
                pageLength = 10
              ))
  })
  
  # Gráficos
  output$grafico_ano_barra <- renderPlotly({
    datos_filtrados() %>%
      count(Año) %>%
      mutate(Año = as.character(Año)) %>%
      plot_ly(x = ~Año, y = ~n, type = 'bar',
              marker = list(color = 'purple')) %>%
      layout(title = "Artigos por Ano",
             xaxis = list(title = "Ano"),
             yaxis = list(title = "Quantidade"))
  })
  
  output$grafico_eje_barra <- renderPlotly({
    datos_filtrados() %>%
      count(Ejes, sort = TRUE) %>%
      top_n(15, n) %>%
      plot_ly(x = ~reorder(Ejes, n), y = ~n, type = 'bar',
              marker = list(color = '#2ca02c')) %>%
      layout(title = "Artigos por Eje (Top 15)",
             xaxis = list(title = "Eje"),
             yaxis = list(title = "Quantidade"))
  })
  
  output$grafico_categoria_barra <- renderPlotly({
    datos_filtrados() %>%
      count(Categorías, sort = TRUE) %>%
      top_n(15, n) %>%
      plot_ly(x = ~reorder(Categorías, n), y = ~n, type = 'bar',
              marker = list(color = '#d62728')) %>%
      layout(title = "Artigos por Categoría (Top 15)",
             xaxis = list(title = "Categoría"),
             yaxis = list(title = "Quantidade"))
  })
  
  output$grafico_pais_barra <- renderPlotly({
    datos_filtrados() %>%
      count(País, sort = TRUE) %>%
      top_n(15, n) %>%
      plot_ly(x = ~reorder(País, n), y = ~n, type = 'bar',
              marker = list(color = 'blue')) %>%
      layout(title = "Artigos por País (Top 15)",
             xaxis = list(title = "País"),
             yaxis = list(title = "Quantidade"))
  })
}

# Executar o app
shinyApp(ui = ui, server = server)
