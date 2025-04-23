# Carregar os pacotes necessários
library(shiny)
library(DT) # Para tabelas interativas
library(dplyr)
library(tidyr)
data <- readRDS("data_results.rds")

# Preparar os dados (use seu código para data_mod aqui)
data_mod <- data %>%
  select(SR, DI, LCS, TC, AU, TI, CT, PY, AU1_CO) %>%
  rename(
    Referencia = SR,
    DOI = DI,
    `Co-Citas` = LCS,
    Citas = TC,
    Autor = AU,
    Título = TI,
    Categorías = CT,
    Año = PY,
    País = AU1_CO
  )

# Adicionar as opções "Todas" nas categorias e países
data_mod <- data_mod %>%
  mutate(
    Categorías = replace_na(Categorías, "Sin Categoría"),
    País = replace_na(País, "Sin País")
  )

# Definir a interface do usuário
ui <- fluidPage(
  titlePanel("Visualización de Datos - Material Suplementario"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtros"),
      textInput("busca_geral", "Buscar un término general:", value = ""),
      selectInput("categoria", "Seleccionar Categoría:", 
                  choices = c("Todas", unique(data_mod$Categorías)), 
                  selected = "Todas"),
      selectInput("pais", "Seleccionar País:", 
                  choices = c("Todas", unique(data_mod$País)), 
                  selected = "Todas"),
      sliderInput("ano", "Seleccionar intervalo de años:", 
                  min = min(data_mod$Año, na.rm = TRUE), 
                  max = max(data_mod$Año, na.rm = TRUE), 
                  value = c(min(data_mod$Año, na.rm = TRUE), max(data_mod$Año, na.rm = TRUE))),
      sliderInput("citas", "Seleccionar intervalo de Citas:", 
                  min = min(data_mod$Citas, na.rm = TRUE), 
                  max = max(data_mod$Citas, na.rm = TRUE), 
                  value = c(min(data_mod$Citas, na.rm = TRUE), max(data_mod$Citas, na.rm = TRUE))),
      sliderInput("co_citas", "Seleccionar intervalo de Co-Citas:", 
                  min = min(data_mod$`Co-Citas`, na.rm = TRUE), 
                  max = max(data_mod$`Co-Citas`, na.rm = TRUE), 
                  value = c(min(data_mod$`Co-Citas`, na.rm = TRUE), max(data_mod$`Co-Citas`, na.rm = TRUE)))
    ),
    mainPanel(
      h3("Tabla Interactiva"),
      dataTableOutput("tabla")
    )
  )
)

# Definir o servidor
server <- function(input, output) {
  # Filtrar os dados com base nos filtros selecionados e na busca geral
  datos_filtrados <- reactive({
    dados <- data_mod %>%
      filter(
        (Categorías == input$categoria | input$categoria == "Todas"),
        (País == input$pais | input$pais == "Todas"),
        Año >= input$ano[1] & Año <= input$ano[2],
        Citas >= input$citas[1] & Citas <= input$citas[2],
        `Co-Citas` >= input$co_citas[1] & `Co-Citas` <= input$co_citas[2]
      )
    
    # Aplicar busca geral, se houver um termo digitado
    if (input$busca_geral != "") {
      dados <- dados %>%
        filter(
          grepl(input$busca_geral, Referencia, ignore.case = TRUE) |
            grepl(input$busca_geral, Autor, ignore.case = TRUE) |
            grepl(input$busca_geral, Título, ignore.case = TRUE) |
            grepl(input$busca_geral, Categorías, ignore.case = TRUE) |
            grepl(input$busca_geral, País, ignore.case = TRUE)
        )
    }
    dados
  })
  
  # Renderizar a tabela interativa
  output$tabla <- renderDataTable({
    datos_filtrados()
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
