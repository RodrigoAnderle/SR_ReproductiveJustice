

# Tabla S1 - Relación de Artículos Identificados, categorizados y  --------

## Packages
require(tidyverse)
require(openxlsx)

## data
data <- readRDS("data/data_results.rds")


# tabla S1 ----------------------------------------------------------------
# Seleção e modificação dos dados
data_mod <- data %>%
  select(SR, DI, LCS, TC, AU, TI, CT, PY, AU1_CO) %>%
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
    Categorías = CT,
    Año = PY,
    País = AU1_CO
  ) %>% 
  arrange(desc(`Co-Citas`))

# Criar um novo workbook (arquivo Excel)
wb <- createWorkbook()

# Adicionar uma folha ao Excel
addWorksheet(wb, "Tabela S1")

# Escrever os dados na folha
writeData(wb, "Tabela S1", data_mod)

# Adicionar bordas à tabela
# Estilo de bordas padrão
border_style <- createStyle(border = "TopBottomLeftRight", borderColour = "black")

# Obter dimensões da tabela
num_rows <- nrow(data_mod)
num_cols <- ncol(data_mod)

# Aplicar o estilo de bordas à tabela inteira
addStyle(
  wb, 
  sheet = "Tabela S1", 
  style = border_style, 
  rows = 1:(num_rows + 1), # Incluindo cabeçalhos
  cols = 1:num_cols,
  gridExpand = TRUE
)

# Salvar o arquivo Excel com bordas
saveWorkbook(wb, file = "data/tablaS1.xlsx", overwrite = TRUE)
