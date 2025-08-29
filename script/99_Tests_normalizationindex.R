## Normalizaciones


# Packages ----------------------------------------------------------------

require(openxlsx)
require(countrycode)
require(tidyverse)
require(ggrepel)

# Documentos por países ---------------------------------------------------
dir("raw")
data_docs <- data.frame()
for(i in 2006:2024){
  namefile <- paste0("raw/scimagojr country",i,".xlsx")
  readdata <- read.xlsx(namefile)
  readdata$Year <- i
  readdata$Total_Docs <- sum(readdata$Documents)
  readdata$Total_Citations <- sum(readdata$Citations)
  readdata$Total_SelfCitations <- sum(readdata$`Self-citations`)
  
  data_docs <- rbind(data_docs, readdata)
}
data_docs %>% head

## Reading data with all papers
data_papers <- readRDS("data/data_Recategorization.rds")

###Converting countries names to spanish
countries_spanish <- countrycode(data_docs$Country, 
                                 origin = "country.name", 
                                 destination = "cldr.short.es")
data_docs$País <- countries_spanish
data_docs$País[is.na(countries_spanish)] <- data_docs$Country[is.na(countries_spanish)]

### Mergins datas
data_papers <- merge(data_papers, data_docs, by.x = c("AU1_CO", "PY"), 
                     by.y = c("País","Year"), all.x = T)
nrow(data_papers)
table(is.na(data_papers$AU1_CO))
data_papers$TI[is.na(data_papers$AU1_CO)] <- "EE. UU."
## working with only one data
data = data_papers
rm(data_papers, data_docs)


# Creating Normalizing Index for countries -------------------------------------
#data %>%
#  add_count(PY, name = "Total_Area") %>%
#  group_by(PY, AU1_CO) %>%
#  reframe(
#    qtd = n(),
#    Documents = unique(Documents),
#    Total_Area = unique(Total_Area),
#    Total_docs = unique(Total_Docs),
#    TSI = (n() / unique(Documents)) / (unique(Total_Area) / unique(Total_Docs)),
#    RSI = (TSI -1) / (TSI +1)) %>% 
#  write.xlsx("data/PublicaçõesPaís_TSI.xlsx") ## acho q isso está errado

data %>%
  mutate(Total_docs = sum(Documents, na.rm = T)) %>% 
  group_by(AU1_CO) %>%
  reframe(
    qtd = n(),
    Documents = sum(Documents),
    Total_Area = nrow(data),
    TSI = (n() / sum(Documents)) / (Total_Area / sum(Total_Docs)),
    RSI = (TSI -1) / (TSI +1)
    ) %>% 
  write.xlsx("data/PublicaçõesGeral_TSI.xlsx")
  
  #ggplot(aes(x = PY, y = RSI, color = AU1_CO))+
  #geom_line() +
  #theme_classic()
  

# Plot --------------------------------------------------------------------


data_plot <- data %>%
  mutate(Total_docs = sum(Documents, na.rm = T)) %>% 
  group_by(AU1_CO) %>%
  reframe(
    qtd = n(),
    Documents = sum(Documents),
    Total_Area = nrow(data),
    TSI = (n() / sum(Documents)) / (Total_Area / sum(Total_Docs)),
    RSI = (TSI -1) / (TSI +1),
    Global = ifelse(unique(AU1_CO) %in% c(
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
      "Líbano"), "Sur Global", "Norte Global"),
    nudge_dir = ifelse(RSI > 0, 0.02,-0.02)) 

data_plot %>%
  ggplot(aes(x = RSI, y = log(qtd), 
             label = paste(AU1_CO, "\n", round(RSI,2), 
                           "\n", qtd), color = Global)) +
  geom_text_repel(nudge_x = data_plot$nudge_dir, size = 3, max.overlaps = Inf, fontface = "bold") +
  geom_vline(xintercept = 0, color = "black") +
  scale_color_manual(values = c(
    "Sur Global" = "darkgreen",
    "Norte Global" = "purple"
  )) +
  labs(x = "RSI", y = "Log(Publicaciones)", title = element_blank()) +
  theme_bw() + 
  theme(legend.position = "none") -> a

jpeg("data/RelativeSpecializationIndex.jpeg", res = 300, width = 15, height = 10, 
     units = "in") # Unidades definidas como polegadas
print(a)
dev.off()

