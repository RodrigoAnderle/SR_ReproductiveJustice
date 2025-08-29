


require(tidyverse)
require(treemapify)

data <- readRDS("data/data_ejes_results.rds")
data <- rename(data, EJES = "EJES.(EIXOS)")

custom_contrast_palette <- colorRampPalette(c("red",  "yellow",
                                              "green", "blue",  
                                              "violet"))(5)

data %>%
  group_by(EJES, Categorías) %>%
  summarize(qtd = n(), .groups = "drop") %>%
  drop_na() %>% 
  ggplot(aes(area = qtd, fill = EJES, label = Categorías, subgroup = EJES)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white") +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "white") +
  labs(title = element_blank())+
  scale_fill_manual(values = custom_contrast_palette) +
  theme(legend.position = "none") -> a

jpeg("data/EJES_Categorías.jpeg", res = 380, width = 30, height = 15, 
     units = "in") # Unidades definidas como polegadas
print(a)
dev.off()

  

