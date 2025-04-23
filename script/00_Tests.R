
# testes

# Gerando um excel para conferência manual --------------------------------
table(data$DT)

data %>% 
  select(SR, TC, PY, TI, AB, DI, DT) %>% 
  write.csv2("data/sourced_1084_data.csv")


# 02_deduplicating --------------------------------------------------------



table(is.na(source2$DI & source2$SR))

sourcena <- source2[which(is.na(source2$DI) & is.na(source2$SR)),]
nrow(source2[which(!is.na(source2$DI) & is.na(source2$SR)),])

source2 %>% nrow
distinct(tolower(DI), .keep_all = TRUE) %>% nrow
distinct(tolower(SR), .keep_all = TRUE) -> source3



## dealing with nas
table(is.na(sourcena$DI))
table(is.na(sourcena$SR))
str(sourcena)
table(is.na(sourcena$scielo_ID))
length(unique(sourcena$scielo_ID)) #são 16 artigos do Scielo

#### Missing Information
source %>% 
  distinct(tolower(DI), .keep_all = TRUE) %>% 
  distinct(tolower(TI), .keep_all = TRUE) %>% 
  filter(is.na(DI)) %>% nrow

source %>% 
  distinct(tolower(DI), .keep_all = TRUE) %>% 
  distinct(tolower(TI), .keep_all = TRUE) %>% 
  distinct(tolower(SR), .keep_all = TRUE) %>% 
  filter(is.na(TI)) %>% nrow



