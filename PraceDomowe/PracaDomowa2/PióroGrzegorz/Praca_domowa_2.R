datab <- demo_fmonth_1_Data
library(stringi)
library(dplyr)
library(ggplot2)
options(scipen = 7)
numery<- stri_replace_all_regex(datab$Value, " ","")
numery <- as.numeric(numery)
datab$Value = numery

datab %>% 
  filter(MONTH != 'Unknown') %>% 
  select(TIME, Value) %>% 
  group_by(TIME) %>% 
  summarise(Suma = sum(Value)/1000) %>% 
  ggplot(aes(x = TIME, y = Suma))+
  geom_col(color = "royalblue1", fill = "royalblue1", width = 0.5, position = 'dodge') + 
  labs(title = 'Liczba ¿ywych urodzeñ w Polsce w latach 2002 - 2019', x = 'Rok', y  = 'Liczba urodzeñ (w tys.)')
