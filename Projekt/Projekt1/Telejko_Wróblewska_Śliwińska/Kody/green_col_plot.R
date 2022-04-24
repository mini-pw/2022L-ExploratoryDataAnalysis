library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggwordcloud)
dataset = read_sav("ROSES.sav") 

# wybór grupy badawczej naukowców
data2 <- dataset %>% 
  filter(dataset[, 93] >= 3, dataset[, 84] >= 3, dataset[, 96] >= 3,dataset[, 60] <= 2) 
data2 <-data2[!(is.na(data2$Q5_5)), ]

# utworzenie poziomego wykresu dla grupy naukowców
data2 %>%
  group_by(Q5_5) %>% 
  summarise(n = n()) %>% 
  mutate(Odpowiedzi = c("1 - nie jestem zainteresowany/a", "2", "3", "4 - jestem bardzo zainteresowany/a")) %>% 
  ggplot(aes(x='', y=n/sum(n), fill = Odpowiedzi)) +
  geom_col()+
  scale_fill_manual(values  = c("#dbffec", "#88c599", "#3a754e", "#014421"))+
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(x = "", y = "Procent osób zainteresowanych w danym stopniu jak powstało życie na ziemi",  title = "Jak bardzo interesuje cię jak powstało życie na ziemi" )
