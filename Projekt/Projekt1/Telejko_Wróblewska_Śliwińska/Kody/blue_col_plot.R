library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggwordcloud)
dataset = read_sav("ROSES.sav") 

# wybór grupy badawczej metafizyków
data1 <- dataset %>% 
  filter(dataset[, 57] == 4, dataset[, 58] == 4, dataset[, 60] == 4, dataset[, 61] == 4, 
         dataset[, 84] <= 2, dataset[, 93] <= 2) 
data1 <-data1[!(is.na(data1$Q5_5)), ]

# utworzenie poziomego wykresu dla grupy metafizyków
data1 %>%
  group_by(Q5_5) %>% 
  summarise(n = n()) %>% 
  mutate(Odpowiedzi = c("1 - nie jestem zainteresowany/a", "2", "3", "4 - jestem bardzo zainteresowany/a")) %>% 
  ggplot(aes(x='', y=n/sum(n), fill = Odpowiedzi)) +
  geom_col()+
  scale_fill_manual(values  = c("#addaff", "#49a8f5", "#1a79c7", "#043e6e"))+
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(x = "", y = "Procent osób zainteresowanych w danym stopniu jak powstało życie na ziemi" , title = "Jak bardzo interesuje cię jak powstało życie na ziemi")
