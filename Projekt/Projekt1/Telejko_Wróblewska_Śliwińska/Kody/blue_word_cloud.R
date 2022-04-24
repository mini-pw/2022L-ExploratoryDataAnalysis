library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggwordcloud)
dataset = read_sav("ROSES.sav") 

# kolumna z zawodami, które chciałyby wykonywać osoby z grupy metafizyków
data1 <- dataset %>% 
  filter(dataset[, 57] == 4, dataset[, 58] == 4, dataset[, 60] == 4, dataset[, 61] == 4, 
         dataset[, 84] <= 2, dataset[, 93] <= 2) %>% 
  select(Q18_1)

# utworzenie ramki zawierającej zawody i ich liczność wystąpień wśród badanej grupy
zawod <- c("informatyk", "ogrodnik", "kosmetyczka", "weterynarz", "nie wiem", 
           "psycholog", "fotograf", "lekarz", "prokurator", "lingwista", "matematyk",
           "malarz", "instruktor jazdy konnej", "architekt", "tłumacz", "pisarz", "aniamtor", 
           "ilustrator", "polityk", "fryzjer", "poligrafik", "wychowawca", "elektryk", "raper", 
           "deweloper", "piosenkarz", "grafik komputerowy", "aktor", "kucharz", "chemik", "wizażysta",
           "trener personalny", "nauczyciel", "artysta", "pilot")
licznosc <- c(4, 1, 5, 1, 6, 5, 1, 3, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 2, 1, 1, 1, 1)
data10 <- data.frame(zawod, licznosc)

# utworzenie chmury słów
set.seed(1)
ggplot(data10, aes(label = zawod, size = licznosc, color = licznosc))+
  geom_text_wordcloud(shape = "circle")+
  theme_minimal()+
  scale_color_gradient(low = "darkblue", high = "blue")+
  scale_size_area(max_size = 8) 