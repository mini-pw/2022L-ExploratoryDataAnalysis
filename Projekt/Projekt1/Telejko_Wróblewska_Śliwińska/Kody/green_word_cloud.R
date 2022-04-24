library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggwordcloud)
dataset = read_sav("ROSES.sav") 

# kolumna z zawodami, które chciałyby wykonywać osoby z grupy naukowców
data11 <- dataset %>% 
  filter(dataset[, 93] >= 3, dataset[, 84] >= 3, dataset[, 96] >= 3,dataset[, 60] <= 2) %>% 
  select(Q18_1)

# utworzenie ramki zawierającej zawody i ich liczność wystąpień wśród badanej grupy
zawod2 <- c("hodowca koni", "rolnik", "mechanik", "kosmetyczka", "lekarz", "kucharz", "nie wiem",
            "żołnierz", "piłkarz", "psycholog", "ratownik medyczny", "dietetyk", "florysta", "operator maszyn",
            "architekt", "prezydent", "kafelkarz", "nauczyciel", "grafik komputerowy", "chirurg plastyczny", "strażak",
            "opiekunka", "muzyk", "cukiernik", "astronom", "aktor", "sadownik", "plantator", "policjant", "weterynarz",
            "informatyk", "pisarz", "artysta", "matematyk", "geolog", "prawnik")
licznosc2 <- c(2, 4, 3, 1, 10, 3, 3, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1)
data12 <- data.frame(zawod2, licznosc2)
data12 <- data12 %>%     mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

# utworzenie chmury słów
set.seed(2)
ggplot(data12, aes(label = zawod2, size = licznosc2, color = licznosc2))+
  geom_text_wordcloud(shape = "circle")+
  theme_minimal()+
  scale_color_gradient(low = "darkgreen", high = "green")+
  scale_size_area(max_size = 8) 
