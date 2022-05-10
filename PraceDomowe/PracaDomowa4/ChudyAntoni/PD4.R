library(plotly)
library(dplyr)

dfc <- read.csv("C:/Users/Admin/Desktop/PD4/bestsellers.csv")

srednie <- dfc %>% 
  filter(Genre == "Fiction") %>% 
  group_by(Author) %>% 
  summarise(opinie = mean(User.Rating))

cena <- dfc %>% 
  filter(Genre == "Fiction") %>% 
  group_by(Author) %>% 
  summarise(cena = mean(Price)) %>% 
  left_join(srednie, by="Author") %>% 
  mutate(gatunek = TRUE)

srednie2 <- dfc %>% 
  filter(Genre == "Non Fiction") %>% 
  group_by(Author) %>% 
  summarise(opinie = mean(User.Rating))

cena2 <- dfc %>% 
  filter(Genre == "Non Fiction") %>% 
  group_by(Author) %>% 
  summarise(cena = mean(Price)) %>% 
  left_join(srednie2, by="Author") %>% 
  mutate(gatunek = FALSE)

e <- rbind(cena, cena2)
k <- e %>% 
  arrange(-cena) 
k <- k[1:20,]
k1 <- k %>% 
  filter(opinie >= 4.8 & gatunek == T) %>% 
  mutate(logiczna = "Fiction; opinie >= 4.8")
k2 <- k %>% 
  filter(opinie >= 4.8 & gatunek == F) %>% 
  mutate(logiczna = "Non Fiction; opinie >= 4.8")

k3 <- k %>% 
  filter(opinie < 4.8 & gatunek == T) %>% 
  mutate(logiczna = "Fiction; opinie < 4.8")
k4 <- k %>% 
  filter(opinie < 4.8 & gatunek == F) %>% 
  mutate(logiczna = "Non Fiction; opinie < 4.8")

wynik <- rbind(k1,k2,k3,k4)

plot_ly(
  data = wynik, 
  x = ~reorder(Author, c(1:20)), 
  y = ~cena,
  color =~logiczna,
  colors = "Set1",
  type = "bar") %>% 
  layout(
  title = "20 autorow z najwiekszymi srednimi cenami swoich dziel",
  xaxis = list(title = "Autor"),
  yaxis = list(range = c(0, 110)),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "bar"),
             label = "Wykres slupkowy"),
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Wykres punktowy")
      ))
  ))













