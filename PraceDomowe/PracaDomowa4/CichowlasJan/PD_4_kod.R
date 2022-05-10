# WYKRES INTERAKTYWNY

# Załadowanie bibliotek i wczytanie danych

library(dplyr)
library(plotly)
data <- read.csv("PD_4/dane.csv")


# pomysł: najbardziej znany autor (suma Reviews) na przestrzeni lat (animacja)


# Przygotowanie danych (dziesięciu najbardziej popularnych w każdym roku)

data <- data %>% 
  group_by(Author, Year) %>% 
  summarise(Popularity = sum(Reviews)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  top_n(10, Popularity) %>% 
  arrange(desc(Popularity), .by_group = TRUE) %>% 
  bind_cols("Position" = rep(1:10, 14)) # dodajemy tę kolumnę dla łatwiejszego 
                                        # tworzenia wykresu


# Wykres

p <- plot_ly(data = data,
             x = ~Position,
             y = ~Popularity,
             frame = ~Year,
             text = ~Author,
             type = "bar") %>% 
  layout(title = "Top 10 most popular writers",
         showlegend = FALSE,
         xaxis = list(tickvals = 1:10)) %>% 
  animation_opts(redraw = TRUE)

p

