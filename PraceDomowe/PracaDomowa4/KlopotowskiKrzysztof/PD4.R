
library(ggplot2)
library(plotly)
library(dplyr)

df <- read.csv("bestsellers_with_categories_2022_03_27.csv")

figure <- df %>% count(Genre, Year) %>% 
  plot_ly(
  x = ~Genre,
  y = ~n,
  frame = ~Year,
  color = I("firebrick2"),
  type = 'bar'
) %>% layout(margin = list(t = 75),
             xaxis = list(title = list(text = "Gatunek", standoff = 10), 
                          fixedrange = TRUE),
             yaxis = list(title = "Liczba książek", constrain="domain",
                          range = list(0, 36), fixedrange = TRUE),
             title = list(text = "Gatunki książek na przestrzeni lat", 
                          font = list(size = 20), y = 0.95),
             showlegend = F, bargap = 0.5) %>% animation_opts(1000)

figure

htmlwidgets::saveWidget(as_widget(figure), "wykresPD4.html")
