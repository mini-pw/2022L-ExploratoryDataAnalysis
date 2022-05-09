dane <- read.csv('bestsellers_with_categories_2022_03_27.csv')
library(dplyr)
library(tidyverse)
library(plotly)
install.packages("htmlwidgets")

dane1 <- dane %>%
  filter(Price != 0) %>% 
  filter(Year %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%
  mutate(Year = factor(Year, levels = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))

plot1 <- plot_ly(
  data = dane1, 
  x = ~User.Rating, 
  y = ~Price, 
  color = ~Year,
  colors = c("#f5592a", "#fc9312", "#17bf1d", "#33c1f5", "#417ffa", "#b361ff", "#fc81e8"),
  hoverinfo = 'text',
  text = ~paste('</br> Author: ', Author,
                '</br> Title: ', Name)
) %>% 
  layout(title = "Price of books released in the years 2016-2022 depending on users ratings",
         xaxis = list(title = 'Users Rating'), 
         yaxis = list(title = 'Price'), 
         legend = list(title = list(text ='Year')))

htmlwidgets::saveWidget(as_widget(plot1), "index.html")
