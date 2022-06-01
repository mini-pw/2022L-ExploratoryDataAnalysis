library(plotly)
library(tidyverse)
library(dplyr)

df <- read.csv('C:/Users/Jacek.LAPTOK/Downloads/bestsellers_with_categories_2022_03_27.csv')
View(df)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

fig <- plot_ly(df, x = ~User.Rating, y = ~Price, color = ~Genre, size = ~Reviews, frame = ~Year, text = ~Name, type = 'scatter',hoverinfo = "text",mode = 'markers')


fig <- fig %>% layout(
  title = list(text = "Najardziej sprzedaj¹ce siê ksi¹¿ki w Amazon", y = 1),
  xaxis = list(domain = c(0.1, 1), title = "Ocena u¿ytkowników"),
  yaxis = list(title = "Cena"))%>%
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  ) %>% 
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="black"))
  )

fig

