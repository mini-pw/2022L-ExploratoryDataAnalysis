library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
bestsellers <- read_csv("Praceeksplo/bestsellers_with_categories_2022_03_27.csv")
bestsellers <- bestsellers %>% 
  rename(User_Rating = `User Rating`)
bestsellers$Year <- as.character(bestsellers$Year)
wykres <- plot_ly(
  data = bestsellers,
  x = ~Year,
  y = ~Price,
  color = ~Year,
  type = 'box',
  mode = 'markers',
  hoverinfo = 'text',
  text = ~paste("</br>Year: ", Year,
                "</br>Price: ",Price,
                "</br>Name: ", Name)
) %>% 
  layout(
    title = "Top 50 bestsellers' prices on Amazon (2009-2022)",
    xaxis = list(title = 'Year'),
    yaxis = list(title = "Price (in US dollars)")
)
htmlwidgets::saveWidget(as.widget(wykres), "wykresbestsellers.html")