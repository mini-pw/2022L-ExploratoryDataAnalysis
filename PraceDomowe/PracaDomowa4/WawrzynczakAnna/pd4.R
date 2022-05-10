df<- as.data.frame(bestsellers_with_categories_2022_03_27)
library(plotly)
library(ggplot2)
library(dplyr)
install.packages("htmlwidgets")

#Creating a plot that shows the mean ratings of the books from 2009-2022 based
#on the chosen prize

plot_pd4 <- df %>%
  plot_ly(
    x = ~Year, 
    y = ~`User Rating`, 
    color = ~Genre,
    colors = c("salmon", "sienna"),
    frame = ~Price, 
    text = paste0("Name: ", df$Name , "<br>Author: ", df$Author), 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>% 
  layout(title = "Mean users' rating of the best-selling books published between 2009 and 2022 depending on the specified prize",
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Mean users' rating",
                      range = list(0, 5.3)), 
         legend = list(title = list(text ="Genre"))) %>% 
  animation_slider(
    currentvalue = list(prefix = "Prize: ", font = list(color="slategray4"))
  )

plot_pd4

htmlwidgets::saveWidget(as_widget(plot_pd4), "plot_pd4.html")


