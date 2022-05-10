library(ggplot2)
library(plotly)
library(dplyr)


dane <- read.csv("bestsellers_with_categories_2022_03_27.csv")

fig <- dane %>%
  plot_ly(x = ~Reviews, y = ~Price, 
          text = paste0("Name: ", dane$Name, "<br>Author: ", dane$Author),
          hovertemplate = paste('<b>%{text}</b><br><b>Number of reviews</b>: %{x}<br><b>Price</b>: %{y}$ <extra>tooltip</extra>')
  ) %>%
  layout(xaxis = list(title = "Number of reviews", type = "log"),
         yaxis = list(title = "Price"),
         legend = list(
           title = list(text = "Genre"),
           bgcolor = "#E2E2E2"
         ))

fig <- fig %>%
  add_markers(color = ~Genre, colors = "Set1", frame = ~Year, ids = ~Name) %>%
  animation_opts(1000, easing = "linear", redraw = TRUE) %>%
  animation_button(x = 1.1, y = 0) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color = "black"))
  )

htmlwidgets::saveWidget(as_widget(fig), "wykres.html")
