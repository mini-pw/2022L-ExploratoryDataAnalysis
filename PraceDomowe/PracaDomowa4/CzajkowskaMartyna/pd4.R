library(plotly)
library(dplyr)

df <- read.csv("bestsellers_with_categories_2022_03_27.csv") %>% 
  filter(Year == "2010")
plot_ly(
  data = df, 
  x = ~User.Rating, 
  type = "violin",
  color = ~Genre,
  colors = "Set1",
  text = paste0("Name: ", df$Name, "<br>Stage: ", df$Stage),
  hoverinfo = 'x+y+text'
  # hovertemplate = paste('<b>%{text}</b><br><b>X</b>: %{x}<br><b>Y</b>: %{y} <extra>tooltip</extra>')
)%>% layout(
  title = "Rozkład ocen użytkowników w roku 2010",
  xaxis = list(title = "Ocena użytkowników"),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "violin"),
             label = "Violinplot"),
        list(method = "restyle",
             args = list("type", "box"),
             label = "Boxplot")
      ))
  ))

