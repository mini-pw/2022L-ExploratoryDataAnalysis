df_amazon <- read.csv('amazon_bestsellers.csv')
library(plotly)
library(dplyr)
df_a <- df_amazon %>%
  filter(Genre %in% c("Fiction", "Non Fiction")) %>%
  mutate(Genre = factor(Genre, levels = c("Fiction", "Non Fiction")))
plot_ly(
  data = df_a, 
  x = ~Genre, 
  y = ~Price,
  type = "box"
) %>% layout(
  title = "Price distribution",
  xaxis = list(title = "Genre"),
  yaxis = list(range = c(0, 105)),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "box"),
             label = "Boxplot"),
        list(method = "restyle",
             args = list("type", "violin"),
             label = "Violinplot")
      ))
  ))

