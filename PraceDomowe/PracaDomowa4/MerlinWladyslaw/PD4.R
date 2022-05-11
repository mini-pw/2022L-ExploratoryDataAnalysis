pd41<-read.csv("bestsellers_with_categories_2022_03_27.csv")
library(dplyr)
library(plotly)
 pd41<-pd41 %>% 
   filter(Year == 2022)
## 1 wykres
df1<-pd4 %>% 
  group_by(Author) %>% 
  summarise(Avg.Rating = mean(User.Rating))
m<-plot_ly(
  data = pd41, 
  x = ~User.Rating, 
  y = ~Reviews, 
  symbol = ~Genre, 
  symbols = c('circle', 'square'),
  text = ~paste('Name: ', Name),
  type = "scatter"
) %>% layout(
  title = "Rating reliability",
  xaxis = list(title = c(4,5)),
  yaxis = list(range = c(0,210000)),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "ScatterPlot"),
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "Heatmap")
      ))
  ))
htmlwidgets::saveWidget(m, "wykres1.html")
m

## 2 wykres
p<-plot_ly(
  data = pd41, 
  x = ~User.Rating, 
  y = ~Reviews, 
  z = ~Price,
  color = ~Genre,
  symbol = ~Genre, 
  symbols = c('circle', 'square'),
  text = ~paste('Name: ', Name,"\n", 'Author: ',Author),
  type = "scatter3d",
  mode = "markers") 
p
htmlwidgets::saveWidget(p, "wykres2.html")
