library(dplyr)
library(ggplot2)
library(plotly)


plot_ly(
  data = bestsellers, 
  x = ~Price, 
  y = ~Reviews, 
  color = ~factor(Year),
  colors = "Set1",
  type = "scatter",
  mode = "markers",
  text = ~paste("Price:", Price, '<br>Reviews:', Reviews)
) %>% layout(
  title = 'Number of reviews corresponding to price',
  updatemenus = list(
    list(
      active = -1,
      y = 0.8,
      buttons = list(
        list(
          method = "update",
          args = list(list(visible = c(TRUE, FALSE)),
                      list(title = 'Number of reviews corresponding to price in Odd Years')),
          label = "Odd Years")
        ,
        list(
          method = "update",
          args = list(list(visible = c(FALSE, TRUE)),
                      list(title = 'Number of reviews corresponding to price in Even Years')),
          label = 'Even Years')
      )
        )
      )
    )
 

