library(dplyr)
library(plotly)
library(quantmod)
library(MASS)

books <- read.csv("books.csv")

fig <- books %>%
  plot_ly(type = 'violin')

fig <- fig %>%
  add_trace(
    x = ~ Year[books$Genre == 'Fiction'],
    y = ~ Price[books$Genre == 'Fiction'],
    legendgroup = 'Fiction',
    scalegroup = 'Fiction',
    name = 'Fiction',
    box = list(visible = T),
    meanline = list(visible = T),
    color = I("steelblue")
  )

fig <- fig %>%
  add_trace(
    x = ~ Year[books$Genre == 'Non Fiction'],
    y = ~ Price[books$Genre == 'Non Fiction'],
    legendgroup = 'Non Fiction',
    scalegroup = 'Non Fiction',
    name = 'Non Fiction',
    box = list(visible = T),
    meanline = list(visible = T),
    color = I("gold")
  )

fig <- fig %>%
  layout(
    xaxis = list(title = "Year",
                 rangeslider = list(type = "date")) ,
    yaxis = list(title = "Price",
                 zeroline = F),
    updatemenus = list(list(
      x = 1 ,
      y = 1 ,
      buttons = list(
        list(
          method = "restyle",
          args = list("type", "violin"),
          label = "Violin"
        ) ,
        list(
          method = "restyle",
          args = list("type", "box"),
          label = "Boxplot"
        )
      )
    )) ,
    
    violinmode = 'group',
    boxmode = 'group'
  )

fig

#htmlwidgets::saveWidget(as_widget(fig), "interakcja.html")






