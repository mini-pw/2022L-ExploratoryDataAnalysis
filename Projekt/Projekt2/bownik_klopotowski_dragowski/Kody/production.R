# library(dplyr)
# library(ggplot2)
# library(plotly)
# library(readxl)

df <- read.csv("dane/emission_intensities.csv")

prod <- df %>% 
  filter(Element == "Production") %>% 
  select(Area, Item, Year, Value)

temp <- data.frame(table(prod$Item))
temp <- temp %>% 
  filter(Freq == 399)

prod <- prod %>% 
  filter(Item %in% temp$Var1) 

reg <- unique(prod$Area)

pal <- c("#820a0a","#e3e181", "#dbbad6", "#33bdb6", "#2f398a", "#7a629e", "#c93853", "#91ba84", "#d97c18")


figprod <- plot_ly(data = prod %>% 
                 filter(Area == "World"),
               x = ~Year,
               y = ~Value,
               color = ~Item,
               type = 'scatter', 
               mode = 'lines+markers',
               colors = pal)

for (i in reg[2:7]){
  figprod <- figprod %>% 
    add_trace(data = prod %>% 
                filter(Area == i),
              x = ~Year,
              y = ~Value,
              color = ~Item,
              type = 'scatter',
              mode = 'lines+markers',
              colors = pal,
              visible = FALSE)
}


t_or_f <- rep(FALSE, 63)
t_or_f[1:9] <- TRUE

buttons <- list(
  list(method = "update",
       args = list(list(visible = t_or_f),
                   list(yaxis = list(range = c(0, signif(2179613222, 2))), # Ta duża liczba to produkcja światowa
                        title = list(text = "Production of food in kilograms in World", 
                                     xanchor = "left", x = 0.25))),
       label = "World"))

for (i in 2:7){
  t_or_f <- rep(FALSE, 63)
  t_or_f[((i-1)*9 + 1):(i*9)] <- TRUE

  
  buttons <- append(buttons, list( list(
    method = "update",
    args = list(list(visible = t_or_f),
                list(yaxis = list(range = c(0, signif(745126355, 2))), # Ta duża liczba to produkcja kontynentalna
                     title = list(text = paste("Production of food in kilograms in", reg[i]), 
                                  xanchor = "left", x = 0.25))), 
    label = reg[i])))
  
}

figprod <- figprod %>% 
  layout(
    title = "Production of food in kilograms in World",
    xaxis = list( tickmode = "array", 
                  tickvals = c(1961, 1970, 1980, 1990, 2000, 2010, 2017) ),
    updatemenus = list(
      list(xanchor="left",
        x = 1.05,
        y = 0.53,
        buttons = buttons,
        type = "buttons"))
    )

figprod

