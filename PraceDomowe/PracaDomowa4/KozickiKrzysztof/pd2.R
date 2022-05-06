bestsellers <- read.csv("C:/Users/kko.DESKTOP-P1EJRA1/Desktop/dane/bestsellers.csv")


library(plotly)
library(dplyr)

bestsellers <- bestsellers %>% 
  group_by(Year) %>% 
  summarise(User.Rating = mean(User.Rating), Reviews = sum(Reviews), Price = mean(Price))





wykres <- plot_ly(data = bestsellers, type = "scatter", mode = "lines") %>%
  add_lines(x=~Year, y=~User.Rating) %>%
  add_lines(x=~Year, y=~Reviews) %>%
  add_lines(x=~Year, y=~Price) %>% 
  layout(title = "Iloœæ Opini",
         showlegend=FALSE,
         xaxis=list(title="Rok"),
         yaxis=list(title="Iloœæ Opinii"),
         updatemenus=list(
           list(
             active = -1,
             type= 'buttons',
             buttons = list(
               list(
                 label = "Iloœæ Opinii",
                 method = "update",
                 args = list(list(visible = c(FALSE, FALSE, TRUE)),
                             list(title = "Iloœæ Opinii",
                                  yaxis = list(title = "Iloœæ Opinii")))),
               list(
                 label = "Œrednia Ocen",
                 method = "update",
                 args = list(list(visible = c(FALSE, TRUE, FALSE)),
                             list(title = "Œrednia Ocen",
                                  yaxis = list(title = "Œrednia Ocen")))),
               list(
                 label = "Œrednia Cena",
                 method = "update",
                 args = list(list(visible = c(TRUE, FALSE, FALSE)),
                             list(title = "Œrednia Cena",
                                  yaxis = list(title = "Œrednia Cena"))))))))

wykres
