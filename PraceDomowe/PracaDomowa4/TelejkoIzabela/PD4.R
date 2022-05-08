library(dplyr)
library(plotly)
library(RColorBrewer)

df <- read.csv("bestsellers_with_categories.csv")

df <- df %>% filter(Year > 2011) %>% 
  mutate(Year = paste(Year, Genre, sep = " - "))

vcol <- colorRampPalette(brewer.pal(11, "Dark2"))(11)
mycolors <- vector(class(vcol), 22)
mycolors[c(TRUE, FALSE)] <- vcol
mycolors[c(FALSE, TRUE)] <- vcol

fig <- plot_ly(df, 
               x = ~Reviews, 
               y = ~User.Rating, 
               color = ~Year,
               colors = mycolors,
               type = 'scatter', 
               mode = 'markers',
               hoverinfo = 'text',
               text = ~paste('</br> Author: ', Author,
                             '</br> Title: ', Name)) %>%
  layout(title = 'Ratings of books released in the years 2012-2022 depending on number of reviews', 
         plot_bgcolor = "#FBFBFB", 
         xaxis = list(title = 'Number of reviews'), 
         yaxis = list(title = 'Rating'), 
         legend = list(title=list(text='Year and Genre')),
         updatemenus = list(
           list(
             active = -1,
             y = 0.8,
             type = "buttons",
             buttons = list(
               list(method = "update",
                    args = list(list(visible = c(TRUE, FALSE)),
                                list(title = "Ratings of fictional books released in the years 2012-2022 depending on number of reviews")),
                    label = "Fiction"),
               
               list(method = "update",
                    args = list(list(visible = c(FALSE, TRUE)),
                                list(title = "Ratings of non-fictional books released in the years 2012-2022 depending on number of reviews")),
                    label = "Non Fiction"),
               
               list(method = "update",
                    args = list(list(visible = c(TRUE, TRUE)),
                                list(title = "Ratings of books released in the years 2012-2022 depending on number of reviews")),
                    label = "All")
))))

fig

