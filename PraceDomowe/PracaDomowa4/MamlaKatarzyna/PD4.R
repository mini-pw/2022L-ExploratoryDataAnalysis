read.csv('book.csv')-> books
install.packages('plotly')
library(plotly)
library(dplyr)


books %>%
  select(User.Rating,Reviews,Price,Year,Genre) %>%
  group_by(Year, Genre) %>% 
  transmute('Mean Rating'=mean(User.Rating),
            'Mean number of Reviews'= mean(Reviews),
            'Mean price'= mean(Price)) %>%  distinct() %>%  ungroup()-> tab



plot_ly(tab, x = ~Year) %>%
  add_lines(y = ~`Mean Rating`, color = ~Genre, colors='Set2') %>%
  add_lines(y = ~`Mean number of Reviews`, color = ~Genre,colors='Set2', visible = FALSE) %>%
  add_lines(y=~`Mean price`,  color = ~Genre,colors='Set2', visible = FALSE) %>% 
  layout(xaxis = list(title = "Years"),
         updatemenus = list(
           list(
             type = "list",
             x = 0.25,
             y = 1.15,
             buttons = list(
               list(
                 method = "update",
                 args = list(list(visible= list(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
                             list(title ="Average user rating over the years",
                                  yaxis = list(title = "User rating"))),
                 
                 label = "Average user rating"),
               
               list(
                 method = "update",
                 args = list(list(visible=list( FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)),
                             list(title='Average number of reviews over the years',
                                  yaxis = list(title =  "Number or reviews"))),
                            
                 label = "Average number of reviews"),
               list(
                 method = "update",
                 args = list(list(visible=list( FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)),
                             list(title='Average book price over the years',
                                  yaxis = list(title = "Book price ($)"))),
                             
                 label = "Average book price")
             ))))

  