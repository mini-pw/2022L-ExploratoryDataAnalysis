library(readr)
bestsellers<- read_csv("MATLAB/archive/bestsellers_with_categories_2022_03_27.csv")
View(bestsellers)
library(dplyr)
library(plotly)
tab <-bestsellers %>% 
  group_by(Author) %>% 
  summarise(ile = n()) %>% 
  arrange(-ile) %>% 
  head(10) %>% 
  left_join(bestsellers, by = 'Author')
plot_ly(
  data = tab,
  x=~Price,
  y=~Reviews,
  color=~Author,
  colors = c('lightgreen', 'lightblue', 'darkred', 'darkblue', 'yellow', 'brown', 'darkgreen', 'grey', 'black','orange'),
  type = "scatter",
  marker = list(size = 8)
) %>% 
  layout(title="Correlation between price and review<br><sup>by Authors</sup>")

