library(dplyr)
library(plotly)
library(stringr)
library(tidyverse)
bestsellers <- read.csv2('bestsellers_with_categories_2022_03_27.csv', sep = ',')

word_count <- table(unlist(strsplit(tolower(bestsellers$Name), " ")))
word_count <- as.data.frame(word_count)
colnames(word_count)<- c('Word', 'Count')



word_count <- toString(bestsellers$Name)
word_count %>% 
  tolower() %>% 
  str_replace_all('[^a-z]',' ') -> word_count
str_count(word_count)  
word_count = toString(word_count)
typeof(word_count)
word_count <- as.data.frame(table(as.data.frame(strsplit(word_count, " ")[[1]])))
colnames(word_count)<- c('Word', 'Count')

word_count %>% 
  filter(Count>4) %>% 
  arrange(-Count) %>% 
  slice(-1)-> word_count

word_count %>% 
  slice(c(-9,-42,-43,-60,-97,-157,-192,-197)) -> word_count



fig <- plot_ly(
  
  x = ~reorder(word_count$Word,-word_count$Count),
  
  y = ~word_count$Count,
  
  name = "Most popular words used in titles",
  
  type = "bar"

)
fig
fig <- fig %>% layout(title = "Most popular words used in titles",
                      
                      xaxis = list(title = "Word", tickangle = -45,  rangeslider = list(type = "count")),
                      
                      yaxis = list(title = "Number of occurrences"),
                      
                      margin = list(b = 100),
                      
                      barmode = 'group')
fig
