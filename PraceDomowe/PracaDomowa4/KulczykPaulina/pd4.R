library(readr)
library(plotly)
library(dplyr)
library(htmlwidgets)
bestsellers <-  read_csv("~/Desktop/bestsellers_with_categories_2022_03_27.csv")
names(bestsellers)[3] <- "User.Rating"
licznosc <- data.frame(bestsellers) %>% 
  group_by(Genre, Year) %>% 
  summarise(Amount = n(), .groups = "drop")

wykres_licznosc <- plot_ly(
  data = licznosc, 
  x = ~Genre, 
  y = ~Amount,
  frame = ~Year,
  type = "bar"
) %>% 
  layout(title ='Genre Distribution in 2009 - 2022',
  xaxis = list(title = "Genre"),
  yaxis = list(title = "Amount")
  ) %>% 
  animation_opts(transition = 20) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Year: ",
                                       font = list(color="blue")))

bestsellers_2022 <- bestsellers %>% 
  filter(bestsellers$Year == 2022)
rate_4.9 <- bestsellers_2022 %>% 
  filter(bestsellers_2022$User.Rating == 4.9) %>% 
  top_n(3, Reviews)
rate_4.8 <- bestsellers_2022 %>% 
  filter(bestsellers_2022$User.Rating == 4.8) %>% 
  top_n(3, Reviews)
rate_4.7 <- bestsellers_2022 %>% 
  filter(bestsellers_2022$User.Rating == 4.7) %>% 
  top_n(3, Reviews)
rate_4.6 <- bestsellers_2022 %>% 
  filter(bestsellers_2022$User.Rating == 4.6) %>% 
  top_n(3, Reviews)
rate_4.5_4.3 <- bestsellers_2022 %>% 
  filter(bestsellers_2022$User.Rating<=4.5)
top_bestsellers_2022 <- rbind(rate_4.9 , rate_4.8, rate_4.7, rate_4.6, rate_4.5_4.3)

top_bestsellers_2022[3,1] <- "1.Brown Bear, Brown Bear, What Do You See?\n2.The Very Hungry Caterpillar"
top_bestsellers_2022[3,2] <- "1.Bill Martin Jr.\n2.Eric Carle"
top_bestsellers_2022[6,1] <- "1. The Boy, the Mole, the Fox and the Horse\n2.Where the Crawdads Sing"
top_bestsellers_2022[6,2] <- "1.Charlie Mackesy\n2.Delia Owens"
top_bestsellers_2022 <- top_bestsellers_2022[-c(1,5), -4]

top_bestsellers <- plot_ly(
  data = top_bestsellers_2022, 
  x = ~User.Rating, 
  y = ~Price, 
  color = ~Genre,
  colors = "Set1",
  text = paste0("Title: ", top_bestsellers_2022$Name, "<br>Author: ", top_bestsellers_2022$Author),
  hoverinfo = 'x+y+text'
) %>% layout(title ='Most-reviewed bestsellers 2022',
             xaxis = list(title = "Rate"),
             yaxis = list(title = "Price")
) 
  
saveWidget(top_bestsellers, "top_bestsellers.html", selfcontained = F, libdir = "lib")

saveWidget(wykres_licznosc, "wykres_licznosc.html", selfcontained = F, libdir = "lib")
