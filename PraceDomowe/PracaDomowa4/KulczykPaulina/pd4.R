library(readr)
library(plotly)
library(dplyr)
library(htmlwidgets)
bestsellers <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/2037178/3378805/bestsellers_with_categories_2022_03_27.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20220506%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220506T201911Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=09f1f17ef613d06e00bdcf2a2fac4f2a4ef272f1c7d5f6edcdf5b0bf40479854601ca926e954ffee27a7383e493058065beeb773a9e7875436a18d08644c34840badd1970d552b7b47e99e97fbaf45afcfc3125174bda5e1d71c50eff0a33d2065c811bfc67f83ba16139d995912e62e6e050a8678fe54b0250dc604ffdd5a9dac9f499662f91e89db0ef3ee0c50bd6b70508522e0dc53ad30a4f26589ea3f316c4650eefd0f2cfc85ebc52794f3d8fd92a3528969905f617683f92e2b5529174911be75c610d11091920ab7cfe68eac1a5ffb3b33497dd2a2ca05ce3534a10696c482e88f0cc6be1c3502089d48053c7c0d853e86d9a4970090773295e21992")

licznosc <- data.frame(bestsellers %>% 
  group_by(Genre, Year) %>% 
  summarise(Amount = n(), .groups = "drop"))

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
