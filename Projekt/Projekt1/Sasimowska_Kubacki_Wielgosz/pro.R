library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(fmsb)
library(RColorBrewer)
df1<-read_excel("Excel_projekt.xlsx")

#**********************************************************

ilu_rodzicow_studiowalo <- df1 %>%
  mutate(studia_rodzicow = ifelse(P4 == 1 & P5 == 1, "2S", ifelse( (P4 == 2 & P5 == 1) | (P4 == 1 & P5 == 2), "1S" , ifelse(P4 == 2 & P5 == 2, "0S", ifelse((P4 == 1 & P5 > 3) | (P4 > 3  & P5 == 1), "1S", "brak_danych"))))) %>%
  filter(studia_rodzicow !="brak_danych") %>%
  select(id_ucznia,studia_rodzicow)

ilu_rodzicow_pracuje <-df1 %>% 
  mutate(praca_rodzicow = ifelse(P6 == 1 & P8 == 1, "2P", ifelse( (P6 == 2 & P8 == 1) | (P6 == 1 & P8 == 2), "1P" , ifelse(P6 == 2 & P8 == 2, "0P", ifelse((P6 == 1 & P8 > 3) | (P6 > 3  & P8 == 1), "1P", "brak_danych"))))) %>%
  filter(praca_rodzicow !="brak_danych") %>% 
  select(id_ucznia,praca_rodzicow)

#**********************************************************

#tu jest bardzo toporne, ale zostaje id wiêc mo¿na u¿ywaæ dalej

rodzice_praca_studia <- merge(x = ilu_rodzicow_studiowalo, y = ilu_rodzicow_pracuje, by = "id_ucznia") %>%
  mutate(grupa = ifelse(studia_rodzicow == "0S" & praca_rodzicow == "0P", "G00",
                        ifelse(studia_rodzicow=="0S" & praca_rodzicow=="1P","G01",
                               ifelse(studia_rodzicow=="0S" & praca_rodzicow=="2P","G02",
                                      ifelse(studia_rodzicow=="1S" & praca_rodzicow=="0P","G10",
                                             ifelse(studia_rodzicow=="1S" & praca_rodzicow=="1P","G11",
                                                    ifelse(studia_rodzicow=="1S" & praca_rodzicow=="2P","G12",
                                                           ifelse(studia_rodzicow=="2S" & praca_rodzicow=="0P","G20",
                                                                  ifelse(studia_rodzicow=="2S" & praca_rodzicow=="1P","G21",
                                                                         ifelse(studia_rodzicow=="2S" & praca_rodzicow=="2P","G22","inne")))))))))) %>% 
  
group_by(grupa) %>% 
mutate(n = n()) %>% 
filter(n>50)

#**********************************************************

#teraz bêdzie ocena przysz³oœci, poŸniej poka¿emy zale¿noœæ miêdzy sytuacj¹ rodziców, a spojrzeniem na przysz³oœæ
# 1 negatywnie, 2 pozytywnie, zmieniamy, tylko dla celów lepszej wizualizacji

spojrzenie_na_przyszlosc <- df1 %>%
  select(id_ucznia, D15, D16, D17, D18, D19, D20, D21, D22) %>%
  filter(rowSums(.[2:9]) < 150) %>%
  mutate(Kontrolna = ifelse(rowSums(.[2:9]) > 20, 1 , 0)) %>%
  mutate(across(D17:D22, ~ifelse(.x==1 , 2 , ifelse(.x==2 , 1 , .x) ) ) ) %>%
  mutate(across(D15:D22, ~ifelse(.x>3, 0 , .x))) %>%
  mutate(srednia = 100*(ifelse(Kontrolna == 1, rowSums(.[2:9])/7 , rowSums(.[2:9])/8 )-1))

#**********************************************************
 
# finalnie1 <- merge(x=rodzice_praca_studia, y = spojrzenie_na_przyszlosc, by = "id_ucznia") %>%
#   filter(grupa != "G00")
# 
# ggplot(finalnie1, aes(x = `srednia`, y = `grupa`, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0) +
#   scale_fill_viridis(option = "I") +
#   labs(title = 'Spojrzenie na przysz³oœæ') +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.8, "lines"),
#     strip.text.x = element_text(size = 8)
#  )

#**********************************************************

oceny <- df1 %>%
  select(klasa, id_ucznia, P10_M, P10_JP, P10P, P10B, P10C, P10F) %>%
  filter(klasa < 10) %>%
  mutate(across(P10_M:P10F, ~ifelse(.x>10, 100 , .x))) %>%
  mutate(kont = floor(  rowSums(.[3:8])/100 ) ) %>%
  filter(kont < 4) %>%
  mutate(across(P10_M:P10F, ~ifelse(.x>10, 0 , .x))) %>%
  mutate(srednia_ocen =round(rowSums(.[3:8])/(6-kont),2 ) )


finalnie2 <- merge(x=rodzice_praca_studia, y = oceny, by = "id_ucznia")

# ggplot(finalnie2, aes(x=grupa, y=srednia_ocen, fill=grupa)) +
#   geom_violin() +
#   scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
#   theme_bw()+
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   facet_wrap(~klasa)+
#   ggtitle("Violin chart of mean of grades") +
#   xlab("")



# ggsave(filename = "Wave.png",ggplot(finalnie1, aes(x = `srednia`, y = `grupa`, fill = ..x..)) +
#          geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#          scale_fill_viridis(option = "I") +
#          labs(title = 'Spojrzenie na przysz³oœæ') +
#          theme_ipsum() +
#          theme(
#            legend.position="none",
#            panel.spacing = unit(0.1, "lines"),
#            strip.text.x = element_text(size = 8)
#          )
#          ,
#        width = 10, height = 8, dpi = 450, units = "in", device='png')






