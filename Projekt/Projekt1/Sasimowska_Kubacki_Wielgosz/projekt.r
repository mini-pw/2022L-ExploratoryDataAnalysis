library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(readxl)
library(forcats)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(patchwork)
library(ggthemes)


df <- read_excel("Excel_projekt.xlsx")

#emocje podzia³ stopieñ odbioru pozytywnego, negatywnego wystawy CNK w zale¿noœci od stopnia zainteresowania nauk¹ (science) ucznia

both2 <- df %>% 
  select(lp_koder,`P 23`:`P 37`, `P 42`:`P 44`, W1_1:W1_10) %>% 
  mutate(srednia_p=(rowSums(.[2:19]))/18) %>% 
  filter(srednia_p <= 5) %>% 
  select(srednia_p, W1_1:W1_10) %>% 
  filter(rowSums(.[2:11])/10<=3) %>%
  mutate(pozytywny = ((W1_1+W1_3+W1_5+W1_7+W1_9)/10 -0.5)*100) %>%
  mutate(negatywny = ((W1_2+W1_4+W1_6+W1_8+W1_10)/10 -0.5)*(-100)) %>%
  select(pozytywny,negatywny, srednia_p) %>% 
  mutate(grupa = ifelse(srednia_p < 0.5, "0-0.5",
                        ifelse(srednia_p<1,"0.5-1",
                               ifelse(srednia_p<1.5, '1-1.5',
                                      ifelse(srednia_p<2, '1.5-2',
                                             ifelse(srednia_p<2.5, '2-2.5',
                                                    ifelse(srednia_p<3, '2.5-3',
                                                           ifelse(srednia_p<3.5, '3-3.5',
                                                                  ifelse(srednia_p<4, '3.5-4',
                                                                         ifelse(srednia_p< 4.5, '4-4.5', '4.5-5')))))))))) %>% 
  group_by(grupa)%>% 
  summarise(positive = signif(mean(pozytywny), 3), negative = signif(mean(negatywny),3)) %>% 
  pivot_longer(!grupa, names_to = 'Emotions', values_to = 'srednia')

#pivot_longer(!both2, names_to = "income", values_to = "count")
#View(both2)

# ggplot(both2, aes( x = grupa, y = srednia, fill = Emotions))+
#   geom_col()+
#   scale_fill_manual(values = c('#f4a582', '#b8e186'))+
#   theme_gdocs()+
#   theme(legend.position = 'middle', text = element_text(size = 18))+
#   scale_y_continuous(limits = c(-100,100))+
#   labs(title = 'Children emotions during exibition', subtitle = 'According to their interest in science',
#        y ='The frequency of feeling emotions', x ='Interest in science')+
#   geom_text(aes(label = srednia), nudge_y = 0)

ggsave(filename = "Pozytywy222.png",ggplot(both2, aes( x = grupa, y = srednia, fill = Emotions))+
         geom_col()+
         scale_fill_manual(values = c('#f4a582', '#b8e186'))+
         theme_gdocs()+
         theme(legend.position = 'middle', text = element_text(size = 18))+
         scale_y_continuous(limits = c(-100,100))+
         labs(title = 'Children emotions during exibition', subtitle = 'According to their interest in science',
              y ='The frequency of feeling emotions', x ='Interest in science')+
         geom_text(aes(label = srednia), nudge_y = 0)
       ,
       width = 10, height = 8, dpi = 450, units = "in", device='png')

#rozk³ad zainteresowania nauk¹(science) w zale¿noœci od p³ci

girl <- df %>% 
  select(lp_koder, P3, `P 23`:`P 37`, `P 42`:`P 44`) %>% 
  filter(P3 == 1) %>% 
  filter(`P 23`+`P 24`+`P 25`+`P 26`+`P 27`+`P 28`+`P 29`+`P 30`+`P 31`+`P 32`+`P 33`+`P 34`+`P 35`+`P 36`+`P 37`+`P 42`+`P 43`+ `P 44`<=90) %>% 
  mutate(srednia=(`P 23`+`P 24`+`P 25`+`P 26`+`P 27`+`P 28`+`P 29`+`P 30`+`P 31`+`P 32`+`P 33`+`P 34`+`P 35`+`P 36`+`P 37`+`P 42`+`P 43`+ `P 44`)/18) %>% 
  select(lp_koder,srednia) %>% 
  count(srednia)


boy <- df %>% 
  select(lp_koder, P3, `P 23`:`P 37`, `P 42`:`P 44`) %>% 
  filter(P3 == 2) %>% 
  filter(`P 23`+`P 24`+`P 25`+`P 26`+`P 27`+`P 28`+`P 29`+`P 30`+`P 31`+`P 32`+`P 33`+`P 34`+`P 35`+`P 36`+`P 37`+`P 42`+`P 43`+ `P 44`<=90) %>% 
  mutate(srednia=(`P 23`+`P 24`+`P 25`+`P 26`+`P 27`+`P 28`+`P 29`+`P 30`+`P 31`+`P 32`+`P 33`+`P 34`+`P 35`+`P 36`+`P 37`+`P 42`+`P 43`+ `P 44`)/18) %>% 
  select(lp_koder,srednia) %>% 
  count(srednia)


# p_top <- ggplot(girl, aes(x=srednia, y =n))+
#   geom_col(fill='#f1b6da')+
#   scale_y_continuous(limits = c(0,40))+
#   coord_flip()+
#   labs(title='Girls', x ='', y = '')+
#   theme_gdocs()+
#   theme(plot.title=element_text(size=18))
# 
# p_bottom <- ggplot(boy, aes(x=srednia, y =n))+
#   geom_col(fill = '#92c5de')+
#   scale_y_continuous(limits = c(0,40))+
#   scale_y_reverse()+
#   coord_flip()+
#   labs(title='Boys', x = '', y = '')+
#   theme_gdocs()+
#   theme(plot.title=element_text(size=18))
# 
# (p_bottom+p_top) & plot_annotation(title = 'Interest in science', subtitle = 'primary school, grades 6-8')&theme_gdocs()

