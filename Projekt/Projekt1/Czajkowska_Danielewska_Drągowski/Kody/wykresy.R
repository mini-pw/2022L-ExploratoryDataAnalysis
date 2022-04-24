library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggradar)
ROSES <- read_sav("C:/Users/idani/OneDrive/Pulpit/Projekt Roses/ROSES/ROSES/ROSES master quest PL_November 9, 2021_07.50.sav")

# wykres slupkowy
DANE <- ROSES %>%
  group_by(Q2)%>% 
  summarise(Chemia = mean(c(Q5_1,Q5_8,Q21_6,Q9_21),na.rm=TRUE)-1,
            Geografia = mean(c(Q5_2, Q5_3, Q5_4, Q5_14, Q21_2, Q7_11,  Q9_2, Q9_3, Q9_4, Q9_12, Q9_15, Q9_16, Q9_24),na.rm=TRUE)-1,
            Biologia = mean(c(Q5_5,Q5_6,Q5_7, Q5_9, Q21_1, Q21_3, Q21_4, Q21_10,Q21_11,Q21_12,Q21_13, Q21_14, Q21_17, Q9_5,Q9_6,Q9_7,Q9_8,Q9_9, Q9_11, Q9_13, Q9_14, Q9_17,Q9_18,Q9_19, Q9_26),na.rm=TRUE)-1,
            Fizyka = mean(c(Q5_8, Q5_10,Q5_11,Q5_12,Q5_13, Q21_5, Q21_8, Q21_9, Q21_15,Q21_16,Q21_17,Q21_18, Q7_1, Q7_2, Q7_7, Q7_8, Q9_1, Q9_10, Q9_22),na.rm=TRUE)-1) 

pivot_longer(DANE, cols = -Q2, names_to = "mean", values_to = "value")%>% 
  filter(is.finite(value)) %>%
  ggplot(aes(x = Q2, y = value, fill = as.factor(Q2))) +
  geom_bar(stat="identity")+
  facet_grid(cols = vars(mean)) +
  labs(fill="Płeć")+  
  theme_bw() +
  theme(line = element_line(size = 0.2),
        legend.position = "bottom") +
  xlab(label = NULL) +
  ylab("Zainteresowanie danym przedmiotem")+
  scale_x_discrete(NULL)+
  scale_fill_manual(values = c("#FFCCDD","#87CEFA"),labels = c("Dziewczynka","Chłopiec"))+
  scale_y_continuous(limits = c(0,1.75), expand = c(0,0))

# wykres radarowy
this <- ROSES %>% 
  rowwise() %>% 
  mutate(Nauka_pozytywne = mean(c(Q5_1,Q5_8,Q21_6,Q9_21,Q5_2, Q5_3, Q5_4, Q5_14,
                                  Q21_2, Q7_11,  Q9_2, Q9_3, Q9_4, Q9_12, Q9_15, 
                                  Q9_16, Q9_24, Q5_5,Q5_6,Q5_7, Q5_9, Q21_1, 
                                  Q21_3, Q21_4, Q21_10,Q21_11,Q21_12,Q21_13, 
                                  Q21_14, Q21_17, Q9_5,Q9_6,Q9_7,Q9_8,Q9_9, 
                                  Q9_11, Q9_13, Q9_14, Q9_17,Q9_18,Q9_19, Q9_26,
                                  Q5_8, Q5_10,Q5_11,Q5_12,Q5_13, Q21_5, Q21_8, 
                                  Q21_9, Q21_15,Q21_16,Q21_17,Q21_18, Q7_1, Q7_2, 
                                  Q7_7, Q7_8, Q9_1, Q9_10, Q9_22, Q7_3,Q7_4,Q7_5,
                                  Q7_6, Q7_9, Q7_10, Q9_23, Q9_25), na.rm=TRUE)) %>% 
  filter(Nauka_pozytywne > 2.5) %>% 
  group_by(Q99___Miasto_Wie_) %>% 
  summarise(Zoo_oceanarium = mean(Q15_1, na.rm = TRUE),
            Planetarium = mean(Q15_2,na.rm = TRUE),
            Centrum_nauki = mean(Q15_3,na.rm = TRUE),
            Muzeum = mean(Q15_4,na.rm =TRUE),
            Park_ogrod_rezerwat = mean(Q15_5,na.rm =TRUE),
            Wydarzenia_nauk = mean(Q15_6,na.rm =TRUE),
            Pozaszkolne_kola = mean(Q15_7, na.rm =TRUE),
            Strony = mean(Q15_8, na.rm =TRUE),
            Media_spol_cyfr = mean(c(Q15_9,Q15_14), na.rm =TRUE),
            Czasopisma_nauk = mean(Q15_10, na.rm =TRUE),
            Gry = mean(Q15_11, na.rm =TRUE),
            Telewizja = mean(Q15_12, na.rm =TRUE),
            Wideo = mean(Q15_13,na.rm = TRUE)) %>%
  head(2)
this[1] <- c("Wieś", "Miasto")
ggradar(this, values.radar = c("Raczej nie","Raczej tak","Tak"),grid.min = 2,grid.max = 3.5,grid.mid = 2.75,
        group.line.width = 1, 
        group.point.size = 3,
        group.colours = c("#00AFBB", "#E7B800"),
        centre.y = 0,
        axis.labels = c("Odwiedzam zoo,\noceanarium","Odwiedzam\nplanetarium","Odzwiedzam\ncentrum nauki",
                        "Odwiedzam\nmuzeum","Odwiedzam park,\nrezerwat przyrody",
                        "Odwiedzam\nwydarzenia naukowe","Biorę udział w pozaszkolnych\nkołach naukowych",
                        "Przeglądam\nstrony internetowe","Przeglądam\nmedia społecznościowe","Czytam\nczasopisma\nnaukowe",
                        "Gram w gry\nkomputerowe","Oglądam telewizję","Oglądam\nklipy wideo"),
        legend.position = c(0.9,0.9)) +
  theme(legend.background = element_rect(colour = "lightgray")) +
  labs(title= "Uczę się gdy:")

# wykres typu smooth
DANE1 <- ROSES %>%
  filter(Q2 == 1) %>% # 
  rowwise() %>% 
  transmute(Chemia = mean(c(Q5_1,Q5_8,Q21_6,Q9_21),na.rm=TRUE),
            Geografia = mean(c(Q5_2, Q5_3, Q5_4, Q5_14, Q21_2, Q7_11,  Q9_2, Q9_3, Q9_4, Q9_12, Q9_15, Q9_16, Q9_24),na.rm=TRUE),
            Biologia = mean(c(Q5_5,Q5_6,Q5_7, Q5_9, Q21_1, Q21_3, Q21_4, Q21_10,Q21_11,Q21_12,Q21_13, Q21_14, Q21_17, Q9_5,Q9_6,Q9_7,Q9_8,Q9_9, Q9_11, Q9_13, Q9_14, Q9_17,Q9_18,Q9_19, Q9_26),na.rm=TRUE),
            Fizyka = mean(c(Q5_8, Q5_10,Q5_11,Q5_12,Q5_13, Q21_5, Q21_8, Q21_9, Q21_15,Q21_16,Q21_17,Q21_18, Q7_1, Q7_2, Q7_7, Q7_8, Q9_1, Q9_10, Q9_22),na.rm=TRUE),
            Nauka_pozytywne = mean(c(Q10_2,Q10_3,Q10_4,Q10_5,Q10_7,Q10_8,Q10_9),na.rm = TRUE))

DANE2 <- ROSES %>%
  filter(Q2 == 2) %>% # 
  rowwise() %>% 
  transmute(Chemia = mean(c(Q5_1,Q5_8,Q21_6,Q9_21),na.rm=TRUE),
            Geografia = mean(c(Q5_2, Q5_3, Q5_4, Q5_14, Q21_2, Q7_11,  Q9_2, Q9_3, Q9_4, Q9_12, Q9_15, Q9_16, Q9_24),na.rm=TRUE),
            Biologia = mean(c(Q5_5,Q5_6,Q5_7, Q5_9, Q21_1, Q21_3, Q21_4, Q21_10,Q21_11,Q21_12,Q21_13, Q21_14, Q21_17, Q9_5,Q9_6,Q9_7,Q9_8,Q9_9, Q9_11, Q9_13, Q9_14, Q9_17,Q9_18,Q9_19, Q9_26),na.rm=TRUE),
            Fizyka = mean(c(Q5_8, Q5_10,Q5_11,Q5_12,Q5_13, Q21_5, Q21_8, Q21_9, Q21_15,Q21_16,Q21_17,Q21_18, Q7_1, Q7_2, Q7_7, Q7_8, Q9_1, Q9_10, Q9_22),na.rm=TRUE),
            Nauka_pozytywne = mean(c(Q10_2,Q10_3,Q10_4,Q10_5,Q10_7,Q10_8,Q10_9),na.rm = TRUE))

nast1 <- pivot_longer(DANE1,cols = 1:4, names_to = "mean",values_to = "value")
nast2 <- pivot_longer(DANE2,cols = 1:4, names_to = "mean",values_to = "value")

this <- nast2 %>% 
  mutate(Płeć = "Chłopiec") %>% 
  rbind(nast1 %>% mutate(Płeć = "Dziewczynka"))

this %>% 
  filter(is.finite(value) & is.finite(Nauka_pozytywne)) %>% 
  ggplot(aes(x = Nauka_pozytywne, y= value, color = Płeć, fill = Płeć))+
  geom_smooth(size = 0.7, alpha = 0.3)+
  facet_grid(cols=vars(mean))+
  theme_bw() +
  theme(line = element_line(size = 0.2),
        legend.position = "bottom") +
  xlab("Opinia o naukach ścisłych") +
  ylab("Zainteresowanie danym przedmiotem") +
  scale_fill_manual(values=c("#87CEFA","#DD9ECD"))+
  scale_color_manual(values=c("#87CEFA","#DD9ECD")) +
  scale_x_continuous(expand = c(0,0))

