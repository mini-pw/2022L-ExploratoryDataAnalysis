library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggalluvial)

df <- read_sav("roses.sav")
indeksy <- c(23:36, 38:57, 62, 63, 66:85, 87, 88, 90, 92)
df2 <- df %>% 
  mutate(average = round(rowMeans(df[indeksy], na.rm=TRUE))) %>% 
  select(average, Q2, Q99___Miasto_Wie_) %>% 
  mutate(Q2 = case_when(Q2 == 1 ~ "Dziewczynka", TRUE ~ "Chłopiec"),
         average = case_when(average == 1 ~ "Nie", 
                            average == 2 ~ "Raczej nie",
                            average == 3 ~ "Raczej tak", 
                            TRUE ~ "Tak"),
         Q99___Miasto_Wie_ = case_when(Q99___Miasto_Wie_ == 1~ "Wieś", TRUE~"Miasto")) %>% 
  na.omit() %>% 
  group_by(average, Q2, Q99___Miasto_Wie_) %>% 
  summarise(n =n())

ggplot(df2, aes(y = n,
                axis1 = average,
                axis2 = Q2,
                axis3 = Q99___Miasto_Wie_ ),
       reverse = FALSE)+
  geom_alluvium(aes(fill = as.factor(average)), 
                reverse = FALSE)+
  geom_stratum(reverse = FALSE)+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3, discern = TRUE, reverse = FALSE)+
  scale_fill_manual(values = c("#ffff09",
    "#83e262",
    "#18b789",
    "#268787"))+
  labs(y = "Czy interesujesz się przedmiotami ścisłymi?")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )
