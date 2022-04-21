library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(cowplot)

roses <- read.csv("roses_1.csv")

roses <- roses[, c(20, 24:98)]

# Tajemnica

roses_mys <- roses[, c("Q2", "Q7_3", "Q7_4", "Q7_5", "Q7_6", "Q7_9", "Q9_31")]

roses_mys_1 <- roses_mys %>% 
  filter(Q2 == 1) %>% 
  dplyr::select(-Q2)

roses_mys_1$mean <- rowMeans(roses_mys_1, na.rm = TRUE)

roses_mys_1 <- roses_mys_1 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_mys_1$intrest <- rep("mys", length(roses_mys_1$mean))

roses_mys_2 <- roses_mys %>% 
  filter(Q2 == 2) %>% 
  dplyr::select(-Q2)

roses_mys_2$mean <- rowMeans(roses_mys_2, na.rm = TRUE)

roses_mys_2 <- roses_mys_2 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_mys_2$intrest <- rep("mys", length(roses_mys_2$mean))

#Ochrona œrodowiska

roses_env <- roses[, c("Q2", "Q21_1", "Q7_10", "Q7_11", "Q9_2", "Q9_3",
                       "Q9_4", "Q9_11", "Q9_14", "Q9_15", "Q9_16")]

roses_env_1 <- roses_env %>% 
  filter(Q2 == 1) %>% 
  dplyr::select(-Q2)

roses_env_1$mean <- rowMeans(roses_env_1, na.rm = TRUE)

roses_env_1 <- roses_env_1 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_env_1$intrest <- rep("env", length(roses_env_1$mean))

roses_env_2 <- roses_env %>% 
  filter(Q2 == 2) %>% 
  dplyr::select(-Q2)

roses_env_2$mean <- rowMeans(roses_env_2, na.rm = TRUE)

roses_env_2 <- roses_env_2 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_env_2$intrest <- rep("env", length(roses_env_2$mean))

# Kosmos

roses_cos <- roses[, c("Q2", "Q5_12", "Q5_13", "Q21_8", "Q21_16",
                       "Q7_2", "Q7_7", "Q9_22")]

roses_cos_1 <- roses_cos %>% 
  filter(Q2 == 1) %>% 
  dplyr::select(-Q2)

roses_cos_1$mean <- rowMeans(roses_cos_1, na.rm = TRUE)

roses_cos_1 <- roses_cos_1 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_cos_1$intrest <- rep("cos", length(roses_cos_1$mean))

roses_cos_2 <- roses_cos %>% 
  filter(Q2 == 2) %>% 
  dplyr::select(-Q2)

roses_cos_2$mean <- rowMeans(roses_cos_2, na.rm = TRUE)

roses_cos_2 <- roses_cos_2 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_cos_2$intrest <- rep("cos", length(roses_cos_2$mean))

# Chemia/Fizyka

roses_chem <- roses[, c("Q2", "Q5_1", "Q5_8", "Q21_5", "Q21_6",
                        "Q21_17", "Q21_18")]

roses_chem_1 <- roses_chem %>% 
  filter(Q2 == 1) %>% 
  dplyr::select(-Q2)

roses_chem_1$mean <- rowMeans(roses_chem_1, na.rm = TRUE)

roses_chem_1 <- roses_chem_1 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_chem_1$intrest <- rep("chem", length(roses_chem_1$mean))

roses_chem_2 <- roses_chem %>% 
  filter(Q2 == 2) %>% 
  dplyr::select(-Q2)

roses_chem_2$mean <- rowMeans(roses_chem_2, na.rm = TRUE)

roses_chem_2 <- roses_chem_2 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_chem_2$intrest <- rep("chem", length(roses_chem_2$mean))

# Zwierzêta/Roœliny

roses_ani <- roses[, c("Q2", "Q5_9", "Q21_3", "Q21_4", "Q9_11",
                       "Q9_13", "Q9_19", "Q9_20")]

roses_ani_1 <- roses_ani %>% 
  filter(Q2 == 1) %>% 
  dplyr::select(-Q2)

roses_ani_1$mean <- rowMeans(roses_ani_1, na.rm = TRUE)

roses_ani_1 <- roses_ani_1 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_ani_1$intrest <- rep("ani", length(roses_ani_1$mean))

roses_ani_2 <- roses_ani %>% 
  filter(Q2 == 2) %>% 
  dplyr::select(-Q2)

roses_ani_2$mean <- rowMeans(roses_ani_2, na.rm = TRUE)

roses_ani_2 <- roses_ani_2 %>% 
  filter(!is.nan(mean)) %>% 
  dplyr::select(mean)

roses_ani_2$intrest <- rep("ani", length(roses_ani_2$mean))


roses_1 <- rbind(roses_mys_1, roses_env_1, roses_cos_1, roses_chem_1, roses_ani_1)
roses_1$gender <- rep("dziewczynki", length(roses_1$mean))
roses_2 <- rbind(roses_mys_2, roses_env_2, roses_cos_2, roses_chem_2, roses_ani_2)
roses_2$gender <- rep("ch³opcy", length(roses_2$mean))
roses <- rbind(roses_1, roses_2)

ggplot(roses, aes(y = intrest, x = mean, fill = gender)) +
  geom_boxplot()  +
  scale_fill_manual(values = c("#86cbed", "#f48ba9")) +
  scale_y_discrete(labels = c("Roœliny i zwierzêta", "Chemia i fizyka", "Kosmos i astronomia", "Œrodowisko i jego ochrona", "Tajemnica i mistycyzm")) +
  labs(fill = "P³eæ",
       x = "Œrednia",
       y = "Zainteresowania",
       title = "Rozk³ad œredniego zainteresowania danymi tematami")