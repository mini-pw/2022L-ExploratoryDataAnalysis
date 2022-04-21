library(dplyr)
library(ggplot2)
library(stringi)
library(tidyr)
library(forcats)
library(ggthemes)

roses <- read.csv("roses_1.csv")

roses <- roses %>% 
  dplyr::select(Q2, hmm) %>%
  filter(!is.na(Q2) & !is.na(hmm) & hmm != "nie wiem")

ile <- stri_count_fixed(roses$hmm, ",") + 1

Q2 <- rep(roses$Q2, ile)
Q18_1 <- unlist(stri_split_fixed(roses$hmm, ", "))

roses <- data.frame(cbind(Q2, Q18_1))

roses <- roses %>% 
  group_by(Q18_1, Q2) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Q2, values_from = n, values_fill = 0) %>% 
  dplyr::rename(boys = 3, girls = 2, zawody = Q18_1) %>% 
  mutate(ratio = (boys/(boys + girls))*100, sum = boys + girls) %>%
  dplyr::select(zawody, sum, ratio) 

roses <- roses %>% 
  arrange(-sum) %>%
  filter(sum > 20)

inna_kolejnoœæ <- roses$zawody[22:1]

roses$zawody <- factor(roses$zawody, levels = inna_kolejnoœæ)


graf1 <- ggplot(roses, aes(x = sum, y = zawody, fill = ratio)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(
    low = "#f48ba9",
    mid = "#936ed4",
    high = "#86cbed",
    midpoint = 50
  ) +
  labs(fill = "% ch³opców",
       x = "Iloœæ",
       y = "Zawody",
       title = "Najczêœciej wybierane zawody")

graf1