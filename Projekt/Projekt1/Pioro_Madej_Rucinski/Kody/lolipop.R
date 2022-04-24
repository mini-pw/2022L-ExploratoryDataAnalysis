library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)
library(hrbrthemes)

my_breaks <- function(x, n = 4) {return(x[c(TRUE, rep(FALSE, n - 1))])}


loli <- data_mlodsi %>% 
  left_join(data_starsi, by = 'columns') %>% 
  mutate(ile_procent_mlodsi = round(ile_99.x /1598 * 100,1), ile_procent_starsi = round(ile_99.y /1598 * 100,1)) %>% 
  filter(!columns %in% max_braki$columns & columns != 'X207')
  

ggplot(loli,aes(x = forcats::fct_inorder(factor(columns)), xend = forcats::fct_inorder(factor(columns)),
                y =ile_procent_mlodsi, yend = ile_procent_starsi )) +
  geom_segment() +
  geom_point(aes(x = forcats::fct_inorder(factor(columns)), y = ile_procent_mlodsi, color = 'młodsi (2007-2008)')) +
  geom_point(aes(x = forcats::fct_inorder(factor(columns)), y = ile_procent_starsi, color = 'starsi (2002-2006)')) +
  coord_flip() +
  labs(x = "Pytanie", y = "procent braków", color = "Legend") +
  scale_color_manual(values = c("orange","orchid3"),
                     guide = guide_legend(),
                     name = "Wiek") +
  theme(legend.position = c(0.75,0.25)) +
  scale_x_discrete(breaks = my_breaks) +
  scale_y_continuous(expand = c(0,0), limits = (c(- 0.05, 12)))
