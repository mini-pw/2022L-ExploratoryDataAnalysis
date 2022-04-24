library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(stringr)

kopernik <- kopernik %>% 
  distinct(id_ucznia, miejsc, .keep_all = TRUE)
colnames(kopernik) <- stri_replace_all_fixed(colnames(kopernik), "P ", "P")

kopernik <- kopernik %>% 
  select(!weryfikator:woj_nr) %>% 
  select(!P45:P45k)

data_99 <- data.frame(columns = colnames(kopernik), ile_99 = rep(0, dim(kopernik)[2]))

iter <- 1:dim(kopernik)[2]
for (i in iter){
  data_99[i,2] <- kopernik %>% 
    select(i) %>% 
    rename(col1 = 1) %>% 
    filter(col1 == 99) %>% 
    count()
}
subs <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>%
  mutate(ile_99 = 100*ile_99/dim(kopernik)[1]) %>%
  mutate("prev_value" = lag(ile_99)) %>%
  filter(ile_99-prev_value > 1.5) %>% 
  select(columns,ile_99)


data_99 <- data_99 %>% 
  mutate(etap = case_when(
    str_detect(columns,"W5_") ~ "4", 
    str_detect(columns,"W") ~ "3",
    str_detect(columns,"D") ~ "2",
    str_detect(columns,"P") ~ "1"
  ))



g <- data_99 %>% 
  filter(ile_99 <= quantile(ile_99,probs = 0.96), row_number() <= n()-1) %>% 
  filter(!str_detect(columns, "P45")) %>% 
  mutate(ile_99 = 100*ile_99/dim(kopernik)[1]) %>% 
  ggplot(aes(x = forcats::fct_inorder(factor(columns)), y = ile_99, fill = etap, color = etap)) +
  geom_col() +
  #scale_fill_manual(values = c("lightpink1", "hotpink3", "mediumorchid3","darkorchid4")) +
  #scale_color_manual(values = c("lightpink1", "hotpink3", "mediumorchid3","darkorchid4"))
  scale_fill_manual(values = c("lightpink1", "hotpink3", "mediumorchid3","darkorchid4")) +
  scale_color_manual(values = c("lightpink1", "hotpink3", "mediumorchid3","darkorchid4")) 
  

g + labs(title = "Procent braków danych w pytaniach w ankiecie Naukobus", x = 'Pytania', y = 'Procent braków danych(%)')+
theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = (c(0,20))) 
  


g1 <- ggplot(data_99, aes(x = "", y = ile_99, fill = etap)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() 
