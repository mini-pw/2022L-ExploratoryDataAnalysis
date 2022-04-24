library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(reshape2)

dataset=read_sav(file.choose())

## Prezentowanie zaufania do nauki
science_ds <- dataset %>% 
  filter(Progress == 100) %>% 
  select(c(19,60,61,68:71,77:82,90,92:97,123,131,146:153,155,157)) %>% 
  mutate(P³eæ = ifelse(Q2==1, 'Dziewczynka', 'Ch³opiec')) %>% 
  relocate(where(is.numeric), .after = where(is.character)) %>% 
  select(-2) %>% 
  na.omit(Q11_12) %>%
  t() %>% 
  t() %>% 
  as.data.frame() %>%
  group_by(P³eæ, Q11_12) %>%
  summarise(Count = n()) %>% 
  ungroup()

# Wykres przedstawiajacy zaufanie m³odzie¿y w zale¿noœci od p³ci

colnames(science_ds) <- c('P³eæ', 'Zaufanie', 'Zliczone' )
science_ds 
ggplot(science_ds, aes(x = Zaufanie, y = Zliczone, fill = P³eæ)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = '\n Zaufanie do naukowców', y = 'Iloœæ osób', fill = 'P³eæ',
       title = 'Zaufanie m³odzie¿y do naukowców w zale¿noœci od p³ci')+
  scale_x_discrete(labels = c('Brak', 'Niewielkie', 'Œrednie', 'Wysokie'))

str(science_ds)

science_ds2 <- dataset %>% 
  filter(Progress == 100) %>% 
  select(c(19,60,61,68:71,77:82,90,92:97,123,131,146:153,155,157)) %>% 
  select(-1) %>% 
  na.omit(Q11_12) %>%
  t() %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(across(Q7_5:Q11_12, as.numeric)) %>% 
  group_by(Q11_12) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE) )) %>% 
  ungroup()

science_ds2 %>% 
  select(-1) %>% 
  t() %>% 
  as.data.frame() -> science_ds2

colnames(science_ds2) <- c('Brak', 'Niewielkie', 'Œrednie', 'Wysokie')

row.names(science_ds2) <- NULL

science_ds2$Pytanie = c('Q7_5','Q7_6','Q9_2','Q9_3','Q9_4','Q9_5','Q9_11','Q9_12','Q9_13','Q9_14','Q9_15',
                         'Q9_16','Q9_24','Q9_26','Q9_27','Q9_28','Q9_29','Q9_30','Q9_31','Q8_3','Q8_11',
                         'Q11_1','Q11_2','Q11_3','Q11_4','Q11_5','Q11_6','Q11_7','Q11_8','Q11_10')


science_ds2 %>% 
  sort()


science_ds2 %>% 
  tidyr::gather("id", "value", 1:4) %>% 
  ggplot(., aes(factor(Pytanie, level =c('Q7_5','Q7_6','Q9_12','Q9_14','Q9_24','Q9_2','Q9_3','Q9_4','Q9_5',
                                         'Q9_11','Q9_15','Q9_16','Q8_3','Q8_11','Q9_13','Q11_2','Q9_26',
                                         'Q9_27','Q9_28','Q9_29','Q9_30','Q9_31','Q11_1','Q11_3','Q11_4',
                                         'Q11_5','Q11_6','Q11_7','Q11_8','Q11_10')), value, col = id))+
  geom_point()+
  stat_smooth()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Pytania', y = 'Œrednia odpowiedzi', col = 'Zaufanie do naukowców',
       title = 'Odpowiedzi na pytania w zale¿noœci od poziomu zaufania')

str(science_ds2)
