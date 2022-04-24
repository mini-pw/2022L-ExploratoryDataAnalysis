library(dplyr)
#install.packages('haven')
library(haven)
library(ggplot2)

dataset=read_sav(file.choose())



dataset %>%  select(c(9,38,39,66,68:69,77,80:82)) -> tab2

tab2 = na.omit(tab2)

tab2 %>% 
  t() %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() -> tab2

tab2 %>%
  mutate_at(c('Q21_1','Q21_2','Q7_11', 'Q9_2', 'Q9_3', 'Q9_11','Q9_14', 'Q9_15', 'Q9_16'), as.numeric) %>% 
  rowwise() %>%
  transmute(ResponseId, Grupy= mean(c_across(Q21_1:Q9_16))) %>% 
  transmute(ResponseId,Grupy, Grupy=cut(Grupy,c(1,2,3,4),
                                                  labels=c('Niezainteresowani',
                                                           'Obojêtni',
                                                           'Zainteresowani'),
                                                  include.lowest= TRUE))-> answers_tab


dataset %>%  select(c(9,157)) -> tab3 


tab3 %>% 
  t() %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() -> tab3
tab3 = na.omit(tab3)


tab3 %>%  mutate_at(c('Q11_12'), as.numeric) %>%
  inner_join(answers_tab, by = 'ResponseId') -> tab4




  tab4 %>% group_by(Grupy) %>% 
  count(Q11_12) %>% rename(Obserwacje = n, Nastawienie = Q11_12 ) %>% 
  ggplot(aes(x=Nastawienie, y=Obserwacje)) +
  geom_col()+facet_grid(~Grupy) +
  labs(title = "Zaufanie do naukowców",
       x= 'Poziom zaufania',y='Liczba osób', fill=NULL,
       subtitle = "Dla grup osób o ró¿nym stopniu zainteresowania problemami œrodowiskowymi")+
       theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 310)) +
  theme(strip.background = element_rect( fill = "white"))

  