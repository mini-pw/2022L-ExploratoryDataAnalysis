library(dplyr)
library(ggplot2)
library(stringi)
library(tidyr)
library(forcats)
library(ggthemes)
library(stringr)
library(haven)



roses <- read.csv("roses_1.csv")

tajemnica <- roses %>% 
  select(Q7_3, Q7_4, Q7_5, Q7_6, Q7_9, Q9_31)
srodowisko <- roses %>% 
  select(Q21_1, Q7_10, Q7_11, Q9_3, Q9_4, Q9_2, Q9_11, Q9_14, Q9_15, Q9_16)
kosmos <- roses %>% 
  select(Q5_12, Q5_13, Q21_8, Q21_16, Q7_2, Q7_7, Q9_22)
fizyka <- roses %>% 
  select(Q5_1, Q5_8, Q21_5, Q21_6, Q21_17, Q21_18)
zwierzatka <- roses %>% 
  select(Q5_9, Q21_3, Q21_4, Q9_11, Q9_13, Q9_19, Q9_20)

roses <- roses %>% 
  mutate(mys = rowMeans(tajemnica, na.rm = TRUE)) %>% 
  mutate(env = rowMeans(srodowisko, na.rm = TRUE)) %>% 
  mutate(cos = rowMeans(kosmos, na.rm = TRUE)) %>% 
  mutate(chem = rowMeans(fizyka, na.rm = TRUE)) %>% 
  mutate(ani = rowMeans(zwierzatka, na.rm = TRUE))

renesans <- roses %>% 
  select(mys, env, cos, chem, ani) %>% 
  mutate(taj = if_else(mys >= 3, 1, 0)) %>% 
  mutate(sro = if_else(env >= 3, 1, 0)) %>% 
  mutate(kos = if_else(cos >= 3, 1, 0)) %>% 
  mutate(fiz = if_else(chem >= 3, 1, 0)) %>% 
  mutate(zwi = if_else(ani >= 3, 1, 0)) %>% 
  mutate(licz_zaint = taj+sro+kos+fiz+zwi)

roses$licz_zaint = renesans$licz_zaint



cat <- roses %>% 
  select(kategoria, mys, env, cos, chem, ani, licz_zaint) %>% 
  mutate(kol1 = word(kategoria, 1)) %>% 
  mutate(kol2 = word(kategoria, 2)) %>% 
  mutate(kol3 = word(kategoria, 3)) %>% 
  mutate(kol4 = word(kategoria, 4)) %>% 
  mutate(kol5 = word(kategoria, 5)) %>% 
  select(!kategoria)
cat$kol1 <- gsub(",", "", cat$kol1)
cat$kol2 <- gsub(",", "", cat$kol2)
cat$kol3 <- gsub(",", "", cat$kol3)
cat$kol4 <- gsub(",", "", cat$kol4)
cat$kol5 <- gsub(",", "", cat$kol5)


for(i in 1:length(cat$kol1)){
  for(a in 1:4){
    for(b in (a+1):5){
      if(is.na(cat[i, 6+a]) == FALSE & is.na(cat[i, 6+b]) == FALSE){
        if(cat[i, 6+a] == cat[i, 6+b]){
          cat[i, 6+b] = NA
        }
      }
    }
  }
}
kotek <- cat
for(i in 1:length(kotek$kol1)){
  for(a in 2:5){
    if(is.na(kotek[i, 7]) == FALSE & is.na(kotek[i, 6+a]) == FALSE)
      kotek[i, 7] = paste(kotek[i, 7], kotek[i, 6+a], sep = ", ")
  }
}
roses$kategoria = kotek$kol1



cat1 <- cat %>% 
  select(mys, env, cos, chem, ani,licz_zaint, kol1) %>% 
  rename(kol = kol1) %>% 
  filter(is.na(kol) == FALSE)
cat2 <- cat %>% 
  select(mys, env, cos, chem, ani,licz_zaint, kol2) %>% 
  rename(kol = kol2) %>% 
  filter(is.na(kol) == FALSE)
cat3 <- cat %>% 
  select(mys, env, cos, chem, ani,licz_zaint, kol3) %>% 
  rename(kol = kol3) %>% 
  filter(is.na(kol) == FALSE)
cat4 <- cat %>% 
  select(mys, env, cos, chem, ani,licz_zaint, kol4) %>% 
  rename(kol = kol4) %>% 
  filter(is.na(kol) == FALSE)





cat <- full_join(cat1,cat2)
cat <- full_join(cat, cat3)
cat <- full_join(cat, cat4)




dog <- cat %>% 
  group_by(kol) %>% 
  summarise(mys = mean(mys, na.rm = TRUE), 
            env = mean(env, na.rm = TRUE), 
            cos = mean(cos, na.rm = TRUE), 
            chem = mean(chem, na.rm = TRUE), 
            ani = mean(ani, na.rm = TRUE),
            zaint = mean(licz_zaint, na.rm = TRUE))

pies <- cat %>% 
  summarise(mys1 = mean(mys, na.rm = TRUE), 
            env1 = mean(env, na.rm = TRUE), 
            cos1 = mean(cos, na.rm = TRUE), 
            chem1 = mean(chem, na.rm = TRUE), 
            ani1 = mean(ani, na.rm = TRUE),
            zaint1 = mean(licz_zaint, na.rm = TRUE))

dog <- merge(dog, pies)
dog <- dog %>% 
  mutate(mys = mys/mys1, 
         env = env/env1,
         cos = cos/cos1,
         chem = chem/chem1,
         ani = ani/ani1,
         zaint = zaint/zaint1,) %>% 
  select(kol, mys, env, cos, chem, ani, zaint)

dog1 <- dog %>% 
  select(kol, env) %>% 
  rename(val = env) %>% 
  mutate(prz = "env")
dog2 <- dog %>% 
  select(kol, mys) %>% 
  rename(val = mys) %>% 
  mutate(prz = "mys")
dog3 <- dog %>% 
  select(kol, cos) %>% 
  rename(val = cos) %>% 
  mutate(prz = "cos")
dog4 <- dog %>% 
  select(kol, chem) %>% 
  rename(val = chem) %>% 
  mutate(prz = "chem")
dog5 <- dog %>% 
  select(kol, ani) %>% 
  rename(val = ani) %>% 
  mutate(prz = "ani")
dog6 <- dog %>% 
  select(kol, zaint) %>% 
  rename(val = zaint) %>% 
  mutate(prz = "zaint")

dog <- full_join(dog1, dog2)
dog <- full_join(dog, dog3)
dog <- full_join(dog, dog4)
dog <- full_join(dog, dog5)
dog <- full_join(dog, dog6)







ggplot(dog, aes(x = val, y = kol, fill = prz)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_vline(xintercept = 1)+
  scale_fill_manual(values = c("#ffc7b8", "#5681b9", "#ff5c6f", "#93c4d2", "#ff00d0" ,"#00429d"))




