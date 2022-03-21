library(dplyr)
df <- read.csv("house_data.csv")


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
df %>% 
  mutate(mediancond = median(condition)) %>% 
  filter(condition >= mediancond & waterfront == 1) %>%
  summarise(mean(price))
  

# Odp:1662564


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

df %>% 
  filter(floors == 2 | floors == 3) %>% 
  group_by(floors) %>% 
  summarise(median(bathrooms))

# Odp:Nieruchomosci o 2 pietrach nie maja wiekszej liczby lazienek niz nieruchomosci o 3 pietrach (jest taka sama).


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

df %>% 
  mutate(midlat = (max(lat)+min(lat))/2, midlong = ((max(long)+min(long))/2)) %>% 
  mutate(pos = case_when(lat > midlat & long<midlong ~ 'NW',
                         lat < midlat & long>midlong ~ 'SE',
                         TRUE ~ 'Other')) %>% 
  filter(pos == 'NW' | pos == 'SE') %>% 
  group_by(pos) %>% 
  summarise(n())
  
15677/85*100-100

# Odp:18343.53%


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?

df %>% 
  filter(yr_built>=1990 & yr_built<=2000) %>% 
  group_by(yr_built) %>% 
  summarise(median(bathrooms))
         

# Odp:Mediana liczby lazienek nie zmieniala sie.


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

df %>% 
  mutate(midlat = (max(lat)+min(lat))/2) %>% 
  mutate(is_north = case_when(lat>midlat ~'N',
                               TRUE ~ 'S')) %>% 
  filter(is_north == 'N') %>% 
  group_by(waterfront) %>% 
  summarise(q025 = quantile(grade,0.25), q075 = quantile(grade,0.75))

# Odp:Z widokiem na wode: Q0.25 = 8, Q0.75 = 10, bez widoku na wode: Q0.25 = 7, Q0.75 = 8.


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(price,n = n()) %>% 
  filter(n==602) %>% 
  summarise(quantile(price,0.75)-quantile(price,0.25))

# Odp:Pod kodem pocztowym 98103, rozstep miedzykwartylowy cen polozonych tam nieruchomosci wynosi 262875.


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

df %>% 
  filter(sqft_living < sqft_living15) %>% 
  summarise(n = n()/nrow(df)*100)

# Odp:42.59473% nieruchomosci ma wyzsza srednia powierzchnie 15 najblizszych sasiadow wzgledem swojej powierzchni.


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

df %>% 
  filter(price>quantile(price,0.75) & yr_renovated>=2012 & yr_built>1970)

# Odp:3 z tych nieruchomosci maja 3 pokoje, 1 ma 4 pokoje i 1 ma 5 pokoi.

# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

df %>% 
  filter(sqft_living < (quantile(sqft_living,0.25)- 1.5*(quantile(sqft_living,0.75)-quantile(sqft_living,0.25))) | sqft_living > (quantile(sqft_living,0.75)+1.5*(quantile(sqft_living,0.75)-quantile(sqft_living,0.25)))) %>% 
  summarise(n = n())
  
# Odp:Jest 572 wartosci odstajacych wzgledem powierzchni nieruchomosci.


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

df %>% 
  mutate(m2 = sqft_living/10.764) %>% 
  mutate(ppm2 = price/m2) %>% 
  summarise(max(ppm2))

# Odp:Najwieksza cena za metr kwadratowy to 8720.335