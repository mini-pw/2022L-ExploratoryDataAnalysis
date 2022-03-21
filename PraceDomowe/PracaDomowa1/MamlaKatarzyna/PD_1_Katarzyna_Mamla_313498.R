library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
df %>% select(price,waterfront,grade) %>%
  filter(waterfront==1) %>%
  mutate(Grade_median = median(grade)) %>%
  filter(grade >= Grade_median) %>% 
  summarise(Price_mean = mean(price))

# Odp:  œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia to 2302236


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?
df %>% select(floors,bathrooms) %>%
  filter(floors == 2.0 | floors==3.0) %>% 
  group_by(floors) %>%
  summarise(median_bathrooms = median(bathrooms))

# Odp: Nie, nieruchomoœci o 2 i 3 pietrach maja taka sama liczbe lazienek w oparciu o wartosc mediany


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?
latitude <- df %>% 
  summarise(m = median(lat)) 


longitude <- df %>% 
  summarise(m = median(long))

df %>%  select(lat, long) %>% 
  filter((lat > latitude[1,1] & long < longitude[1,1]) |
           (lat < latitude[1,1] & long > longitude[1,1])  ) %>% 
  mutate(Directions = ifelse((lat > latitude[1,1] & long < longitude[1,1]),
                             'north_west', 'south_east')) %>% 
  group_by(Directions) %>%  summarise(n=n()) %>%
  summarise(statistic = (n[1] - n[2])*100/ n[2] )
  

# Odp: nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód jest wiecj o 0,148 %



# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?

df %>%  select(yr_built, bathrooms) %>%
  filter(yr_built>= 1990 &  yr_built<=2000) %>%
  mutate(Time_period = ifelse(yr_built== 2000, 2000, 90)) %>% 
  group_by(Time_period) %>%
  summarise(Bathrooms_median = median(bathrooms))

# Odp: Mediana nie ulegla zmianie, utrzymuje sie na poziomie 2.5 


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

df %>%  filter(lat >= latitude[1,1]) %>%
  select(grade, waterfront) %>%
  group_by(waterfront) %>% 
  summarise(q1 = quantile(grade, 0.25), q3 = quantile(grade, 0.75))


# Odp: Dla nieruchomosci NIE polozonych nad woda Q1 = 7, Q3 = 8, dla nieruchomosci polozonych nad woda Q1 = 8, Q3 = 11


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

df %>%  count(zipcode, sort = TRUE) %>% 
  slice(1) %>%
  left_join(df) %>% 
  select(zipcode,price) %>% 
  group_by(zipcode) %>% 
  summarise(IQR = quantile(price, 0.75)- quantile(price, 0.25))

# Odp: Kod pocztowy to 98103, a rozstep miedzykwartylowy tego adresu to 262875


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

df %>% select(sqft_living, sqft_living15) %>%
  mutate(less_than_living15 = ifelse(sqft_living < sqft_living15, 1, 0)) %>% 
  summarise(percentage = (sum(less_than_living15)/ nrow(.))* 100)


# Odp: Wyzsza srednia powierzchnie 15 najblizszych sasiadow wzgledem swojej powierzchni ma 42,59 % nieruchomosci


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

# pokoj == sypialnia 

df %>% filter(yr_built> 1970 & price> quantile(price, 0.75) & yr_renovated>= 2012) %>%
  select(bedrooms) 

# Odp: Liczba pokoi to 3 , 4 lub 5


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

#boxplot.stats(df$sqft_living)
#boxplot(df$sqft_living)

df %>% select(sqft_living) %>%
  filter(sqft_living %in% boxplot.stats(sqft_living)$out) %>%
  count()

# Odp: Jest 572 wartosci odstajacych, wszystkie gornej granicy


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

# ft^2 - stopy kwadratowe 
# m^2 = ft^2/ 10.764

df %>%  select(sqft_living, price) %>%
  mutate(sqm_living = sqft_living/10.764) %>% 
  mutate(Cost_per_sq_meter =price/sqm_living ) %>%
  select(- c(sqft_living,price,sqm_living)) %>% top_n(1)

# Odp: Najwiêksza cena za metr kwadratowy powierzchni mieszkalnej to 8720.335