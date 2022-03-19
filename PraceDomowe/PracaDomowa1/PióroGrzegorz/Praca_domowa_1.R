df <- read.csv("R/house_data.csv")
library(dplyr)

df %>% 
  View()


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
df %>% 
  filter(waterfront == 1) %>% 
  select(price,waterfront,grade) %>% 
  filter(grade >= median(df$grade)) %>% 
  summarise(srednia = mean(price))

# Odp: 1784152


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?
df %>% 
  select(floors,bathrooms) %>% 
  filter(floors == 2 | floors == 3) %>% 
  group_by(floors) %>%
  summarise(liczba = median(bathrooms))
# Odp: Nie, nie maj¹. W obu przypadkach mediana wynosi 2.5.


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?
NW <- df %>% 
  select(id, lat, long) %>% 
  filter(lat < median(df$lat), long > median(df$long)) %>% 
  summarise(n = n())
SE <- df %>% 
  select(id, lat, long) %>% 
  filter(lat > median(df$lat), long < median(df$long)) %>% 
  summarise(n = n())

# Odp:Tych na po³udniowym wschodzie jest o 9 wiêcej.


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?
df %>% 
  select(bathrooms, yr_built) %>% 
  filter(yr_built == c(1990:1999)) %>% 
  summarise(med_dziew = median(bathrooms))
df %>% 
  select(bathrooms, yr_built) %>% 
  filter(yr_built == 2000) %>% 
  summarise(med_dziew = median(bathrooms))
# Odp: Mediana nie zmieni³a siê.


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?
df %>% 
  select(grade, long, waterfront) %>% 
  filter(long > median(long)) %>% 
  group_by(waterfront) %>% 
  summarise(x = quantile(grade))
  
# Odp: Dla nieruchomoœci po³ozonych nad wod¹, to Q1 = 8, Q3 = 11, a nie nad wod¹ Q1 = 7, Q3 = 9


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?
df %>% 
  select(price,zipcode) %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice(1)
df %>% 
  select(price, zipcode) %>% 
  filter(zipcode == 98103) %>% 
  summarise(srednia_cena = mean(price))

# Odp:584919.2


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?
x <- df %>% 
  select(sqft_lot15, sqft_lot) %>% 
  filter(sqft_lot15 > sqft_lot) %>% 
  summarise(n=n())
x[1]/ 21613
# Odp: 39.51326%


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?
df %>% 
  select(price) %>% 
  summarise(x = quantile(price))

df %>% 
  select(price, bedrooms, bathrooms, yr_built, yr_renovated) %>% 
  filter(yr_built > 1970, yr_renovated > max(yr_renovated - 10), price > 645000) %>% 
  mutate(liczba_pokoi = bathrooms + bedrooms) %>% 
  summarise(srednialp = mean(liczba_pokoi))

# Odp: Srednio 7.19 pokoi.


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).
boxplot(df$sqft_living)
quantile(df$sqft_living)
IQR(df$sqft_living)
df %>% 
  select(sqft_living) %>% 
  filter(sqft_living > quantile(df$sqft_living)[4]+1.5*IQR(df$sqft_living) | sqft_living < quantile(df$sqft_living)[2]-1.5*IQR(df$sqft_living)) %>% 
  summarise(n=n())
# Odp:572


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.
df %>% 
  select(price, sqft_living) %>% 
  mutate(cena_m2 = price/sqft_living) %>% 
  arrange(-cena_m2) %>% 
  slice(1)

# Odp: 810.1389