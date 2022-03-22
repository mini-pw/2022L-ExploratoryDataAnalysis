library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?

df %>% 
  filter(grade >= median(grade, na.rm = TRUE)) %>% 
  filter(waterfront == 1) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE))

# Odp: 1784152


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

df %>% 
  filter(floors == 2) %>% 
  summarise(med_2 = median(bathrooms, na.rm = TRUE))

df %>% 
  filter(floors == 3) %>% 
  summarise(med_3 = median(bathrooms, na.rm = TRUE))

# Odp: Nie, maj¹ tyle samo (2.5)


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

A <- df %>% 
     mutate(med_lat = median(lat), med_long = median(long)) %>% 
     filter(lat > med_lat & long < med_long) %>% 
     summarise(n())

B <- df %>% 
     mutate(med_lat = median(lat), med_long = median(long)) %>% 
     filter(lat < med_lat & long > med_long) %>% 
     summarise(n())

((A-B)/B)*100

# Odp: 0.1478561%


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?
df %>% 
  filter(yr_built <= 2000) %>% 
  summarise(med_90 = median(bathrooms, na.rm = TRUE))

df %>% 
  filter(yr_built > 2000) %>% 
  summarise(med_00 = median(bathrooms, na.rm = TRUE))


# Odp: Wzros³a o 0.5


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?
df %>% 
  filter(lat >= median(lat, na.rm = TRUE)) %>% 
  filter(waterfront == 1) %>% 
  summarise(q_1 = quantile(grade))

df %>% 
  filter(lat >= median(lat, na.rm = TRUE)) %>% 
  filter(waterfront == 0) %>% 
  summarise(q_1 = quantile(grade))

# Odp: Ma widok na morze: 8 i 11, nie ma widoku na morze: 7 i 8


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1) %>% 
  select(zipcode)

df %>% 
  filter(zipcode == 98103) %>% 
  summarise(iqr = IQR(price))

# Odp: 98103, 262875


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

rows <- nrow(df)
df %>% 
  filter(sqft_living15 > sqft_living) %>% 
  summarise(n = n()/rows) 

# Odp: 0.4259473


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

df %>% 
  filter(price > quantile(price)[4] & 
           yr_renovated >= 2012 & 
           yr_built > 1970) %>% 
  summarise(med = median(bedrooms))

# Odp: 3


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

df %>% 
  filter(sqft_living < (quantile(sqft_living)[2] - 1.5*IQR(sqft_living)) | 
         sqft_living > (quantile(sqft_living)[4] + 1.5*IQR(sqft_living))) %>% 
  summarise(n = n())

# Odp: 572


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.
df %>% 
  mutate(price_per_sqm = price/(sqft_living/(3.2808)^2)) %>% 
  arrange(-price_per_sqm) %>% 
  head(1) %>% 
  select(price_per_sqm)

# Odp: 8720.05