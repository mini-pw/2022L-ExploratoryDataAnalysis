library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?

quality_median <- median(df$grade)

mean_price <- df %>%
  filter(grade >= quality_median, waterfront == 1) %>%
  summarise(mean = mean(price)) %>%
  unlist()

# Odp: 1784152


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

df %>%
  group_by(floors) %>%
  summarise(median = median(bathrooms)) %>%
  filter(floors == 2 | floors == 3)

# Odp: Nie


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

middle_long <- mean(range(df$long))
middle_lat <- mean(range(df$lat))

how_many <- df %>%
  filter((lat > middle_lat & long < middle_long) | (lat < middle_lat & long > middle_long)) %>%
  mutate(location = ifelse(lat > middle_lat, "NW", "SE")) %>%
  group_by(location) %>%
  summarise(n = n()) 

ratio <- unlist(((how_many[1, 2] - how_many[2, 2])/how_many[2, 2])*100)

# Odp: 18343.53%


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?

median1 <- df %>%
  filter(yr_built >= 1990, yr_built <= 1999) %>%
  summarise(median = median(bathrooms)) %>%
  unlist()

median2 <- df %>%
  filter(yr_built == 2000) %>%
  summarise(median = median(bathrooms)) %>%
  unlist()

# Odp: Nie zmieni³a siê


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

df %>% 
  filter(lat > middle_lat) %>%
  group_by(waterfront) %>%
  summarise(qt_25 = quantile(grade)[2], gt_75 = quantile(grade)[4])


# Odp: Je¿eli jest nad wod¹, to kwartyl 0.25 = 8, kwartyl 0.75 = 10, je¿eli nie jest nad wod¹, to kwartyl 0.25 = 7, kwartyl 0.75 = 8.


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

zipcode1 <- df %>% 
  group_by(zipcode) %>%
  summarise(n = n()) %>%
  top_n(1, n) %>%
  select(zipcode) %>%
  unlist()

iqr <- df %>%
  filter(zipcode == zipcode1) %>%
  summarise(iqr = IQR(price)) %>%
  unlist()


# Odp: Kod pocztowy: 98103, rozstêp miêdzykwartylowy: 262875.


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

ratio <- df %>%
  mutate(is_higher = ifelse(sqft_living15 > sqft_living, "yes", "no")) %>%
  group_by(is_higher) %>%
  summarise(n = n()) %>%
  mutate(ratio = (n/sum(n))*100) %>%
  filter(is_higher == 'yes') %>%
  select(ratio) %>%
  unlist()

# Odp: 42.6%


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

df %>%
  filter(price > quantile(price)[4], yr_renovated >= 2012, yr_built >= 1970) %>%
  group_by(bedrooms) %>%
  summarise(n = n())


# Odp: 4 z 3 pokojami, 2 z 4 pokojami i 1 z 5 pokojami.


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

q_25 <- quantile(df$sqft_living)[2]
q_75 <- quantile(df$sqft_living)[4]
iqr <- IQR(df$sqft_living)

outliers <- df %>%
  filter(((sqft_living < q_25 - 1.5*iqr) | (sqft_living > q_75 + 1.5*iqr)) & ((sqft_living >= q_25 - 3*iqr) | (sqft_living <= q_75 + 3*iqr))) %>%
  summarise(n = n()) %>%
  unlist()

# Odp: 572


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

top_price <- df %>%
  mutate(sqm = sqft_living/(10.764)) %>%
  mutate(price_per_sqm = price/sqm) %>% 
  select(price_per_sqm) %>%
  top_n(1, price_per_sqm) %>%
  unlist()

# Odp: 8720.335