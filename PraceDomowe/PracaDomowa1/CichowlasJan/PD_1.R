library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakość wykończenia jest równa lub większa od mediany jakości wykończenia?

df %>% 
  filter(waterfront == 1, grade >= median(df$grade)) %>% 
  summarise(mean_price = mean(price))

# Odp: Średnia cena to 1784152.


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

df %>% 
  filter(floors == 2) %>% 
  summarise(median(bathrooms)) > df %>% 
  filter(floors == 3) %>% 
  summarise(median(bathrooms))

# Odp: Nie.


# 3. O ile procent więcej jest nieruchomości leżcych na północny zachód niż  nieruchomości leżących na południowy wschód?

(df %>% filter(lat > mean(df$lat), long < mean(df$long)) %>% summarise(n()) - 
 df %>% filter(lat < mean(df$lat), long > mean(df$long)) %>% summarise(n()))/ 
 df %>% filter(lat < mean(df$lat), long > mean(df$long)) %>% summarise(n()) * 100


# Odp: 29,66597%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

df %>% filter(yr_built >= 1990, yr_built < 2000) %>% summarise(bathroom_number = median(bathrooms)) # 2.5
df %>% filter(yr_built == 2000) %>% summarise(bathroom_number = median(bathrooms)) # 2.5

# Odp: Nie zmieniła się.


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

df %>% 
  filter(lat > mean(df$lat)) %>% 
  group_by(waterfront) %>% 
  summarise(value = quantile(grade, c(0.25, 0.75)), q = c(0.25, 0.75))

# Odp: Dla tych bez widoku na wodę są to 7 i 8, a w przeciwnym przypadku są to 8 i 11. 


# 6. Pod którym kodem pocztowym jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(N = n(), IQR = quantile(price, c(0.75)) - quantile(price, c(0.25))) %>% 
  arrange(desc(N)) %>% 
  head(1)
  
# Odp: Najwięcej nieruchomości położonych jest pod kodem pocztowym 98103, a rozstęp międzykwartylowy dla ich cen wynosi 262875.


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

df %>% 
  filter(sqft_living15 + sqft_lot15 > sqft_living + sqft_lot) %>% # z definicji nieruchomości wziąłem sumę mieszkania i gruntu
  summarise(n()) / dim(df)[1] * 100

# Odp: Około 46,32%.


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
  
df %>% 
  filter(price > quantile(df$price, c(0.75)), yr_built > 1970, yr_renovated > max(df$yr_renovated) - 10) %>% 
  summarise(n_rooms = median(bedrooms))

# Odp: Mediana liczby sypialni (tak interpretuję liczbę pokoi, nie jako sumę bedrooms i bathrooms) dla tego typu nieruchomości wyniosła 4.


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

df %>% 
  transmute(area = sqft_living + sqft_lot) -> df_area  # z definicji nieruchomości wziąłem sumę mieszkania i gruntu

df_area %>% 
  summarise(Q1 = quantile(area, c(0.25)), Q2 = quantile(area, c(0.75)), IQR = Q2 - Q1) -> stats

df_area %>% 
  filter(area < stats$Q1 - 1.5*stats$IQR | area > stats$Q2 + 1.5*stats$IQR) %>% 
  summarise(n())

# Odp: Takich wartości jest 2419.


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

df %>% 
  transmute(price_per_sqm = price / (sqft_living * 0.0929)) %>% # 0.0929 przelicznik stóp kwadratowych na metry kwadratowe
  arrange(desc(price_per_sqm)) %>% 
  head(1)
  

# Odp: Najwyższa cena za metr kwadratowy to 8720,548.