library(dplyr)
df <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

df %>% filter(grade >= median(grade) & waterfront == 1) %>% summarise(mean(price))

# Odp: Średnia cena nieruchomości wynosi 1784152.


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

df %>% group_by(floors) %>% filter(floors == 2 | floors == 3) %>% summarise(median(bathrooms))

# Odp: Nie, mają taką samą liczbę łazienek.


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

NW <- df %>% filter(lat > mean(lat) & long < mean(long)) %>% count()
SE <- df %>% filter(lat < mean(lat) & long > mean(long)) %>% count()
(NW - SE)/SE

# Odp: Jest ich o 29.67% więcej.


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

df %>% filter(yr_built >= 1990 & yr_built < 2010) %>% mutate(decade = floor(yr_built/10) ) %>% 
  group_by(decade) %>% summarise(median(bathrooms))
  
# Odp: Pozostała ona taka sama.


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

df %>% filter(lat > mean(lat)) %>% group_by(waterfront) %>% 
  summarise(Q1 = quantile(grade, 0.25), Q3 = quantile(grade, 0.75))

# Odp: Dla nieruchomości bez widoku na wodę kwartyle 0.25 i 0.75 wynosiły odpowiednio 7 i 8, natomiast dla tych z widokiem to było 8 i 11.


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df %>% group_by(zipcode) %>% summarise(count = n(), IQR = IQR(price)) %>% 
  arrange(-count) %>% head(1)

# Odp: Najwięcej nieruchomości jest pod kodem 98103, a odstęp miedzykwartylowy wynosi 262875.


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

df %>% filter(sqft_living15 > sqft_living) %>% summarise(percent = n()/length(df[,1]))

# Odp: Jest to 42.59% nieruchomości.


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
  
df %>% filter(yr_built > 1970 & price > quantile(price, 0.75) & yr_renovated > max(yr_renovated) - 10) %>% 
  group_by(bedrooms) %>% count()

# Odp: 7 z tych nieruchomości ma 3 pokoje, 9 z nich - 4, a 5 nieruchomości posiada 5 pokoi.


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

df %>% filter(sqft_living < quantile(sqft_living, 0.25) - 1.5*IQR(sqft_living)) %>% count()
df %>% filter(sqft_living > quantile(sqft_living, 0.75) + 1.5*IQR(sqft_living)) %>% count()

# Odp: Nie ma wartości odstających z dołu, natomiast wartości odstających z góry jest 572.


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

df %>% mutate(meter_price = price / sqft_living) %>% arrange(-meter_price) %>% head(1) %>% select(meter_price)

# Odp: Największa cena za metr kwadratowy wynosi 810.1389.