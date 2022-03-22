library(readr)
library(dplyr)
house_data <- read_csv("OneDrive - Politechnika Warszawska/house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
#bior? median? wyko?czenia wszystkich nieruchomo?ci
water_quality<- house_data %>% 
  filter(waterfront == 1 & grade >= median(grade))

av_price <- water_quality %>% 
  summarise(mean(price))

# Odp: 1784152 


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

# w dwóch roznych tabelkach 
two_floors <- house_data %>% 
  filter(floors == 2) %>% 
  summarise(median(bathrooms))
three_floors <- house_data %>% 
  filter(floors == 3) %>% 
  summarise(median(bathrooms))

# w jednej tabeli porównanie
floors <- house_data %>% 
  filter(floors == 2 | floors == 3) %>% 
  group_by(floors) %>% 
  summarise(median(bathrooms))

# Odp: Nieruchomości o 2 piętrach nie mają większej liby łazienek (w oparciu o medianę) niż nieruchomości o 3 piętrach. Mają one takie same mediany.

# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

av_long <- mean(house_data$long)
av_lat <- mean(house_data$lat)

house_data %>% 
  filter(long < av_long, lat > av_lat) %>% 
  summarize(ile = n()) %>% 
  head(1)[1,1] -> NW

house_data %>% 
  filter(long > av_long, lat < av_lat) %>% 
  summarize(n = n()) %>% 
  head(1)[1,1] -> SE 
Ans <- 100*NW/SE - 100
# Odp: 29.66597%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

yr_median_bathroom <- house_data %>% 
  filter(yr_built >= 1990, yr_built <=2000) %>% 
  group_by(yr_built) %>% 
  summarise(median(bathrooms))


# Odp: Mediana  w latach 1900-99 była stale taka sama i wynosiła 2.5 tak samo w roku 2000.


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
north_location_sea <- house_data %>% 
  filter(lat> av_lat) %>% 
  filter(waterfront == 1) %>% 
  summarise(sea_view = quantile(grade, c(0.25, 0.75)))
north_location_not_sea <- house_data %>% 
  filter(lat> av_lat) %>% 
  filter(waterfront == 0) %>% 
  summarise(notsea_view = quantile(grade, c(0.25, 0.75)))

# Odp: Wartość kwartyla dla nieruchomości nad morzem: q1=8, q3=11, nie nad morzem: q1=7, q3=8


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
postindex <-house_data %>% 
  group_by(zipcode) %>% 
  summarise(n=n()) %>% 
  top_n(1)

iqr<- house_data %>% 
  filter(zipcode == 98103) %>% 
  summarise(quantile(price, c(0.25,0.75)))


# Odp: Najwi?cej nieruchomo?ci po?o?onych jest pod kodem nr 98103. IQR = 69500 - 432125 = 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
ile <- house_data %>% 
  filter(sqft_living < sqft_living15) %>% 
  summarise(n = n()) %>% 
  head(1)[1,1]
ans2<- ile/dim(house_data)[1]*100


# Odp: 42.5947346504419%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?

#Gdy liczymy od dzis:
rooms <- house_data %>% 
  filter(price > quantile(price, 0.75), yr_built >1970, yr_renovated > 2012) %>%   select(id, price, yr_built, yr_renovated, bathrooms, bedrooms) %>% 
  mutate(rooms = bathrooms + bedrooms) %>% 
  summarize(suma = sum(rooms))


#Gdy liczymy 10 lat wstecz od daty ostatniego zarejestrowanego remontu:
top_data <- house_data %>% 
  summarise(m = max(yr_renovated)) %>% 
  head(1)[1,1]
rooms1 <- house_data %>% 
  filter(price > quantile(price, 0.75), yr_built >1970, yr_renovated > top_data - 10) %>%   select(id, price, yr_built, yr_renovated, bathrooms, bedrooms) %>% 
  mutate(rooms = bathrooms + bedrooms) %>% 
  summarize(suma = sum(rooms))


# Odp: Liczba pokoi nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach oraz zostały zbudowane po 1970?:
#W pierwszym przypadku 30.5, w drugim 151


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

q <- house_data %>% 
  summarise(quantiles = quantile(sqft_lot, c(0.25, 0.75)))
Q1 <- q[1,"quantiles"]
Q3 <- q[2,"quantiles"]
down <- 4*Q1 - 3*Q3 
up <- 4*Q3 - 3*Q1 
house_data %>% 
  filter(sqft_lot < (-11904)) %>% 
  summarise(n=n())

house_data %>% 
  filter(sqft_lot > 27632) %>% 
  summarise(n=n())

# Odp: Wartości odstających względem powierzchni nieruchomości wynosi 0 z dołu, 1771 z góry


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
# trzeba zamieni? stopy kwadratowe na metry kwadratowe
top_from_meter <- house_data %>% 
  mutate(sqmt_living = sqft_living*0.093) %>% 
  transmute(price_m = price/sqmt_living) %>% 
  arrange(-price_m) %>% 
  top_n(1)
# Odp: 8711.171