library(dplyr)
library(tidyr)
library(readr)
data <- read_csv("https://raw.githubusercontent.com/MI2-Education/2022L-ExploratoryDataAnalysis/main/PraceDomowe/PracaDomowa1/house_data.csv")

# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakość wykończenia jest równa lub większa od mediany jakości wykończenia?

data %>% 
  filter(grade >= median(grade, na.rm = TRUE), waterfront == 1) %>% 
  summarise(mean_price = mean(price))

# Odp: Średnia cena wynosi 1784152.


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

second_floor <- data %>% 
  filter(floors == 2.0) %>% 
  summarise(median1 = median(bathrooms, na.rm = TRUE))

third_floor <- data %>% 
  filter(floors == 3.0) %>% 
  summarise(median2 = median(bathrooms, na.rm = TRUE))

# Odp: Nie, oba typy nieruchomości mają taką samą medianę wynoszącą 2.5.


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

north_west <- data %>% 
  filter(lat > median(lat), long < median(long)) %>%  # jako środek miasta przyjmuje medianę z szerokości i długości geograficznej, i od tych współrzędnych wyznaczam kierunki
  summarise(n = n())

south_east  <- data %>% 
  filter(lat < median(lat),  long > median(long)) %>% 
  summarise(n = n())

((north_west - south_east)/south_east) *100

# Odp: Jest o 0.1478% więcej.

# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

ninety_years <- data %>% 
  filter(yr_built >= 1990 & yr_built <= 1999) %>% 
  summarise(median_bathrooms = median(bathrooms, na.rm = TRUE))

twenty_first_century <- ninety_years <- data %>% 
  filter(yr_built >= 2000) %>% 
  summarise(median_bathrooms = median(bathrooms, na.rm = TRUE))

ninety_years - twenty_first_century

# Odp: Mediana nie zmieniła się, w obydwu przypadkach wynosi 2.5.


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

with_water <- data %>% 
  filter(lat > median(lat), waterfront == 1) %>% 
  summarise(quantile_grade1 = quantile(grade, na.rm = TRUE)) 
with_water <- with_water[c(2, 4),]

without_water <- data %>% 
  filter(lat > median(lat), waterfront == 0) %>% 
  summarise(quantile_grade2 = quantile(grade, na.rm = TRUE))
without_water <- without_water[c(2, 4),]

# Odp: W nieruchomościach z widokiem na wodę wartości kwartyli 0.25 i 0.75 to kolejno 8 i 11, 
# a tych bez widoku na wodę to 7 i 8.


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

data %>% 
  count(zipcode) %>% 
  top_n(1)

data %>% 
  filter(zipcode == 98103) %>% 
  summarise(IQR_price = IQR(price, na.rm = TRUE))

# Odp: Najwięcej nieruchomości jest położonych pod kodem 98103, a rozstęp międzykwartylowy między ich cenami wynosi 262875.


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

data %>% 
  filter(sqft_lot15 > sqft_lot) %>% 
  summarise((n = n()/count(data))* 100)

# Odp: Jest to w przybliżeniu 39.51326%.


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dane) oraz zostały zbudowane po 1970?

data %>% 
  filter(yr_built != 0) %>% 
  arrange(desc(yr_built)) %>% 
  head(1) # ostatni zapisany rok remontu domu to 2015 więc to od niego będe liczyła 10 lat wstecz

data %>% 
  filter(yr_built > 1970, yr_renovated >= 2005, price > quantile(price, 0.75)) %>% 
  count(bedrooms)

# Odp: Wśród takich nieruchomości jest 7 z trzema pokojami, 10 z czterema pokojami i 6 z pięcioma pokojami.


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

data %>% 
  filter(sqft_lot < (quantile(sqft_lot, 0.25) - 1.5*(IQR(sqft_lot))), sqft_lot > (quantile(sqft_lot, 0.75) + 1.5*(IQR(sqft_lot)))) %>% 
  summarise(n = n())

# Odp: Jest ich 2425.


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
 data %>% 
   transmute(price_per_m2 = price / sqft_living) %>% 
   top_n(1)
   
# Odp: Największa cena za metr kwadratowy to 810.1389.