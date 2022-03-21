library(dplyr)

df <- read.csv("house_data.csv")

# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

df %>% 
  filter(waterfront == 1, grade >= median(grade)) %>% 
  summarise(avg_price = mean(price))

# Odp: 1784152 (przyjęłam, że chodzi o medianę jakości wykończenia dla wszystkich nieruchomości)


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

df %>% 
  filter(floors == 2) %>% 
  summarise(bathrooms_med = median(bathrooms)) 

# mediana liczby łazienek w nieruchomosciach o 2 piętrach: 2,5
  

df %>% 
  filter(floors == 3) %>% 
  summarise(bathrooms_med = median(bathrooms))

# mediana liczby łazienek w nieruchomosciach o 2 piętrach: 2,5

# Odp: Nie


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

df %>% 
  select(lat, long) %>% 
  summarise(centre_lat = median(lat), centre_long = median(long))

# środek miasta ma współrzędne 47.5718 N 122.23 W

df %>% 
  filter(lat > 47.5718, long < -122.23) %>% 
  summarise(count = n()) %>% 
  pull(1) -> n1

# na północny zachód jest 6096 nieruchomości

df %>% 
  filter(lat < 47.5718, long > -122.23) %>% 
  summarise(count = n()) %>% 
  pull(1) -> n2

# na połódniowy wschód jest 6087 nieruchomości

(n1 -n2) / n1 * 100

# Odp: Na północny zachód jest 0,15 % więcej nieruchomości


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

df %>% 
  filter(yr_built >= 1990, yr_built <= 1999) %>% 
  summarise(bathrooms_med = median(bathrooms)) 

df %>% 
  filter(yr_built >= 2000) %>% 
  summarise(bathrooms_med = median(bathrooms)) 
  

# Odp: Liczba łazienek się nie zmieniała (wynosiła 2.5)


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

# z widokiem na wodę:

df %>% 
  filter(waterfront == 1, lat > 47.5718) %>% 
  summarise(first_quantile = quantile(grade, 0.25), third_quantile = quantile(grade, 0.75))

# bez widoku na wodę: 

df %>% 
  filter(waterfront == 0, lat > 47.5718) %>% 
  summarise(first_quantile = quantile(grade, 0.25), third_quantile = quantile(grade, 0.75))

# Odp: Nieruchomości z widokiem na wodę: kwartyl 0.25: 8, kwartyl 0.75: 11, nieuchomości bez widoku na wodę: kwartyl 0.25: 7, kwartyl 0.75: 8


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(count = n()) %>% 
  top_n(1, count)

df %>% 
  filter(zipcode == 98103) %>% 
  summarise(price_IQR = IQR(price))

# Odp: Najwięcej nieruchomości jest położonych pod kodem pocztowym: 98103, a rozstęp międzykwartylowy dla cen nieruchomości pod tym adresem wynosi: 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

df %>% 
  filter(sqft_lot < sqft_lot15) %>% 
  summarise(count = n()) %>% 
  pull(1) -> n1

# liczba nieruchomości o wyższej średniej powierzchni 15 najbliższych sąsiadów względem swojej powierzchni: 8540

df %>% 
  summarise(count = n()) %>% 
  pull(1) -> n2

# łaczna liczba nieruchomości: 21613

n1 / n2 * 100

# Odp: Wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni ma 39,5 % nieruchomości


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
  
df %>% 
  summarise(max_yr_renovated = max(yr_renovated))

# najpóźniejszy rok remontu to 2015

df %>% 
  filter(price > quantile(price, 0.75)) %>% 
  filter(yr_renovated > 2005, yr_built > 1970) %>% 
  group_by(bedrooms) %>% 
  summarise(count = n())

# Odp: 7 nieruchomości ma 3 pokoje, 9 ma 4 pokoje i 5 ma 5 pokoi


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

df %>% 
  summarise(IQR_value = IQR(sqft_lot), first_quantile = quantile(sqft_lot, 0.25), third_quantile = quantile(sqft_lot, 0.75))

# wartość IQR dla powierzchni nieruchomości: 5648
# wartość pierwszego kwartyla: 5040
# wartość 3 kwartyla: 10688

IQR_value <- 5648
quantile1 <- 5040
quantile3 <- 10688

df %>% 
  filter(sqft_lot < quantile1 - 1.5 * IQR_value | sqft_lot > quantile3 + 1.5 * IQR_value) %>% 
  summarise(count = n())

# Odp: Odstającyh wartości jest 2425


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

# 1 stopa to ok. 0.3048 metra, 1 stopa kwadratowa to ok. 0,0929 metra

df %>% 
  mutate(price_per_sqm = price / sqft_living / 0.0929) %>% 
  summarise(max_price_per_sqm = max(price_per_sqm))

# Odp: Największa cena za metr kwadratowy to 8720.548