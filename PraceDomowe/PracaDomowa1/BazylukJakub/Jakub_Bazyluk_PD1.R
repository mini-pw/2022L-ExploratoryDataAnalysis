library(dplyr)

df <- read.csv("house_data.csv")

head(df)
# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

df %>% 
  summarise(median_grade = median(grade)) %>% 
  pull(median_grade) -> median_grade 

df %>% 
  filter(waterfront == 1, grade >= median_grade) %>%
  summarise(mean_price = mean(price)) %>% 
  pull(mean_price)
  
# Odp: 1 784 152 (uwaga: zinterpretowałem, że chodzi o medianę dla wszystkich mieszkań)


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

df %>% 
  filter(floors == 2) %>% 
  summarise(median_bedrooms2 = median(bedrooms)) %>% 
  pull(median_bedrooms2) -> median_bedrooms2

df %>% 
  filter(floors == 3) %>% 
  summarise(median_bedrooms3 = median(bedrooms)) %>% 
  pull(median_bedrooms3) -> median_bedrooms3

median_bedrooms2 > median_bedrooms3

# Odp: TAK


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

df %>% 
  summarise(mid_lat = mean(lat)) %>% 
  pull(mid_lat) -> mid_lat

df %>% 
  summarise(mid_long = mean(long)) %>% 
  pull(mid_long) -> mid_long

df %>% 
  filter(lat > mid_lat, long < mid_long) %>% 
  summarise(n = n()) %>% 
  pull(n) -> n_north_west

df %>% 
  filter(lat < mid_lat, long > mid_long) %>%
  summarise(n = n()) %>% 
  pull(n)  -> n_south_east

(n_north_west - n_south_east)/n_south_east

# Odp: o 30%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

df %>% 
  filter(yr_built >= 1990, yr_built <=1999) %>% 
  summarise(med_bathroom = median(bathrooms)) %>% 
  pull(med_bathroom) -> bathroom_1990

df %>% 
  filter(yr_built >= 2000) %>% 
  summarise(med_bathroom = median(bathrooms)) %>% 
  pull(med_bathroom) -> bathroom_2000

# Odp: Nie zmieniła się (UWAGA: w zadaniu pojawiła się literówka, przyjąłem że chodziło o "OD roku 2000")


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

df %>% 
  filter(lat > mid_lat, waterfront == 1) %>% 
  summarise(q25_view = quantile(grade, 0.25)) %>% 
  pull(q25_view) -> q25_view
  

df %>% 
  filter(lat > mid_lat, waterfront == 1) %>% 
  summarise(q75_view = quantile(grade, 0.75)) %>% 
  pull(q75_view) -> q75_view

df %>% 
  filter(lat > mid_lat, waterfront == 0) %>% 
  summarise(q25_no_view = quantile(grade, 0.25)) %>% 
  pull(q25_no_view) -> q25_no_view


df %>% 
  filter(lat > mid_lat, waterfront == 0) %>% 
  summarise(q75_no_view = quantile(grade, 0.75)) %>% 
  pull(q75_no_view) -> q75_no_view


# Odp: Z widokiem kolejno 8, 11; bez widoku kolejno 7, 8


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  top_n(1) %>% 
  pull(zipcode) -> max_zip

df %>% 
  filter(zipcode == max_zip) %>% 
  summarise(interq = quantile(price, 0.75) - quantile(price, 0.25)) %>% 
  pull(interq)

# Odp: Pod 98103, rozstęp 262 875



# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

df %>% 
  filter(sqft_living > sqft_living15) %>% 
  summarise(n = n()) %>% 
  pull(n) -> n

df %>% 
  summarise(n = n()) %>% 
  pull(n) -> n_all

n/n_all

# Odp: 45.5 %

# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
  
df %>% 
  summarise(expected_date = max(max(yr_built), max(yr_renovated))) %>% 
  pull(expected_date) -> expected_date

df %>% 
  summarise(q3 = quantile(price, 0.75)) %>% 
  pull(q3) -> q3

df %>% 
  filter(price > q3, yr_renovated >= (expected_date - 10), yr_built > 1970) %>% 
  summarise(n_room = mean(bedrooms)) %>% 
  pull(n_room)
# Odp: Mają (ŚREDNIO) 3.96 pokoi


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

df  %>% 
  summarise(outliers_high = quantile(sqft_living, 0.75) + 1.5*(quantile(sqft_living, 0.75) - quantile(sqft_living, 0.25))) %>% 
  pull(outliers_high) -> outliers_high


df  %>% 
  summarise(outliers_low = quantile(sqft_living, 0.25) - 1.5*(quantile(sqft_living, 0.75) - quantile(sqft_living, 0.25))) %>% 
  pull(outliers_low) -> outliers_low

df %>% 
  filter(sqft_living > outliers_high | sqft_living < outliers_low) %>% 
  summarise(n = n())

# Odp: 572


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

df %>% 
  mutate(price_m2 = price/(sqft_living*0.09290304)) %>% 
  summarise(max_price_m2 = max(price_m2))

# Odp: 8720 (UWAGA: zakładam, że jednostką używaną w tabeli jest stopa kwadratowa - stąd sqft, oraz przeliczenie na metry)