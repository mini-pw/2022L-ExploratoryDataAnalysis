library(dplyr)

df <- read.csv("house_data.csv")

# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

df %>%
  filter(grade >= median(grade)) %>%
  filter(waterfront == 1) %>% 
  summarise(srednia = mean(price))

# Odp:1784152


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?


df %>%
  filter(floors == 2 | floors == 3) %>% 
  group_by(floors) %>% 
  summarise(mediana = median(bathrooms))

# Odp:Liczba lazienek w oparciu o wartosc mediany jest taka sama i wynosi 2.5



# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

NW <- df %>%
  filter(long < mean(long) & lat > mean(lat)) %>%
  summarise(n())
SE <- df %>%
  filter(long > mean(long) & lat < mean(lat)) %>%
  summarise(n())
(NW / SE - 1) * 100
# Odp: Jest ich wiecej o 29.66597%



# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?

df %>%
  group_by(yr_built_90 = yr_built >= 1990 & yr_built < 2000, yr_built_20 = yr_built >= 2000) %>% 
  summarise(mediana = median(bathrooms))

# Odp: Mediana nie zmienila sie i byla rowna 2.5


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy
#biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

df %>%
  filter(lat > mean(lat)) %>%
  group_by(waterfront) %>%
  summarise(kwartyl_1 = quantile(grade, 0.25), kwartyl_3 = quantile(grade, 0.75))
           
# Odp: Dla nieruchomosci lezacych nad woda wartosci kwartyla 0.25 oraz kwartyla 0.75 sa wieksze
#waterfront kwartyl_1 kwartyl_3
# 0         7         8
# 1         8        11


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i 
#jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n() ) %>% 
  arrange(-n) %>% 
  top_n(1)
# uzyskałem ze najwiecej nieruchomosci lezy pod kodem pocztowym 98103

df %>%
  filter(zipcode == 98103) %>% 
  select(price) %>% 
  summarise(kwartyl_1 = quantile(price, 0.25), kwartyl_3 = quantile(price, 0.75)) %>% 
  mutate(IQR = kwartyl_3 - kwartyl_1) %>% 
  select(IQR)
  
# Odp: 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

n = length(df$sqft_living)
n_2 = df %>%
  filter(sqft_living15 > sqft_living) %>%
  summarise(n())
n_2 / n * 100
  
# Odp: 42.59473%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach
#(pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?

df %>%
  mutate(kwartyl_3 = quantile(price, 0.75)) %>%
  mutate(max = max(yr_renovated)) %>%
  filter(price > kwartyl_3 & yr_renovated > (max - 10) & yr_built > 1970) %>% 
  group_by(bedrooms) %>% 
  count(bedrooms)
  
# Odp: Nieruchomosci spelniajace warunki zadania maja 3, 4 lub 5 pokoi
#  bedrooms     n
#1        3     7
#2        4     9
#3        5     5


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających 
#względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

df %>%
  filter( sqft_living > ( quantile(sqft_living, 0.75) + 1.5*( quantile(sqft_living, 0.75) - quantile(sqft_living, 0.25) )) | 
           sqft_living < ( quantile(sqft_living, 0.25) - 1.5*( quantile(sqft_living, 0.75) - quantile(sqft_living, 0.25) ))) %>% 
  summarise(n = n())

# Odp: Jest 572 wartosci odstajacych


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

df %>%
  mutate(Cena_mk = price / sqft_living) %>% 
  select(Cena_mk) %>% 
  arrange(-Cena_mk) %>% 
  top_n(1)
  
# Odp: 810.1389