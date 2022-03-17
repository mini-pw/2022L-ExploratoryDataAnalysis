library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

med = median(df$grade)
df %>% 
  filter(waterfront == 1,grade >= med) %>% 
  summarise(avg_cena = mean(price))

# Odp: 1784152



# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
df %>% 
  filter(floors == 2.0) %>%
  select(bathrooms) %>%
  summarise(avg = median(bathrooms)) -> x2
df %>% 
  filter(floors == 3.0) %>% 
  select(bathrooms) %>%
  summarise(avg = median(bathrooms)) -> x3
x2>x3
# Odp: NIE (mają tą samą)

# 3. O ile procent więcej jest nieruchomości leżcych na północny zachód niż  nieruchomości leżących na południowy wschód?
#północny zachód:

# Współrzędne Seattle lat: 47.60621, long: -122.33207
df %>% 
  filter(lat > 47.60621, long < -122.33207) %>% 
  count() -> nierch_NW
#południowy wschód
df %>% 
  filter(lat < 47.60621, long > -122.33207) %>% 
  count() -> nierch_SE

abs(nierch_NW - nierch_SE)/nierch_SE * 100
# Odp: Na południowy wschód od Seattle leży o 73.05% więcej nieruchomości


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
df %>% 
  filter(yr_built > 1990, yr_built <= 2000) %>% 
  group_by(yr_built) %>% 
  summarise(med_bath = median(bathrooms))

# Odp: Była stale równa 2.5


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
# Współrzędne Seattle lat: 47.60621, long: -122.33207
df %>% 
  filter(lat > 47.60621,waterfront == 1) %>% 
  summarise((quan = quantile(grade, 0.25,na.rm = TRUE)))
df %>% 
  filter(lat > 47.60621,waterfront == 1) %>% 
  summarise((quan = quantile(grade, 0.75,na.rm = TRUE)))
df %>% 
  filter(lat > 47.60621,waterfront == 0) %>% 
  summarise((quan = quantile(grade, 0.25,na.rm = TRUE)))
df %>% 
  filter(lat > 47.60621,waterfront == 0) %>% 
  summarise((quan = quantile(grade, 0.75,na.rm = TRUE)))
  
# Odp: kwartyle 0.25: 8 nad woda, 7 nie nad woda, 0,75: 11 nad woda, 8 nie nad woda

# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
df %>% 
  group_by(zipcode) %>% 
  summarise(ile_nierch = n()) %>% 
  arrange(desc(ile_nierch)) %>% 
  slice(1) %>% 
  select(zipcode)
df %>% 
  filter(zipcode == 98103) %>% 
  summarise(quan = quantile(price, 0.25,na.rm = TRUE)) -> piersz_quan
df %>% 
  filter(zipcode == 98103) %>% 
  summarise(quan = quantile(price, 0.75,na.rm = TRUE)) -> trzeci_quan
rozstep <- trzeci_quan - piersz_quan
# Odp: pod kodem: 98103, rozstep cen dla tego kodu to 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
df %>% 
  filter(sqft_lot < sqft_lot15) %>% 
  count()
procent = 8540/21613 * 100

# Odp: 39,51%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach 
# (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?

df %>% 
  mutate(quan = quantile(price, 0.75, na.rm = TRUE)) %>%
  filter(yr_renovated >= 2012,price > quan, yr_built > 1970) %>% 
  select(bedrooms)
# Odp: 4, 3, 3, 3, 5


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) 
# wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
df %>%
  summarise(quantile(sqft_lot, 0.25))
df %>%
  summarise(quantile(sqft_lot, 0.75))

df %>% 
  filter(sqft_lot < 5040 - 1.5*(10688 - 5040) | sqft_lot > 10688 + 1.5*(10688 - 5040)) %>% 
  select(id)
# Odp: 2378 + 47 = 2425


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
df %>% 
  mutate(cena_za_metr2 = price/sqft_living) %>% 
  select(cena_za_metr2) %>% 
  arrange(desc(cena_za_metr2)) %>% 
  slice(1)

# Odp: 810.14