library(dplyr)

df <- read.csv("Documents/Programming/R/house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
df %>%
  filter(grade >= median(grade), waterfront == 1) %>%
  summarise(mean(price))

# Odp:
# 1784152

# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
df %>%
  filter(floors == 2) %>%
  summarise(median(bathrooms))
df %>%
  filter(floors == 3) %>%
  summarise(median(bathrooms))

# Odp: Nie, mają taką samą


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?

# współrzędne geograficanie Seattle: szerokość 47.6062100, długość -122.3320700

NW <- df %>%
  filter(lat > median(lat), long < median(long)) %>%
  summarise(n())

SE <- df %>%
  filter(lat < median(lat), long > median(long)) %>%
  summarise(n())

((NW - SE)/SE)*100

# Odp: jest więcej o 0.1478%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
df %>%
  filter(yr_built < 2000 & yr_built > 1989) %>%
  summarise(median(bathrooms))

df %>%
  filter(yr_built == 2000) %>%
  summarise(median(bathrooms))
# Odp: nie zmieniła się


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
df %>%
  filter(lat > median(lat)) %>%
  group_by(waterfront) %>%
  summarise(quanile1 = quantile(grade, 0.25), quantile3 = quantile(grade, 0.75))
# Odp: z widokiem na wodę: Q1 = 8, Q3 = 11
# bez widoku: Q1 = 7, Q3 = 8


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
df %>%
  group_by(zipcode) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(1)

df %>%
  filter(zipcode == 98103) %>%
  summarise(rozstep = quantile(price, 0.75) - quantile(price, 0.25)) 
# Odp: kod pocztowy 98103, rozstęp międzykwartylowy = 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

df %>%
  filter(sqft_lot15 > sqft_lot) %>%
  summarise((n() / count(df))*100) 

# Odp: 39.51%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
  
df %>%
  filter(price > quantile(price, 0.75), yr_built > 1970, yr_renovated >= (max(yr_renovated) - 10)) %>%
  count(bedrooms) 

# Odp: 3,4,5

# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
df %>%
  filter(sqft_lot < quantile(sqft_lot, 0.25) - 1.5*(IQR(sqft_lot)) |
         sqft_lot > quantile(sqft_lot, 0.75) + 1.5*(IQR(sqft_lot))) %>%
  summarise(n = n())
# Odp: 2425

# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
df %>%
  summarise(max_price = max(price / sqft_living))
   
# Odp: 810.1389
