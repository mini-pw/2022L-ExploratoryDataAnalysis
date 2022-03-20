library(dplyr)
setwd("C:/Users/idani/Desktop/DanielewskaIza")
df <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
df %>% 
  filter(grade >= median(grade) & waterfront == 1) %>% 
  summarise(mean_price = mean(price))
# Odp: 1784152


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
df %>% 
  filter(floors == 2) %>% 
  summarise(med_2floors = median(bathrooms))

df %>% 
  filter(floors == 3) %>% 
  summarise(med_3floors = median(bathrooms))
# Odp. Nieruchomości o 2 piętrach mają taką samą liczbę łazienek co te o 3 piętrach (w oparciu o mediany)/
#  W obu przypadkach mediana wynosi 2.5.


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?
m_lat <- mean(df$lat)
m_long <- mean(df$long)

pln_zach <- df %>% 
  filter(long<m_long & lat>m_lat) %>% 
  count()

pld_wsch <- df %>% 
  filter(long>m_long & lat<m_lat) %>% 
  count()

(pln_zach-pld_wsch)/pld_wsch * 100
# Odp: Na pólncny zachód leży o 29.66597% więcej nieruchomości niż na południowy wschód


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych w roku 2000?
df %>% 
  filter(yr_built <= 2000 & yr_built >= 1990) %>%
  group_by(yr_built) %>% 
  summarise(med = median(bathrooms))
# Odp: Liczba łazienek (mediana) nie zmieniła się w ciągu tych lat i wynosi 2.5.


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
df %>% 
  filter(lat>m_lat) %>% 
  group_by(waterfront) %>% 
  summarise(kwartyl25 = quantile(grade, 0.25), kwartyl75 = quantile(grade, 0.75))
# Odp: Nieruchomości bez widoku na wodę : kwartyl 0.25 -  7, kwartyl 0.75 - 8; nieruchomości z widokiem na wodę: kwartyl 0.25 - 8, kwartyl 0.75 - 11.


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
df %>% 
  group_by(zipcode) %>% 
  summarise(count = n(),iqr = IQR(price)) %>% 
  arrange(desc(count)) %>% 
  head(1)
# Odp: Najwięcej nieruchomości jest położonych pod kodem 98103 i rozstęp międzykwartylowy dla ceny nieruchomości położonych pod tym adresem wynosi 262875.


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
df %>% 
  filter(sqft_living < sqft_living15) %>% 
  count()/count(df) *100
# Odp: 42.59473 % nieruchomości ma wyższą średnią powierzchnię dla 15 najbliższych sąsiadów niż swoją powierzchnię. 


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
year <- max(df$yr_renovated) - 10
iqr <- quantile(df$price,0.75)

df %>% 
  filter(yr_built > 1970 & yr_renovated >= year & price > iqr) %>%
  summarise(mean = mean(bedrooms), mediana = median(bedrooms))
# Odp: Średnia liczba pokoi dla tych nieruchomości wynosi 3.956522, a mediana 4.


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
dane <- df %>%
  summarise(kwantyl25 = quantile(sqft_living, 0.25),kwantyl75 = quantile(sqft_living, 0.75), iqr= IQR(sqft_living))

dolna_wart_gr <- dane$kwantyl25-1.5*dane$iqr
gorna_wart_gr <- dane$kwantyl75+1.5*dane$iqr

df %>%
  filter(sqft_living < dolna_wart_gr) %>% 
  count() # dolna

df %>% 
  filter(sqft_living > gorna_wart_gr) %>% 
  count() # górna
# Odp: Poniżej dolnej granicy jest 0 wartości odstających, powyżej górnej granicy jest 572 wartości odstające.


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
df %>% 
  transmute(cena_za_metr = price/sqft_living) %>% 
  arrange(desc(cena_za_metr)) %>%
  head(1)
# Odp: Największa cena za metr to 810.1389.