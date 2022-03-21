library(dplyr)

df <- read.csv("house_data.csv")
colnames(df)

# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?

ans1 <- df %>%
  select(price,waterfront,grade) %>% 
  filter(waterfront==1) %>% 
  filter(grade>=median(grade)) %>% 
  summarise(m=mean(price))

# Odp: 2302236


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?

ans2 <- df %>% 
  select(bathrooms,floors) %>% 
  filter(floors %in% c(2,3)) %>% 
  group_by(floors) %>% 
  summarise(mediana=median(bathrooms))

# Odp: Obie mediany sa takie same - 2.5


# 3. O ile procent więcej jest nieruchomości leżacych na północny zachód niż nieruchomości leżących na południowy wschód?

middle <- df %>% 
  select(id,lat,long) %>%
  summarise(mlat=median(lat), mlong=median(long))
NW <- df %>% 
  select(id, lat, long) %>%
  filter(lat<middle$mlat, long>middle$mlong) %>% 
  count()
SE <- df %>% 
  select(id, lat, long) %>%
  filter(lat>middle$mlat, long<middle$mlong) %>% 
  count()
ans3 <- 100*(NW-SE)/NW
# Odp: Na NW jest 0 0.148% mniej nieruchomosci, gdzie srodek - wartosc srodkowa long i lat.


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudowanych w latach 90 XX wieku względem nieruchmości wybudowanych roku w 2000?

ans4 <- df %>% 
  select(bathrooms, yr_built) %>%
  filter(yr_built >=1990, yr_built<=2000) %>% 
  group_by(yr_built) %>% 
  summarise(mediana=median(bathrooms))
ans4

# Odp: Mediana nie zmienila sie pomiedzy rokiem 1990 i 2000 - 2.5


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?

middle <- df %>% 
  select(id,lat,long) %>%
  summarise(mlat=median(lat), mlong=median(long))

Q1 <- df %>% 
  select(waterfront, grade, long) %>%
  filter(long>middle$mlong) %>% 
  group_by(waterfront) %>% 
  filter(grade<=median(grade)) %>% 
  filter(grade ==median(grade)) %>% 
  select(waterfront, grade) %>% 
  unique()
Q1

Q3 <- df %>% 
  select(waterfront, grade, long) %>%
  filter(long>middle$mlong) %>% 
  group_by(waterfront) %>% 
  filter(grade>=median(grade)) %>% 
  filter(grade ==median(grade)) %>% 
  select(waterfront, grade) %>% 
  unique()
Q3

# Odp: Dla nieruchomosci nad woda Q1=9,Q3=11, dla pozostalych Q1=7,Q3=8.


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

maxnum <- df %>% 
  select(zipcode,price) %>% 
  group_by(zipcode) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n==max(n))
quants <- df %>% 
  select(zipcode, price) %>% 
  filter(zipcode==98103) %>% 
  summarise(quantiles=quantile(price))
quants[,1]
ans6 <- quants[4,]-quants[1,]
ans6
# Odp: Odstep = 457000.


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?

higher <- df %>% 
  select(sqft_lot, sqft_lot15) %>% 
  filter(sqft_lot<=sqft_lot15) %>% 
  count()
ans7 <- 100*higher/dim(df)[1]
ans7
# Odp: Okolo 60%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
Q <- df %>% 
  select(price) %>% 
  summarise(Q=quantile(price[4]))
ans8 <- df %>% 
  select(yr_renovated,yr_built, price, bedrooms) %>% 
  filter(price>Q3[4,1], yr_built>1970, yr_renovated>=2012)
ans8
# Odp: Maja od 3 do 5 pokoi.


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).

IQR_med <- df %>%
  select(sqft_lot) %>% 
  summarise(Q=quantile(sqft_lot), med=median(sqft_lot))
iqr <- IQR_med[4,1]-IQR_med[2,1]
med <- IQR_med[1,2]
ans9 <- df %>% 
  select(id, sqft_lot) %>% 
  filter(sqft_lot<=med-1.5*iqr, sqft_lot>=med+1.5*iqr) %>% 
  count()
# Odp: 0


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
#1 square foot = 0.09290304 square meters
ans10 <- df %>% 
  select(price, sqft_living) %>% 
  summarise(perm2=price/sqft_living/0.09290304) %>% 
  top_n(1, perm2)
ans10
# Odp: Najwieksza cena za m2 - 8720 dolarów za m^2