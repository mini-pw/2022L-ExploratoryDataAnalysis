library(dplyr)

house <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś 
# wykończenia jest równa lub większa od mediany jakości wykończenia?
zad1 <-house %>% 
  select(price, waterfront, grade) %>% 
  filter(waterfront == 1) %>% 
  filter(grade>=median(grade))
print(mean(zad1$price))

# Odp: 2302236


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) 
# liczbę łazienek niż nieruchomości o 3 piętrach?
zad2<-house %>% 
  select(floors, bathrooms) %>% 
  filter(floors == 2 | floors == 3) %>% 
  group_by(floors) %>% 
  mutate(bath_med = median(bathrooms))
print(c(filter(zad2, floors == 2)[1,3], filter(zad2, floors == 3)[1,3]))

# Odp: mają w oparciu o medianę porównywalną ilość łazienek


# 3. O ile procent więcej jest nieruchomości leżących na północny zachód niż  
# nieruchomości leżących na południowy wschód?
# nie wiem na północny zachód od czego... więc wyliczę jako punkt odniesienia 
# środek (mediana z długości i szerokości)
srodek = c(median(house$lat), median(house$long))
zad3 <- house %>% 
  select(lat,long) %>% 
  mutate(geo = case_when((lat<srodek[1] & long >srodek[2]) ~ "NW",
                         (lat>srodek[1] &long<srodek[2]) ~ "SE",
                                 TRUE ~ "Cut")) %>% 
  filter(geo == "NW" | geo == "SE") %>% 
  count(geo)
abs((zad3[1,2]-zad3[2,2])/zad3[1,2])*100


# Odp:o 0.14 procenta


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudowanych w 
# latach 90 XX wieku względem nieruchmości wybudowanych w roku 2000?
zad4 <- house %>% 
  select(yr_built,bathrooms) %>% 
  mutate(time = case_when((1990<=yr_built & yr_built<2000) ~ "90'",
                         yr_built==2000 ~ "2000",
                         TRUE ~ "Cut")) %>% 
  filter(time == "90'" | time == "2000") %>% 
  group_by(time) %>% 
  summarise(mediana=median(bathrooms))
head(zad4)

# Odp: mediany wynoszą 2,5


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości
# położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
srodek = c(median(house$lat), median(house$long))
zad5 <- house %>% 
  select(waterfront,grade, long) %>% 
  filter(long >= srodek[2]) %>% 
  group_by(waterfront) %>% 
  summarise(quant_grade = quantile(grade)) %>%
  ungroup()
zad5[c(2,4,7,9),]


# Odp: kwartyle z widokiem: 8,11 ; kwartyle bez widoku: 7,9


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki 
# jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
house %>% 
  count(zipcode) %>% 
  arrange(-n) %>% 
  head(1)
# 98103
# teraz rozstęp

house %>% 
  filter(zipcode == "98103") %>% 
  select(price) %>% 
  summarise(q = quantile(price, na.rm = TRUE)) -> quantiles

abs(quantiles$q[2]-quantiles$q[4])


# Odp:Najwięcej nieruchomości jest pod kodem pocztowym: 98103, a rozstęp 
# kwartylowy ich cen wynosi 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych 
# sąsiadów względem swojej powierzchni?
 
 house %>% 
   select(id, sqft_living, sqft_living15) %>% 
   mutate(założenie = ifelse(sqft_living15 >sqft_living, "True", "False")) %>% 
   count(założenie)
 (9206/21613)*100
 

# Odp: 42,59473


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci 
# kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy 
# były zbierane dne) oraz zostały zbudowane po 1970?
 
quantile(house$price)
house %>% 
  arrange(-yr_renovated) %>% 
  select(yr_renovated) %>% 
  head(1)
house %>% 
  arrange(-yr_built) %>% 
  select(yr_built) %>% 
  head(1)
 
#trzeci kwartyl : 645000, ostatni rok remontu i budowy: 2015

house %>% 
  filter(yr_built>1970 & yr_renovated > 2005 & price > 645000) %>% 
  count(bedrooms)
  
 


# Odp:3 pokoje-7 domów, 4 pokoje-9 domów, 5 pokoi-5 domów


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) 
# wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i
# górna granica wartości odstajacej).

quantile(house$sqft_lot, seq(0.25, 0.75, 0.5), na.rm = TRUE)
         
#kwartyle: pierwszy - 5040, drugi-10688
Q1 = 5040
Q3 = 10688
dolna <- 4*Q1 - 3*Q3
górna <- 4*Q3 - 3*Q1
house %>% 
  filter(sqft_lot < dolna) %>% 
  summarise(n = n())

house %>% 
  filter(sqft_lot > górna) %>% 
  summarise(n = n())


# Odp: poniżej dolnej granicy jest 0, powyżej górnej jest 1771


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy 
# biorąc pod uwagę tylko powierzchnię mieszkalną.

house %>% 
  mutate(sqm_living = sqft_living*0.093) %>% 
  transmute(price_sqm = price/sqm_living) %>% 
  arrange(-price_sqm) %>% 
  head(1)

# Odp: 8711,171