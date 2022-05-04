# Weronika Dyszkiewicz

library(dplyr)
df <- read.csv("house_data.csv")
str(df)

# 1. Jaka jest średnia cena nieruchomości 
# położonych nad wodą, których jakoś wykończenia jest równa 
# lub większa od mediany jakości wykończenia?
df %>% 
  filter(waterfront==1 & (grade >= median(df$grade))) %>% 
  summarise(mean_price= mean(price))

# Odp:
# 1784152


# 2. Czy nieruchomości o 2 piętrach mają większą
#(w oparciu o wartości mediany) liczbę łazienek
#niż nieruchomości o 3 piętrach?
floor_2_bathroom <- df %>% 
  filter(floors==2) %>% 
  summarise(median_bathrooms= median(df$bathrooms))
floor_3_bathroom <- df %>% 
  filter(floors==3) %>% 
  summarise(median_bathrooms= median(df$bathrooms))
isTRUE(floor_2_bathroom > floor_3_bathroom)

# Odp:
# FALSE

# 3. O ile procent więcej jest nieruchomości leżcych 
# na północy zachód niż  nieruchomości leżących
# na południowy wschód?
med_lat  <- median(df$lat)
med_long <- median(df$long)
NW <- df %>% 
  filter(lat > med_lat & long<med_long) %>% 
  summarise(cnt= n())
SE <- df %>% 
  filter(lat < med_lat & long> med_long) %>% 
  summarise(ct = n())
(NW - SE) * 100/SE
# Odp:
# o 0.1478561 procent


# 4. Jak zmieniała się (mediana) liczba łazienek 
# dla nieruchomości wybudownych w latach 90 XX wieku
# względem nieruchmości wybudowanych w roku 2000?
med_1_bath <- df %>% 
  filter(yr_built>= 1990 & yr_built<2000) %>% 
  summarise(median_bath= median(df$bathrooms))
med_2_bath <- df %>% 
  filter(yr_built==2000) %>% 
  summarise(median_bathrooms= median(df$bathrooms))
odpowiedz <- med_1_bath- med_2_bath

# Odp:
# nie zmieniła się

# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75
# jakości wykończenia nieruchomości położonych na
#północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
med_lat  <- median(df$lat)
z_widokiem <- df %>% 
  filter(lat>med_lat & waterfront== 1) %>% 
  summarise(x = quantile(grade, c(0.25, 0.75)), 
            quantile = c(0.25, 0.75))
bez_widoku <- df %>% 
  filter(lat>med_lat & waterfront== 0) %>% 
  summarise(y = quantile(grade, c(0.25, 0.75)), 
            quant = c(0.25, 0.75))

# Odp:
# gdy z widokiem to kwartyl 0.25 wynosi 8, a kwartyl 0.75
# wynosi: 11
# bez widoku na wodę kwartyle 0.25, 0.75 wynoszą odpowiednio:
# 7 i 8


# 6. Pod którym kodem pocztowy jest położonych
# najwięcej nieruchomości i jaki jest rozstęp 
# miedzykwartylowy dla ceny nieruchomości położonych 
# pod tym adresem?
PROBA <- df %>% 
  group_by(zipcode)   %>% 
  summarise(countt = n()) %>% 
  top_n(1,countt)
rozstep <- df %>% 
  filter(zipcode == '98103') %>% 
  summarise(iqr = IQR(price))
# Odp:
# kod pocztowy: 98103, a roztęp: 262875

# 7. Ile procent nieruchomości ma wyższą średnią
# powierzchnię 15 najbliższych sąsiadów względem 
# swojej powierzchni?
wszystkie_nieruchomosci <- df %>% 
  summarise(liczba_nier = n())
df %>% 
  filter(df$sqft_lot < df$sqft_lot15) %>% 
  summarise(cnt = n()*100/wszystkie_nieruchomosci)

# Odp:
# 39.51326 procent

# 8. Jaką liczbę pokoi mają nieruchomości, 
#których cena jest większa niż trzeci kwartyl oraz miały
#remont w ostatnich 10 latach (pamietając że nie wiemy kiedy
#były zbierane dne) oraz zostały zbudowane po 1970?
liczba_pok <- df %>% 
  filter(price > quantile(price, 0.75) & yr_built >1970 & 
    yr_renovated> max(yr_renovated)-10) %>% 
  group_by(bedrooms) %>% 
  summarise(liczba_nier = n())
# Odp:
# 7 nieruchmości ma 3 pokoje,9 ma 4 pokoje i 
# 5 nieruchmosci 5 pokoi

# 9. Patrząc na definicję wartości odstających
# według Tukeya (wykres boxplot) wskaż ile 
#jest wartości odstających względem powierzchni 
#nieruchomości(dolna i górna granica wartości odstajacej).
df %>% 
  filter(sqft_lot < quantile(sqft_lot, 0.25)- 1.5*(IQR(sqft_lot)) |
           sqft_lot > quantile(sqft_lot, 0.75) + 1.5*(IQR(sqft_lot))) %>% 
  summarise(ile=n())

# Odp:
# jest 2425 takich wartości

# 10. Wśród nieruchomości wskaż jaka jest
# największą cena za metr kwadratowy biorąc 
# pod uwagę tylko powierzchnię mieszkalną.
df %>% 
  transmute(max_cena_za_metr= price/sqft_living) %>% 
  top_n(1,max_cena_za_metr)
# Odp:
# najwieksza cena za metr: 810.1389

