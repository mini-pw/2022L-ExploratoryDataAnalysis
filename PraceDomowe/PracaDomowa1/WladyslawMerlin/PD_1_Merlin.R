library(dplyr)

df <- read.csv("house_data.csv")
View(df)


# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
median = df %>% 
  summarise(median(grade))
  
df_1<- df %>% 
  filter(waterfront == 1 & grade >= median[1,1] ) %>% 
  summarise(srednia_cena = mean(price))
  

# Odp: 1784152


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
df_2pietra <- df %>% 
  filter(floors == 2) %>% 
  summarise(srednia_lazienek = median(bathrooms)) 

df_3pietra <-df %>% 
  filter(floors == 3) %>% 
  summarise(srednia_lazienek = median(bathrooms))

# Odp:Taka sama liczba lazienek


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?
zad_3_pom<- df %>% 
  select(lat,long)

pocz_lat<- quantile(zad_3_pom$lat,0.5)
pocz_long<- quantile(zad_3_pom$long,0.5)

zad_3_pz<-zad_3_pom %>% 
  filter(lat > pocz_lat, long < pocz_long) #6096
zad_3_pw<-zad_3_pom %>% 
  filter(lat < pocz_lat, long > pocz_long) #6087
odp = (6096 - 6087)*100/(6096 + 6087)
  

# Odp:0.07387343%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
df_90 <- df %>% 
  filter(yr_built < 2000, yr_built > 1989) %>% 
  summarise(srednia_90 = median(bathrooms))

df_00  <- df %>% 
  filter(yr_built == 2000) %>% 
  summarise(srednia_00 = median(bathrooms))

# Odp: 2,5 dla 90 xx i 2000, czyli taka sama 



# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
zad_5_z_wid<- df %>% 
  filter(lat > pocz_lat,waterfront == 1) %>% 
  arrange(grade)
z_wid_q1 = quantile(zad_5_z_wid$grade,0.25)
z_wid_q3 = quantile(zad_5_z_wid$grade,0.75)

zad_5_bez_wid<- df %>% 
  filter(lat > pocz_lat,waterfront == 0) %>% 
  arrange(grade)
bez_wid_q1 = quantile(zad_5_bez_wid$grade,0.25)
bez_wid_q3 = quantile(zad_5_bez_wid$grade,0.75)

# Odp:z widokiem: q1 = 8, q3 = 11; bez widoku: q1 =7, q3 = 8;


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
df_poczt <-df %>% 
  group_by(zipcode) %>% 
  summarise(ile = n()) %>% 
  arrange(desc(ile)) %>% 
  head(1)

df_poczt_filt<- df %>% 
  filter(zipcode == 98103) %>% 
  arrange(price) %>% 
  select(price)

df_1quartyl <- quantile(df_poczt_filt$price,0.25)#432125
df_3quartyl <- quantile(df_poczt_filt$price,0.75)#695000

wynik = df_3quartyl - df_1quartyl

# Odp:98103, 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
 # liczac teren poza mieszkaniem
df_powierch <- df %>% 
  filter(sqft_living + sqft_lot < sqft_living15 + sqft_lot15) %>% 
  summarise(liczba = n())
#nie liczac
df_powierch_2 <- df %>% 
  filter(sqft_living < sqft_living15) %>% 
  summarise(liczba = n())

wynik = 10012*100/21613
wynik_2 = 9206*100/21613


# Odp: ~46.3%/~42.6%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
q3<-quantile(df$price,0.75)
zad_8 <- df %>% 
  filter(price > q3, yr_built > 1970,yr_renovated > (max(df$yr_renovated)-10)) %>% 
  summarise(srednia_liczba_pokoi = mean(bedrooms + bathrooms))

# Odp:srednia liczba pokojow(sypialni + lazienki) jest 7.190476

 
# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
  boxplot(df$sqft_living)$stats
  out<-boxplot(df$sqft_living)$out

# Odp:dolna granica = 290, nie ma wartosci odstajacych, gorna granica = 4230, 572 wartosci odstajacych


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
zad_10 <-df %>% 
  select(price, sqft_living) %>% 
  mutate(sqft_m = sqft_living*0.092903) %>% 
  transmute(price_mtr = price/sqft_m) %>% 
  arrange(desc(price_mtr)) %>% 
  slice(1)
  

# Odp:8720.266