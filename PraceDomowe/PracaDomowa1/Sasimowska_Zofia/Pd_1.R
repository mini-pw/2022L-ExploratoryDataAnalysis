library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest srednia cena nieruchomosci polozonych nad woda, których jakos wykonczenia jest równa lub wieksza od mediany jakosci wykonczenia?
df %>%
  filter(grade >= median(grade, na.rm=TRUE),waterfront==1) %>% 
  summarize(mean(price))

# Odp: 1 784 870


# 2. Czy nieruchomosci o 2 pietrach maja wieksza (w oparciu o wartosci mediany) liczbe lazienek niz nieruchomosci o 3 pietrach?
laz2 <- df %>% 
  filter(floors==2) %>% 
  summarise(median(bathrooms, na.rm =TRUE))
laz3 <- df %>% 
  filter(floors==3) %>% 
  summarise(median(bathrooms, na.rm =TRUE))
isTRUE(laz2>laz3)

# Odp: Nie.


# 3. O ile procent wiecej jest nieruchomosci lezcych na pólnocy zachód niz  nieruchomosci lezacych na poludniowy wschód?
#Polozenie geograficzne nieruchomosci okreslam w stosunku do srodka odcinka laczacego nieruchomosci najbardziej na pólnoc, poludnie i najbardziej na wschód - zachód.
p_z <- df %>% 
  filter(long < (max(long)+min(long))/2, lat > (max(lat)+min(lat))/2) %>%
  nrow()
p_w <- df %>% 
  filter(long > (max(long)+min(long))/2, lat < (max(lat)+min(lat))/2) %>%
  nrow()
(p_z/p_w)*100 - 100
# Odp: Nieruchomosci lezacych na pólnocny-zachód jest o 18343.53% wiecej niz polozonych na poludniowy-wchód.


# 4. Jak zmieniala sie (mediana) liczba lazienek dla nieruchomosci wybudownych w latach 90 XX wieku wzgledem nieruchmosci wybudowanych roku 2000?
laz90 <- df %>%
  select(yr_built,bathrooms) %>% 
  filter(yr_built > 1989 & yr_built<2000) %>% 
  summarise(median(bathrooms, na.rm =TRUE))
laz2000 <-df %>% 
  select(yr_built,bathrooms) %>% 
  filter(yr_built==2000) %>% 
  summarise(median(bathrooms, na.rm =TRUE))

laz2000 - laz90

# Odp: Nie zmienia sie.


# 5. Jak wyglada wartosc kwartyla 0.25 oraz 0.75 jakosci wykonczenia nieruchomosci polozonych na pólnocy biorac pod uwage czy ma ona widok na wode czy nie ma?
susza <- df %>% 
  filter(lat > (max(lat)+min(lat))/2,waterfront==0)
quantile(susza$grade)
  
woda <- df %>% 
  filter(lat > (max(lat)+min(lat))/2,waterfront==1)
quantile(woda$grade)
# Odp: Dla domów polozonych nad woda kwartyl 0.25 wynosi 8, a kwartyl 0.75 wynosi 10. Dla domów nie nad woda kwartyl 0.25 wynosi 7, a kwartyl 0.75 wynosi 8.


# 6. Pod którym kodem pocztowy jest polozonych najwiecej nieruchomosci i jaki jest rozstep miedzykwartylowy dla ceny nieruchomosci polozonych pod tym adresem?
df %>% 
  count(zipcode) %>% 
  top_n(1)

dm <- df[df$zipcode==98103]
dm = dm$price
IQR(dm)

# Odp: Najwiecej nieruchomosci jest polozonych pod kodem pocztowym 98103. Rozstep miedzykwartylowy dla ceny nieruchomosci polozonych pod tym adresem wynosi 262 875.


# 7. Ile procent nieruchomosci ma wyzsza srednia powierzchnie 15 najblizszych sasiadów wzgledem swojej powierzchni?
lp<-df %>% 
  filter(sqft_living>sqft_living15) %>%
  summarise(n=n())
lq<-df %>% 
  summarise(n=n())
lp*100/lq

# Odp:45.53278%. Ze wzgledu na to, ze w tresci zadania jest slowo 'nieruchomosc', a nie ma slowa 'dzialka' uznalam, ze chodzi o powierzchnie mieszkalna.


# 8. Jaka liczbe pokoi maja nieruchomosci, których cena jest wieksza niz trzeci kwartyl oraz mialy remont w ostatnich 10 latach (pamietajac ze nie wiemy kiedy byly zbierane dne) oraz zostaly zbudowane po 1970?
df %>% 
  count(yr_renovated) %>% 
  arrange(-yr_renovated)
#Od poczatku 2000 roku co roku mialy miejsce renowacje, wiec uzasadniony jest wniosek, ze dane pochodza z roku 2015.
df %>% 
  filter(yr_renovated>2005, yr_built>1970, price>quantile(df$price)[4]) %>% 
  summarise(mean(bedrooms))

# Odp: Nieruchomosci spelniajace powyzsze warunki maja srednio 3.904762 pokoi.


# 9. Patrzac na definicje wartosci odstajacych wedlug Tukeya (wykres boxplot) wskaz ile jest wartosci odstajacych wzgledem powierzchni nieruchomosci(dolna i górna granica wartosci odstajacej).
IQR<-IQR(df$sqft_lot)
df %>% 
  filter(sqft_lot>(quantile(df$sqft_lot)[2]-(3*IQR)),sqft_lot<(quantile(df$sqft_lot)[2]-(1.5*IQR))) 

df %>% 
  filter(sqft_lot<(quantile(df$sqft_lot)[4]+(3*IQR)),sqft_lot>(quantile(df$sqft_lot)[4]+(1.5*IQR))) %>% 
  nrow()

# Odp: Obserwacji odstajacych dolnych nie ma, a obserwacji odstajacych górnych jest 654.


# 10. Wsród nieruchomosci wskaz jaka jest najwieksza cena za metr kwadratowy biorac pod uwage tylko powierzchnie mieszkalna.
df %>% 
  select(price,sqft_living) %>% 
  summarise(cena_za_metr = price/sqft_living) %>% 
  arrange(cena_za_metr) %>% 
  top_n(1)

# Odp: 810.1389