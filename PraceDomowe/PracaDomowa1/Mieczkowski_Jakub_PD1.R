library(dplyr)
library(readr)
library(stats)
house_data <- read.csv("Szkola/Kuba/Studia/rstudio/house_data.csv")
View(house_data)

# 1. Jaka jest Œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœæ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
# ja to rozumiem tak ¿e mediana jakoœci wykoñczenia tyczy siê te¿ tych po³o¿onych nad wod¹
(med <- median(house_data[house_data$waterfront == 1, c("grade")]))
house_data %>% 
  filter(waterfront == 1, grade>= med) %>% 
  summarise(srednia = mean(price))
# Odp: 2302236


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?
house_data %>%
  filter(floors ==2 | floors==3) %>% 
  group_by(floors) %>%
  summarise(mediana = median(bathrooms))
  

# Odp: maj¹ tak¹ sam¹ medianê liczby ³azienek = 2.5


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?
house_data %>% 
  summarise(sr_dlugosc= mean(long)) %>% 
  head(1)[1,1] ->sr_dlugosc

house_data %>% 
  summarise(sr_szerokosc = mean(lat)) %>% 
  head(1)[1,1] ->sr_szerokosc

#pó³nocny zachód
house_data %>% 
  filter(long < sr_dlugosc, lat > sr_szerokosc) %>% 
  summarize(ile = n()) %>% 
  head(1)[1,1] -> pnzach
 
#po³udniowy wschód 
house_data %>% 
  filter(long > sr_dlugosc, lat < sr_szerokosc) %>% 
  summarize(ile = n()) %>% 
  head(1)[1,1] -> pdwsch  
(odp <- 100*pnzach/pdwsch - 100)
# Odp: 29.66597


# 4. Jak zmieniaÅ‚a siÄ™ (mediana) liczba Å‚azienek dla nieruchomoÅ›ci wybudownych w latach 90 XX wieku wzglÄ™dem nieruchmoÅ›ci wybudowanych roku 2000?
house_data %>% 
  filter(yr_built>=1990 , yr_built<=1999) %>% 
  summarise(med1 = median(bathrooms, na.rm = TRUE))

house_data %>% 
  filter(yr_built==2000) %>% 
  summarise(med2 = median(bathrooms, na.rm=TRUE))


# Odp: nie zmieni³a siê, wynosi 2,5


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?
house_data %>% 
  filter(lat > sr_szerokosc) %>% 
  group_by(waterfront) %>% 
  summarise(q = quantile(grade, c(0.25,0.75), na.rm = TRUE))
  

# Odp: Q1 nad wod¹: 8, Q3 nad wod¹: 11, Q1 nie nad wod¹: 7, Q3 nie nad wod¹: 8


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?
house_data %>% 
  group_by(zipcode) %>% 
  summarise(ile = n()) %>% 
  arrange(-ile) %>% 
  head(1)[1,1] -> maxkod
house_data %>% 
  filter(zipcode == maxkod) %>% 
  summarise(q = quantile(price, seq(0.25, 0.75, 0.5), na.rm = TRUE)) ->kwantyle
(rozstêp <- kwantyle[2,1] - kwantyle[1,1])
# Odp: Najwiêcej nieruchomoœci jest pod kodem 98103, rozstêp wynosi 262875


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?
#patrzê na powierzchniê mieszkaln¹ bez ogrodu
house_data %>% 
  filter(sqft_living < sqft_living15) %>% 
  summarise(n = n()) %>% 
  head(1)[1,1] ->df
(ileproc<- df/dim(house_data)[1]*100)

# Odp: 42.59473%


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dane) oraz zosta³y zbudowane po 1970?
#jako liczbê pokoi traktuje sumê sypialni i ³azienek bo o innych pokojach nie ma ¿adnych informacji
house_data %>% 
  summarise(q = quantile(price, 0.75, na.rm = TRUE)) %>% 
  head(1)[1,1] -> q3
house_data %>% 
  summarise(m = max(yr_renovated, na.rm=TRUE)) %>% 
  head(1)[1,1] ->ostrok
house_data %>% 
  filter(price > q3, yr_built>1970, yr_renovated>= ostrok-10) %>% 
  mutate(suma_pokoi = bathrooms + bedrooms) %>% 
  summarize(suma = sum(suma_pokoi, na.rm=TRUE))
  
# Odp: 166.75


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).
#dolna: 4Q1 - 3Q3, górna:4Q3 - 3Q1
#rozwa¿am powierzniê z ogrodem
house_data %>% 
  summarise(q = quantile(sqft_lot, seq(0.25, 0.75, 0.5), na.rm = TRUE)) -> kw
Q1 <- kw[1,1]
Q3 <- kw[2,1]
dolna <- 4*Q1 - 3*Q3
górna <- 4*Q3 - 3*Q1
house_data %>% 
  filter(sqft_lot < dolna) %>% 
  summarise(n = n())
  
house_data %>% 
  filter(sqft_lot > górna) %>% 
  summarise(n = n())

# Odp: 0 odstaj¹cych z do³u i 1771 odstaj¹cych z góry


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.
#uwaga: dane s¹ w stopach na metr kwadratowy

house_data %>% 
  mutate(sqm_living = sqft_living*0.093) %>% 
  transmute(cena_metr = price/sqm_living) %>% 
  arrange(-cena_metr) %>% 
  head(1)


# Odp: 8711.171