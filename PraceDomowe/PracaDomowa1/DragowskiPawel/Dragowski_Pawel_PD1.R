library(dplyr)
df <- read.csv("house_data.csv")

str(df)
# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
df %>%
  filter(waterfront==1 & grade>=median(grade))%>%
  summarise(średnia_cena=mean(price))

# Odp: 1784152


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
zad2_1<-df %>%
  filter(floors==2)
zad2_2<- df%>%
  filter(floors==3)
median(zad2_1$bathrooms)>median(zad2_2$bathrooms)
# Odp:NIE


# 3. O ile procent więcej jest nieruchomości leżących na północy zachód niż  nieruchomości leżących na południowy wschód?
zad3_1<-df%>%
  filter(lat>mean(lat) & long<mean(long))%>%count()
zad3_2<-df%>%
  filter(lat<mean(lat) & long>mean(long))%>%count()
(zad3_1-zad3_2)/zad3_2*100

# Odp: 29.66597 %


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
df%>%
  filter(yr_built>=1990 & yr_built<=2000)%>%
  group_by(yr_built)%>%
  summarise(mediana=median(bathrooms))
# Odp: Nie zmieniła się


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
zad5_1<-df%>%
  filter(lat>median(lat) & waterfront==1)
  quantile(zad5_1$grade,c(0.25,0.75))
zad5_2<-df%>%
  filter(lat>median(lat) & waterfront==0)
  quantile(zad5_2$grade,c(0.25,0.75)) 

# Odp: Wartości to 7 i 8 gdy jest widok na morze oraz 8 i 11 gdy tego widoku nie ma


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
zad6_1<-df%>%
  group_by(zipcode)%>%
  count()%>%
  arrange(-n)%>%
  select(zipcode)%>%
  head(1)
zad6_2<-df%>%
  filter(zipcode==zad6_1[[1]])
  IQR(zad6_2$price)
# Odp:Kod pocztowy to 98103 a rostęp to 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
df%>%
  filter(sqft_living15>sqft_living)%>%
  count()/count(df)*100

# Odp:42.59473%



# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dane) oraz zostały zbudowane po 1970?
df%>%
  filter(price>quantile(price,0.75) & yr_renovated>=2012 & yr_built>1970)%>%
  select(bedrooms)
  
  
# Odp: 4 3 3 3 5


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
df%>%
  filter(sqft_living<quantile(sqft_living,0.25)-1.5*IQR(sqft_living)|sqft_living>quantile(sqft_living,0.75)+1.5*IQR(sqft_living))%>%
  count()

# Odp: 572


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
df%>%
  mutate(cena_za_metr=price/sqft_living)%>%
  arrange(-cena_za_metr)%>%
  select(cena_za_metr)%>%
  head(1)

# Odp: 810.1389