library(dplyr)

df <- read.csv("house_data.cs")

# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
house_data %>%
  filter(waterfront == 1 & grade >= median(grade))%>%
  summarize(mean_prize = mean(price))%>%
  View()


# Odp:1784152


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
num_ofbaths_on2 <- house_data%>%
  filter(floors == 2)%>%
  summarize(median_baths2 =median(bathrooms, na.rm = TRUE))%>%
  View()
num_ofbaths_on3 <- house_data%>%
  filter(floors == 3)%>%
  summarize(median_baths3 =median(bathrooms, na.rm = TRUE))%>%
  View()
# Odp: nie, oba maj� tak� sam� median� r�wn� 2.5


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?
on_north_west <- house_data %>%
  #wyznczam miejscowosc srednia i poronwuje zgodnie z kierunkiem, czyli polnoc:szerko�� > mediana(szerokosc), zachod: dlugosc<mediana(dlugosc)
  filter(lat > median(lat) & long < median(long))%>%
  summarize(n = n())
#analogicznie
on_south_east <- house_data%>%
  filter(lat< median(lat) & long > median(long))%>%
  summarize(n=n())
(abs(on_north_west - on_south_east)/on_south_east )* 100
# Odp: o oko�o 0.15%


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
house_data%>%
  filter(yr_built >= 1990 & yr_built <= 1999)%>%
  summarize(median_baths_90 = median(bathrooms))
#2.5
house_data%>%
  filter(yr_built == 2000)%>%
  summarize(median_baths_2000 = median(bathrooms))

# Odp: nie zmieni�a si�, w obu przypadkach wynosi 2.5


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
#z woda
quantil_withwater <- house_data%>%
  filter(waterfront ==1 & lat> median(lat))%>%
  summarize(quantil_first = quantile(grade,0.25, na.rm = TRUE),quantil_third = quantile(grade,0.75, na.rm = TRUE))
quantil_withwater[,] #8 ,11
#bez wody
quantil_withoutwater <- house_data%>%
  filter(waterfront == 0 & lat> median(lat))%>%
  summarize(quantil_first = quantile(grade,0.25, na.rm = TRUE),quantil_third = quantile(grade,0.75, na.rm = TRUE))
quantil_withoutwater[,] #7, 8
# Odp: dla widoku z woda: 0.25 - 8, 0.75 - 11, a bez wody odpowiednio 7 i 8


# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?
pom <- house_data %>%
  count(zipcode)%>%
  top_n(1)
pom[1,1]
house_data %>%
  filter(zipcode == 98103)%>%
  summarize(IQR_for_postcode = IQR(price,na.rm = TRUE))

# Odp: Najwi�cej nieruchomo�ci znjaduje si� pod kodem 98103 a IQR wynosi 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
neighbours15 <- house_data%>%
  filter(sqft_lot15 > sqft_lot)%>%
  summarize(n = n())
neighbours15[1,1]
#liczba nieruchmosci o sr powierzchnia 15 sasiadow > sr powierzchni:8540
dim(house_data) #21613    
(neighbours15[1,1]/dim(house_data))*100
# Odp: o oko�o 40%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?
house_data%>%
  arrange(desc(yr_renovated))%>%
  head(1)%>%
  View()
#ostatni remont w 2015
house_data%>%
  filter(price > quantile(price,0.75, na.rm = TRUE),yr_renovated >= 2005, yr_built>1970)%>%
  count(bedrooms) %>% 
  View()
  
# Odp: Na te nieruchmo�ci sk�adaj� si� 7 mieszka� 3 pokojowych, 10 4-pokojowych i 6 5-pokojowych


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
house_data %>% 
  filter(sqft_lot > (quantile(sqft_lot,0.75,na.rm = TRUE)+ 1.5*IQR(sqft_lot)) | 
           sqft_lot < (quantile(sqft_lot,0.25,na.rm = TRUE)- 1.5* (IQR(sqft_lot)))) %>% 
  summarise(n = n())

# Odp:2425


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.

#W terminologii metrycznej stop� kwadratow� okre�la si� jako kwadrat o d�ugo�ci bok�w wynosz�cej 0,3048 metra
#Jedna stopa kwadratowa jest r�wna 0,09290304 metra kwadratowego
house_data %>% 
  mutate(prize_perm2 = price / (sqft_living/0.09290304))%>%
  summarize(max(prize_perm2))
  
# Odp:  75.3