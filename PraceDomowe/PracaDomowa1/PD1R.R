library(dplyr)

domy <- read.csv("house_data.csv")

# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
domy %>% 
  filter(grade>=median(grade), waterfront==1) %>% 
  summarise(srednia = mean(price)) 
# Odp: 1784152

# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

domy %>% 
  filter(floors==2) %>% 
  summarise(srenia_liczba_lazienek1 = median(bathrooms)) 
domy %>% 
  filter(floors==3) %>% 
  summarise(srenia_liczba_lazienek2 = median(bathrooms))
# Odp: Nie, w obu przypadkach mediana wynosi 2.5

# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

#Nie wiedzia³em w stosunku do czego maj¹ le¿eæ na zachód, czy wschód itd wiêc korzystaj¹c z najwiêkszych i najmniejszych wartoœci po³o¿enia geograficznego,
#wyznaczy³em "kwadrat", robi¹c œrednie ze skrajnych po³o¿eñ wyznaczy³em jego œrodek i wszystko co by³o w lewej górnej æwiartce uzna³em za po³o¿one na
#pó³nocny zachód, a w prawej dolnej za le¿¹ce na po³udniowy wschód. 

ile_1<-domy %>% 
   filter(long<((max(long)+min(long))/2), lat >(max(lat)+min(lat))/2) %>%
   nrow()
ile_2<-domy %>% 
  filter(long>(max(long)+min(long))/2, lat < (max(lat)+min(lat))/2) %>%
  nrow()
odp = (ile_1/ile_2)*100 

# Odp: Nieruchomoœci le¿cych na pó³nocy zachód jest o 18443.53% ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód.

# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych w/po(<-nie by³o sprecyzowane)) roku 2000?

domy %>% 
  filter(yr_built>=1990, yr_built<2000) %>%
  summarise(med_lazienek1=median(bathrooms)) 
domy %>% 
  filter(yr_built==2000) %>% 
  summarise(med_lazienek2=median(bathrooms)) 
domy %>% 
  filter(yr_built>=2000) %>% 
  summarise(med_lazienek3=median(bathrooms))

# Odp: Mediana nie zmieni³a siê zarówno dala wybudowanych w 2000 roku jak i po 2000 roku. W ka¿dym przypadku mediana wynosi 2.5

# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

df1<-domy %>% 
  filter(lat >(max(lat)+min(lat))/2, waterfront==1) %>% 
  arrange(grade)
quantile(df1$grade)

df2<-domy %>% 
  filter(lat >(max(lat)+min(lat))/2, waterfront==0) %>% 
  arrange(grade)
quantile(df2$grade)

# Odp: Dla domów z widokiem na wodê kwartyl 0.25 wynosi 8, a kwartyl 0.75 wynosi 10,
#natomiast dla domóW bez widoku na wodê kwartyl 0.25 wynosi 7, a kwartyl 0.75 wynosi 8

# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

nie_pokazuj_sie_w_konsoli<-domy %>% 
  group_by(zipcode) %>% 
  summarise(kod=n()) %>%
  arrange(-kod)

kody<-domy %>% 
  filter(zipcode==98103)
rozstep<-quantile(kody$price)[4]-quantile(kody$price)[2]

# Odp: Najpopularniejszym kodem pocztowym jest 98103, a rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem jest równy 262875

# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

zmienna<-domy %>% 
  filter(sqft_lot15>sqft_lot)  
Odp7<-(nrow(zmienna)/nrow(domy))*100
  
# Odp: 39.51326 procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni.

# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dane) oraz zosta³y zbudowane po 1970?

domy %>% 
  filter(price>quantile(domy$price)[4],yr_renovated>=2012,yr_built>1970) %>% 
  summarise(liczba=mean(bedrooms))

# Odp: Nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl, mia³y remont w ostatnich 10 latach oraz zosta³y zbudowane po 1970 maj¹ œrednio 3.6 pokoi.

# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

IQR<-(quantile(domy$sqft_lot)[4] - quantile(domy$sqft_lot)[2])
domy %>% 
  filter(sqft_lot>(quantile(domy$sqft_lot)[2]-(3*IQR)),sqft_lot<(quantile(domy$sqft_lot)[2]-(1.5*IQR))) 
  
domy %>% 
  filter(sqft_lot<(quantile(domy$sqft_lot)[4]+(3*IQR)),sqft_lot>(quantile(domy$sqft_lot)[4]+(1.5*IQR))) %>% 
  nrow()

# Odp: Obserwacji odstaj¹cych dolnych jest 0, a obserwacji odstaj¹cych górncyh jest 654

# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

domy %>% 
  mutate(cena_za_metr=price/sqft_living) %>% 
  summarise(cena_za_metr_max=max(cena_za_metr)) 

# Odp: Uwzglêdniaj¹c tylko powierzchniê mieszkaln¹, najwiêksza cena za metr kwadratowy wynosi 810.1389 .
