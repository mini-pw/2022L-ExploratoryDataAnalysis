library(dplyr)

df <- read.csv("house_data.csv")
View(df)


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
df %>% 
  summarise(mediana = median(condition))
#mediana wynosi 3

df %>% 
  filter(waterfront == 1) %>% 
  filter(condition >= 3) %>% 
  summarise(srednia = mean(price))

# Odp:1662564


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?
df %>% 
  group_by(floors) %>% 
  summarise(mediana = median(bathrooms))

# Odp:Oba maj¹ medianê ³azienek równ¹ 2,5


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?
#policzê od œredniej wartoœci d³ugoœci i szerokoœci geograficznej
df %>% 
  summarise(szerokosc = mean(lat))
#œrednia szerokoœæ geograficzna to 47.56005

df %>% 
  summarise(dlugosc = mean(long))
#œrednia d³ugoœæ geograficzna to -122.2139

df %>% 
  filter(lat > 47.56005) %>% 
  filter(long < -122.2139) %>% 
  summarise(ilosc = n())
#na pó³nocnym zachodzie jest 6871 nieruchomoœci
df %>% 
  filter(lat < 47.56005) %>% 
  filter(long > -122.2139) %>% 
  summarise(ilosc = n())
#a na po³udniowym wschodzie 5299
100*(6871-5299)/5299

# Odp:O 29.66597% wiêcej


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?
df %>% 
  mutate(dekada = yr_built %/% 10) %>% 
  group_by(dekada) %>% 
  summarize(mediana = median(bathrooms)) %>% 
  tail()

# Odp:Nie zmienia³a siê, zarówno w latach 90 jak i w latach 00, a nawet w latach 10 wynosi³a 2,5


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?
df %>% 
  filter(lat > 47.56005) %>% 
  filter(waterfront == 1) %>% 
  summarize(Q = quantile(condition))
#w przypadku nadwodnym jest to 3 i 4

df %>% 
  filter(lat > 47.56005) %>% 
  filter(waterfront == 0) %>% 
  summarize(Q = quantile(condition))
#w przypadku bezwodnym bezwodnym równie¿ 3 i 4

# Odp:Niezale¿nie od obecnoœci wody, dolny kwantyl ma wartoœæ 3, zaœ górny 4


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?
df %>% 
  group_by(zipcode) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc)
#ten kod to 98103
df %>% 
  filter(zipcode == 98103) %>% 
  summarise(Q = quantile(price))

695000-432125


# Odp:Najpopularniejszy kod pocztowy to 98103, a rozstêp miêdzykwartylowy ceny tamtejszych nieruchomoœci to 262875


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?
df %>% 
  summarise(ilosc = n())
#jest 21613 nieruchomoœci

df %>% 
  filter(sqft_living < sqft_living15) %>% 
  summarise(ilosc = n())
#9206 jest mniejszych od œredniej swoich s¹siadów
9206*100/21613

# Odp:42.59473% nieruchomoœci ma mniejsz¹ œr powierzchniê ni¿ 15 najbli¿szych s¹siadów


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?
df %>% 
  summarise(Q = quantile(price))
#trzeci kwartyl ceny to 645000
df %>% 
  arrange(-yr_renovated) %>% 
  select(yr_renovated) %>% 
  head()

df %>% 
  arrange(-yr_built) %>% 
  select(yr_built) %>% 
  head()

#ostatnie dane s¹ z 2015, wiêc za³ó¿my, ¿e remont wci¹gu ostatnich 10 lat znaczy od 2005 wzwy¿

df %>% 
  filter(yr_built > 1970) %>% 
  filter(yr_renovated > 2005) %>% 
  filter(price > 645000) %>% 
  summarise(srednia = mean(bedrooms))

# Odp: Oko³o 3,9 wynosi œrednia liczba sypialni


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).
df %>% 
  summarise(Q = quantile(sqft_living))
2550-1427
#dolny kwartyl to 1427, a dolny to 2550. Rozstêp miêdzy kwartylowy to 1123.
2550+1.5*1123
2550+3*1123
1427-1.5*1123
1427-3*1123

#górne obserwacje odstaj¹ce to te z zakresu (4234.5, 5919), a dolne to te z zakresu(-1942, -257.5), jednak jako, ¿e ujemna powierzchnia nie ma senu, mo¿na ³atwo stwierdziæ, ¿e dolnych obserwacji odstaj¹cych nie ma
df %>% 
  filter(sqft_living > 4234.5) %>% 
  filter(sqft_living < 5919) %>% 
  summarise(ilosc = n())

# Odp: s¹ 498 obserwacje odstaj¹ce górne i nie ma obserwacji odstaj¹cych dolnych


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.
df %>% 
  mutate(metry = sqft_living*0.09290304) %>% 
  mutate(czm = price/metry) %>% 
  arrange(-czm) %>% 
  head()

# Odp:8720,26 $