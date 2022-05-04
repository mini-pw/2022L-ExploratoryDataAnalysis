library(dplyr)

df <- read.csv("E:/RStudio/eskplo/house_data.csv")
str(df)



# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?

mediana = median(df$grade)

df %>%
  select(grade, waterfront, price) %>%
  filter(grade >= mediana) %>%
  filter(waterfront == 1) %>%
  summarise(mean(price))

# Odp: 1784152




# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

df %>%
  select(bathrooms, floors) %>%
  filter(floors == 2 | floors == 3) %>%
  group_by(floors) %>%
  summarise(median(bathrooms)) 

# Odp: Nie (o dziwo obie te mediany wysz³y akurat 2.5).




# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

# Tu przyda siê komentarz definiuj¹cy, co to znaczy "po³udniowy wschód" i 
# "pó³nocny zachód":
# Przyjrza³em siê wartoœciom szerokoœci i d³ugoœci geograficznej z ramki i 
# z zasiêgu przyjmowanego przez te wartoœci (który dosta³em po prostu klikaj¹c
# by RStudio posortowa³o mi je rosn¹co/malej¹co), przy u¿yciu konwertera 
# wspó³rzêdnych geograficznych do lokacji, ustali³em, ¿e dane dotycz¹
# nieruchomoœci w okolicach Seattle-USA.
# W zwi¹zku z tym, naturaln¹ definicj¹ "le¿enia na pó³nocny zachód" jest le¿enie
# na pó³nocny zachód wzglêdem centrum miasta. Analogicznie zdefiniowa³em le¿enie
# na po³udniowy wschód.
# Z tego samego konwertera odczyta³em wspó³rzêdne geograficzne centrum miasta i
# po zaokr¹gleniu (nie wp³ywaj¹cym na ostateczne wyniki, bo dane z ramki s¹
# zaokr¹glone jeszcze bardziej grubo) dosta³em wartoœci:

lat_center = 47.62184 
long_center = -122.3509

# Jako, ¿e zadanie bez tej definicji jest nieœcis³e, proszê uprzejmie o nie
# ucinanie punktów, jeœli moja interpretacja polecenia rozmija siê z wizj¹
# uk³adaj¹cego zadanie.

df %>%
  select(lat,long) %>%
  mutate("direction" = (lat>lat_center) + 2*(long>long_center)) %>%
  group_by(direction) %>%
  summarise(n()) -> dir_info

# Ramka dir_info zawiera teraz informacje o liczbie nieruchomoœci w konkretnych
# æwiartkach uk³adu wspó³rzêdnych, zakodowane w nastêpuj¹cy sposób:
# 0 <- SW (Bo lat<=lat_center = S i long<=long_center = W)
# 1 <- NW (Bo lat>lat_center = N i long<=long_center = W)
# 2 <- SE (Bo lat<=lat_center = S i long>long_center = E)
# 3 <- NE (Bo lat>lat_center = N i long>long_center = E)

# OdpowiedŸ na pytanie mo¿na wiêc wyczytaæ z tabelki:
result = (dir_info[[2,2]]/dir_info[[3,2]] - 1)*100

# Odp: Wcale nie jest wiêcej, jest mniej i to o ok 82.5%.




# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?

df %>%
  select(yr_built, bathrooms) %>%
  filter(yr_built >= 1990 & yr_built <= 2000) %>%
  group_by(yr_built) %>%
  summarise(median(bathrooms))

# Odp: Nie zmienia³a siê (i ca³y czas by³a równa 2.5, te dane s¹ jakieœ dziwne).




# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

# Definicja "po³o¿enia na pó³nocy" analogicznie do zadania nr.3.
?quantile()

df %>%
  select(grade, waterfront, lat) %>%
  filter(lat > lat_center) %>%
  group_by(waterfront) %>%
  summarise(quantile(grade, prob = c(0.25,0.75)))

# Poniewa¿ kwartyl 0.75 na pewno jest wiêkszy ni¿ kwartyl 0.25, ³atwo odczytaæ
# odpowiedŸ z powy¿szej tabelki.

# Odp: Dla nieruchomoœci bez widoku na wodê, wynios³y one odpowiednio 7 i 8, podczas gdy dla nieruchomoœci po³o¿onych nad wod¹ by³y one wy¿sze i wynios³y odpowiednio 8 i 11.




# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

df %>%
  select(zipcode, price) %>%
  group_by(zipcode) %>%
  mutate("n" = n()) %>%
  arrange(-n) %>%     # Na tym etapie wiem ju¿ jaki kod wyst¹pi³ najwiêcej razy.
  filter(zipcode == 98103) %>%
  summarise(quantile(price, prob = 0.75) - quantile(price, prob = 0.25))
  
# Odp: Tym kodem jest 98103, a rozstêp miêdzykwartylowy ich cen to 262875 (zapewne dolarów).




# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?

df %>%
  select(sqft_living, sqft_living15) %>%
  filter(sqft_living15 > sqft_living) %>%
  summarise(n())

# Tê w³asnoœæ posiada 9206 nieruchomoœci, a wszystkich nieruchomoœci jest 21613,
# st¹d szukana odpowiedŸ to:

percentage7 = (9206/21613)*100

# Odp: oko³o 42.6%




# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

# Zgodnie z wytycznymi otrzymanymi na teamsie - 2015 (rok na którym siê urywaj¹
# dane) jest dobrym rokiem odniesienia, interesuj¹ nas tylko remonty, œwie¿e
# budowy nie maj¹ znaczenia, ³azienki siê nie licz¹, a wynikiem powinna byæ
# tabelka: liczba pokoi - liczba nieruchomoœci spe³niaj¹cych kryteria.

prog_cenowy = quantile(df$price, prob = 0.75)

df %>%
  select(bedrooms, bathrooms, price, yr_built, yr_renovated) %>%
  filter(price > prog_cenowy) %>%
  filter(yr_renovated > 2005) %>%
  filter(yr_built > 1970) %>%
  group_by(bedrooms) %>%
  summarise(n())

# Odp: Wœród nieruchomoœci spe³niaj¹cych kryteria jest 7 3-pokojowych, 9 4-pokojowych i 5 5-pokojowych.




# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

q1 = quantile(df$sqft_living, prob = 0.25)
q3 = quantile(df$sqft_living, prob = 0.75)
iqr = q3-q1
dolna_gr = q1 - 1.5*iqr
gorna_gr = q3 + 1.5*iqr
# Jak widaæ, dolna granica nie ma sensu, bo raczej nie ma co oczekiwaæ 
# nieruchomoœci o ujemnej powierzchni, dlatego nale¿y jedynie zobaczyæ, ile
# wyników przekracza granicê górn¹.

df %>%
  select(sqft_living) %>%
  filter(sqft_living > gorna_gr) %>%
  summarise(n())

# Odp: Wartoœci odstaj¹ce to te nieruchomoœci, których powierzchnia przekracza 4234 ft^2. Takich nieruchomoœci jest 572.




# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

# 1m^2 ~ 10.764 ft^2

df %>%
  select(sqft_living, price) %>%
  mutate("cena_za_metrkw" = price/(sqft_living/10.764)) %>%
  arrange(-cena_za_metrkw) %>%
  head(1)

# Odp: Najdro¿sza nieruchomoœæ w przeliczeniu na koszt m^2 kosztowa³a ok. 8720 dolarów za metr kw..