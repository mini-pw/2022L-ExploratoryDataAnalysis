library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest œrednia cena nieruchomoœci po³o¿onych nad wod¹, których jakoœ wykoñczenia jest równa lub wiêksza od mediany jakoœci wykoñczenia?
df %>% 
  filter(waterfront == 1) %>% 
  filter(grade >= median(grade)) %>% 
  summarise(œrednia_cena = mean(price))
  

# Odp: 2302236


# 2. Czy nieruchomoœci o 2 piêtrach maj¹ wiêksz¹ (w oparciu o wartoœci mediany) liczbê ³azienek ni¿ nieruchomoœci o 3 piêtrach?

mediana2 <- df %>% 
              filter(floors==2) %>% 
              summarise(mediana2 = median(bathrooms))
mediana3 <- df %>% 
              filter(floors==3) %>% 
              summarise(mediana3 = median(bathrooms))
mediana2>mediana3

# Odp: Nie maj¹ wiêkszej, maj¹ równ¹


# 3. O ile procent wiêcej jest nieruchomoœci le¿cych na pó³nocy zachód ni¿  nieruchomoœci le¿¹cych na po³udniowy wschód?

num_NW <- df %>% 
            filter(lat>=mean(lat) & long<=mean(long)) %>%
            summarise(num_NW = n())

num_SE <- df %>% 
            filter(lat<=mean(lat) & long>=mean(long)) %>% 
            summarise(num_SE = n())

(num_NW-num_SE)/(num_NW+num_SE)*100
# Odp: 12.91701


# 4. Jak zmienia³a siê (mediana) liczba ³azienek dla nieruchomoœci wybudownych w latach 90 XX wieku wzglêdem nieruchmoœci wybudowanych roku 2000?

df %>% 
  filter(yr_built>=1990 & yr_built<2010) %>%
  select(yr_built, bathrooms) %>% 
  group_by(yr_built) %>% 
  summarise(bathrooms_median = median(bathrooms)) %>% 
  arrange(yr_built)




# Odp: Jest sta³a


# 5. Jak wygl¹da wartoœæ kwartyla 0.25 oraz 0.75 jakoœci wykoñczenia nieruchomoœci po³o¿onych na pó³nocy bior¹c pod uwagê czy ma ona widok na wodê czy nie ma?

df %>% 
  filter(lat>=mean(lat)) %>% 
  group_by(waterfront) %>% 
  summarise(quantiles_count = quantile(grade, c(0.25, 0.75)), quantiles = c(0.25, 0.75))

# Odp: Dla po³o¿onych nad wod¹ 0.25 - 8, 0.75 - 11, a dla nie nad wod¹ 0,25- 7, 0,75 - 8


# 6. Pod którym kodem pocztowy jest po³o¿onych najwiêcej nieruchomoœci i jaki jest rozstêp miedzykwartylowy dla ceny nieruchomoœci po³o¿onych pod tym adresem?

top_zipcode <- df %>% 
  group_by(zipcode) %>% 
  summarise(ilosc = n()) %>% 
  arrange(desc(ilosc)) %>% 
  top_n(1)

quantiles_price <- df %>% 
  filter(zipcode == top_zipcode$zipcode) %>% 
  summarise(quantiles_count = quantile(price, c(0.25, 0.75)), quantiles = c(0.25, 0.75)) %>% 
  arrange(quantiles)

quantiles_price[2,"quantiles_count"]-quantiles_price[1,"quantiles_count"]
# Odp: 262875


# 7. Ile procent nieruchomoœci ma wy¿sz¹ œredni¹ powierzchniê 15 najbli¿szych s¹siadów wzglêdem swojej powierzchni?
liczba_wyzsza_srednia15 <- df %>% 
                              filter(sqft_living<sqft_living15) %>% 
                              summarise(liczba = n())
liczba_wszystkich <- df %>% 
                        summarise(liczba = n())
(liczba_wyzsza_srednia15)/(liczba_wszystkich)*100


# Odp: 42.59473


# 8. Jak¹ liczbê pokoi maj¹ nieruchomoœci, których cena jest wiêksza ni¿ trzeci kwartyl oraz mia³y remont w ostatnich 10 latach (pamietaj¹c ¿e nie wiemy kiedy by³y zbierane dne) oraz zosta³y zbudowane po 1970?

df %>% 
  filter(price>quantile(price, 0.75) & yr_renovated >= 2012 & yr_built > 1970) %>% 
  select(id, bedrooms)
# Odp:3, 4 albo 5


# 9. Patrz¹c na definicjê wartoœci odstaj¹cych wed³ug Tukeya (wykres boxplot) wska¿ ile jest wartoœci odstaj¹cych wzglêdem powierzchni nieruchomoœci(dolna i górna granica wartoœci odstajacej).

df %>% 
  filter(sqft_living < (quantile(sqft_living, 0.25)-1.5*(quantile(sqft_living, 0.75)-quantile(sqft_living, 0.25))) | sqft_living > (quantile(sqft_living, 0.75)+1.5*(quantile(sqft_living, 0.75)-quantile(sqft_living, 0.25)))) %>% 
  summarise(ilosc = n())
# Odp: 572


# 10. Wœród nieruchomoœci wska¿ jaka jest najwiêksz¹ cena za metr kwadratowy bior¹c pod uwagê tylko powierzchniê mieszkaln¹.

df %>% 
  summarise(cena_za_m2 = price/(sqft_living*0.093)) %>% 
  arrange(desc(cena_za_m2)) %>% 
  top_n(1)
# Odp: 8711.171
