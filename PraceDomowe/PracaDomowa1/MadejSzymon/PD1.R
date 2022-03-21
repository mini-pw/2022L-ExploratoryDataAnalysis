library(dplyr)

df <- read.csv("WDED/house_data.csv")
str(df)
View(df)
# 1. Jaka jest średnia cena nieruchomości położonych nad wodą, których jakoś wykończenia jest równa lub większa od mediany jakości wykończenia?
df1 <-df %>%
  filter(grade >= mean(df$grade) & waterfront == 1) %>% 
  summarise(Cena = mean(price))
df1
# Odp: 1959785


# 2. Czy nieruchomości o 2 piętrach mają większą (w oparciu o wartości mediany) liczbę łazienek niż nieruchomości o 3 piętrach?
df2 <- df %>% 
  filter(floors %in% c(2, 3)) %>% 
  group_by(floors) %>% 
  summarise(number_of_barhrooms = median(bathrooms))
  
  

# Odp: NIE


# 3. O ile procent więcej jest nieruchomości leżcych na północy zachód niż  nieruchomości leżących na południowy wschód?
df3 <- df %>% 
  mutate(Posit = ifelse(((lat > (max(df$lat) + min(df$lat))/2) & (long < (max(df$long) + min(df$long))/2)), 'NW',
                      ifelse(((lat < (max(df$lat) + min(df$lat))/2) & (long > (max(df$long) + min(df$long))/2)), 'SE', 'NA'))) %>% 
  group_by(Posit) %>% 
  summarise(Count = n())
((df3[df3$Posit == "NW",2] - df3[df3$Posit == "SE",2]) / df3[df3$Posit == "SE",2]) * 100

# Odp: -99.4578


# 4. Jak zmieniała się (mediana) liczba łazienek dla nieruchomości wybudownych w latach 90 XX wieku względem nieruchmości wybudowanych roku 2000?
df4 <- df %>% 
  filter(yr_built %in% 1990:2000) %>% 
  mutate(NL = ifelse(yr_built == 2000, "2000" , "90s")) %>% 
  group_by(NL) %>% 
  summarise(Srednia_lazienek = median(bathrooms))



# Odp: Nie zmieniła się


# 5. Jak wygląda wartość kwartyla 0.25 oraz 0.75 jakości wykończenia nieruchomości położonych na północy biorąc pod uwagę czy ma ona widok na wodę czy nie ma?
df5 <- df %>% 
  filter((lat > (max(df$long) + min(df$long))/2)) %>% 
  group_by(waterfront) %>% 
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            grade = quantile(grade, c(0.25, 0.5, 0.75)))


# Odp: bez wody 7 to wartość 0.25 kwartyla a 8 to wartość 0.75 kwartyla, a z wodą 8 to wartość 0.25 kwartyla a 10 to wartość 0.75 kwartyla

# 6. Pod którym kodem pocztowy jest położonych najwięcej nieruchomości i jaki jest rozstęp miedzykwartylowy dla ceny nieruchomości położonych pod tym adresem?

df6 <- df %>% 
  group_by(zipcode) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  head(1)

df6a <- df %>% 
  filter(zipcode == 98103) %>% 
  summarise(Q1 = quantile(price, probs = 0.25),
            Q3 = quantile(price, probs = 0.75))

df6a[1,'Q3'] - df6a[1,'Q1']
# Odp: 98103, 262875


# 7. Ile procent nieruchomości ma wyższą średnią powierzchnię 15 najbliższych sąsiadów względem swojej powierzchni?
df7a <- df %>% 
  filter(sqft_living15 > sqft_living) %>% 
  summarise(Count = n())
df7b <- df %>% 
  summarise(Count = n())
df7a/df7b * 100

# Odp: 42.59473%


# 8. Jaką liczbę pokoi mają nieruchomości, których cena jest większa niż trzeci kwartyl oraz miały remont w ostatnich 10 latach (pamietając że nie wiemy kiedy były zbierane dne) oraz zostały zbudowane po 1970?

df8 <- df %>% 
  filter(price >quantile(price, probs = 0.75), yr_renovated > max(yr_renovated) -10, yr_built > 1970) %>% 
  summarise(Suma_pokoi = sum(bedrooms))
# Odp: 82 


# 9. Patrząc na definicję wartości odstających według Tukeya (wykres boxplot) wskaż ile jest wartości odstających względem powierzchni nieruchomości(dolna i górna granica wartości odstajacej).
Q3 = quantile(df$sqft_living, probs = 0.75)
Q1 = quantile(df$sqft_living, probs = 0.25)
IQR = Q3 - Q1
df9 <- df %>% 
  filter(sqft_living >= Q3 + IQR * 1.5 | sqft_living <= Q1 - IQR * 1.5) %>% 
  summarise(Count = n())

# Odp: 572


# 10. Wśród nieruchomości wskaż jaka jest największą cena za metr kwadratowy biorąc pod uwagę tylko powierzchnię mieszkalną.
df10 <- df %>% 
  mutate(price_per_m2 = price / (sqft_living * 0.09290304)) %>% 
  arrange(desc(price_per_m2)) %>% 
  select(price_per_m2) %>% 
  head(1)

# Odp: 8720.262