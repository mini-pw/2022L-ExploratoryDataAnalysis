#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 2            ###
#########################################

install.packages("dplyr")
library(dplyr)

# Dane
starwars

# 0) Krótkie przypomnienie z wykładu - czyli typy danych (dla kolumn c(1, 2, 3, 4, 5, 7, 8))
starwars[, c(1, 2, 3, 4, 5, 7, 8)]
# Informacje o zbiorze danych
str(starwars[, c(1, 2, 3, 4, 5, 7, 8)])
# Podgląd tabeli
View(starwars[, c(1, 2, 3, 4, 5, 7, 8)])
# Określamy typy zmiennych:
# name - jakościowa, nominalna
# height - ilościowa, ilorazowa
# mass - ilościowa, ilorazowa
# hair_color - jakościowa, nominalna
# skin_color - jakościowa, nominalna
# birth_year - ilościowa, przedziałowa
# sex - jakościowa, kategoryczna 

# 1) Wybór wierszy i kolumn w dplyr
# a) wybór kolumn ---> select()

select(starwars, name)
select(starwars, name, gender, mass)
select(starwars, gender, name, mass)

select(starwars, -name)
select(starwars, -name, -skin_color, -species)

wybierz <- c("name", "gender")
select(starwars, wybierz)
wybierz <- c("name", "gender", "skin_color", "mass1")
select(starwars, wybierz)

wybierz <- c(1,2,3,4)
select(starwars, wybierz)

one_of()
select(starwars, one_of(wybierz))

# b) wybór wierszy ---> filter()

filter(starwars, eye_color == "blue")
filter(starwars, eye_color == "blue" & hair_color == "blond")
filter(starwars, eye_color == "blue", hair_color == "blond")

filter(starwars, eye_color == "blue" | hair_color == "blond")

# 2) pipes %>% (skrót Ctrl + Shift + m)

starwars %>%
  filter(eye_color == "blue") %>% 
  select(name) %>% 
  head()

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.

starwars %>% 
  filter(species == "Droid", height > 100) %>% 
  select(name)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.

starwars %>% 
  filter(is.na(hair_color)) %>% 
  select(name)

# c) sortowanie wierszy ---> arrange()
starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(-height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(desc(height))

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

starwars %>% 
  arrange(desc(mass)) %>% 
  head(1)

starwars %>% 
  top_n(1, mass)

# d) transformacja zmiennych ---> mutate()

starwars %>% 
  mutate(height_m = height/100) %>% 
  View()

# e) transformacja zmiennych ---> transmute()

starwars %>% 
  transmute(height_m = height/100)

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

starwars %>% 
  mutate(height_m = height/100,
         BMI = mass/height_m^2) %>% 
  top_n(1, BMI) %>% 
  select(name)
  
# f) kolejność kolumn ---> relocate()

starwars %>% 
  relocate(sex:homeworld, .before = height)

starwars %>% 
  relocate(sex:homeworld, .after = height)

starwars %>% 
  relocate(where(is.numeric), .after = where(is.character))

# g) dyskretyzacja ---> ifelse(), case_when()

starwars %>% 
  mutate(species_new = ifelse(species == "Human", "Human", "Other")) %>% 
  select(name, species, species_new)

starwars %>% 
  mutate(species_new = case_when(species == "Human" ~ "Human",
                                 species == "Droid" ~ "Droid",
                                 TRUE ~ "Other")) %>% 
  select(name, species, species_new) %>% 
  tail()

# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

summary(starwars$height)

starwars %>% 
  summarise(mean_mass = mean(mass))

starwars %>% 
  summarise(mean_mass = mean(mass, na.rm = TRUE))

starwars %>% 
  filter(hair_color == "blond") %>% 
  summarise(n = n())


# i) grupowanie ---> group_by() + summarise()

starwars %>% 
  group_by(species) %>% 
  summarise(mediana = median(mass, na.rm = TRUE))

starwars %>% 
  group_by(skin_color, eye_color) %>% 
  summarise(n = n())

# Zadanie 5
# Używając funkcji z pakietu dplyr() sprawdź:

# Średnia masa z zależności od koloru oczu?

starwars %>% 
  group_by(eye_color) %>% 
  summarise(mean_mass = mean(mass, na.rm = TRUE))
  
# Kto jest najmłodszy?

starwars %>% 
  top_n(1, -birth_year)

starwars %>% 
  na.omit(birth_year) %>% 
  arrange(birth_year) %>% 
  head(1)

# Jaka jest średnia masa postaci o wzroście ponad 1.5 metra?

starwars %>% 
  mutate(height_m = height/100) %>% 
  filter(height_m > 1.5) %>% 
  summarise(mean_mass = mean(mass, na.rm = TRUE))
