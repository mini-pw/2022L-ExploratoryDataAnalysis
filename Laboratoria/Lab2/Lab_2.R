#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 2            ###
#########################################

install.packages("dplyr")
library(dplyr)

# Dane
starwars

# 0) Krótkie przypomnienie z wykładu - czyli typy danych (dla kolumn c(1, 2, 3, 4, 5, 7, 8))

# Informacje o zbiorze danych

# Podgląd tabeli

# Określamy typy zmiennych:
# name - 
# height - 
# mass -
# hair_color - 
# skin_color - 
# birth_year - 
# sex -

# 1) Wybór wierszy i kolumn w dplyr
# a) wybór kolumn ---> select()

# b) wybór wierszy ---> filter()

# 2) pipes %>% (skrót Ctrl + Shift + m)

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.


# c) sortowanie wierszy ---> arrange()

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

# d) transformacja zmiennych ---> mutate()

# e) transformacja zmiennych ---> transmute()

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

# f) kolejność kolumn ---> relocate()

# g) dyskretyzacja ---> ifelse(), case_when()

# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

# i) grupowanie ---> group_by() + summarise()

# Zadanie 5
# Używając funkcji z pakietu dplyr() sprawdź:
