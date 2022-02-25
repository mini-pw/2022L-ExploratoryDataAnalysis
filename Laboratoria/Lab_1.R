#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 1            ###
#########################################

# 0) Prowadzący.
## Anna Kozak, Katarzyna Woźnica
## Kontakt: MS Teams

# 1) Materiały do zajęć.
## Repozytorium na GitHub
## https://github.com/MI2-Education/2022L-ExploratoryDataAnalysis


# 2) Jak działa GitHub?
## Jak zgłosić pracę domową/projekt? (fork, pull request)


# 3) Przypomnienie podstaw R.

data(mtcars)
head(mtcars)

?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

## Pierwszy wiersz, pierwsza kolumna?

## 10 pierszych wierszy, 2 i 3 kolumna?

## Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?

## Jak wybierać jedną kolumnę?

## Pytania

## 1. Wymiar ramki danych

## 2. Jakie są typy zmiennych?

## 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

## 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?


## Prosty wykres

## Zależność "mpg" i "hp" - scatter plot



## Zmienna "cyl" - barplot


# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()


# 5) Umieszczamy na repozytorium rozwiązanie.