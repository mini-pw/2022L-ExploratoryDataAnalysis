#########################################
###    WSTEP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 1            ###
#########################################

# 0) Prowadzacy.
## Anna Kozak, Katarzyna Woznica
## Kontakt: MS Teams

# 1) Materialy do zajec.
## Repozytorium na GitHub
## https://github.com/MI2-Education/2022L-ExploratoryDataAnalysis


# 2) Jak dziala GitHub?
## Jak zglosic prace domowa/projekt? (fork, pull request)


# 3) Przypomnienie podstaw R.

data(mtcars)
head(mtcars)

?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[3,]
mtcars[,4]
mtcars[3:4,]

## Pierwszy wiersz, pierwsza kolumna?

## 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10,2:3]
mtcars[1:10,c(5,2)]

## Wszystkie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]

## Jak wybierac jedna kolumne?

mtcars[,"am"]
mtcars$am

## Pytania

## 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars)

## 2. Jakie sa typy zmiennych?

str(mtcars)
summary(mtcars)

## 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

unique(mtcars$cyl)

## 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4,"drat"])

## Prosty wykres

## Zaleznosc "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

## Zmienna "cyl" - barplot

table(mtcars$cyl)
barplot(table(mtcars$cyl))

# 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()


# 5) Umieszczamy na repozytorium rozwiazanie.

data(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins")

data(top1000passwords)

for (i in 1:1000) {
  if (proton(action = "login", login="johnins", password=top1000passwords[i]) 
      == "Success! User is logged in!" ) {
    print(i)
    break
  }
  
}
# i = 120
pass <- top1000passwords[i]

data(logs)
log_piet <- employees[employees$surname == 'Pietraszko', 'login']
log_max <- which.max( table(logs[logs$login == log_piet, 'host']) )
table(logs[logs$login == log_piet, 'host'])[log_max]
proton(action = "server", host="194.29.178.16")
