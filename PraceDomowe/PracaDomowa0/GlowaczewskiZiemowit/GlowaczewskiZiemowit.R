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
tail(mtcars,5)
?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[3,]
mtcars[,4]
rownames(mtcars)
colnames(mtcars)

## Pierwszy wiersz, pierwsza kolumna?

## 10 pierwszych wierszy, 2 i 3 kolumna?
  
head(mtcars[,2:3],10)
mtcars[1:10,2:3]
mtcars[1:10,c(2,5)]
  
## Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]
mtcars[,"am"]
## Jak wybierać jedną kolumnę?

mtcars$am

## Pytania

## 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars)
ncol(mtcars)

## 2. Jakie są typy zmiennych?

class(mtcars)
typeof(mtcars)
str(mtcars)
summary(mtcars)

## 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars$cyl)

## 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars$drat[mtcars$cyl==4])

## Prosty wykres

## Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg,mtcars$hp)

## Zmienna "cyl" - barplot

table(mtcars$cyl)
barplot(table(mtcars$cyl))

# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$name == "John"& employees$surname == "Insecure",]
proton(action = "login", login="johnins")
for (i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins",password=top1000passwords[i])
}
head(logs)
employees[employees$surname=="Pietraszko",]
which.max(table(logs[logs$login=="slap","host"]))
table(logs[logs$login=="slap","host"])[134]
proton(action="server",host="194.29.178.16")
# 5) Umieszczamy na repozytorium rozwiązanie.
