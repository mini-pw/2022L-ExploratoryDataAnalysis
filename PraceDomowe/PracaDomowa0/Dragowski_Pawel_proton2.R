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
tail(mtcars,10)
?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

## Pierwszy wiersz, pierwsza kolumna?

## 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,3:4],10)
## Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am", "wt", "mpg")]
## Jak wybierać jedną kolumnę?
mtcars[,"am"]

mtcars$am
## Pytania
## 1. Wymiar ramki danych
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
## 2. Jakie są typy zmiennych?
type(mtcars)
typeof(mtcars)
str(mtcars)
## 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
## 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4,'drat'])

## Prosty wykres

## Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)


## Zmienna "cyl" - barplot
table(mtcars$cyl)
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))
# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()


# 5) Umieszczamy na repozytorium rozwiązanie.
head(employees)
#1
employees[employees$name=='John'&employees$surname=='Insecure',"login"]
proton(action = "login", login="johnins")
#2
for (i in 1:length(top1000passwords)) {
  response <-proton(action = "login", login="johnins", password=top1000passwords[i])
if(response=="Success! User is logged in!"){print(top1000passwords[i])
  break}}
#3
log_piet<- employees[employees$surname=='Pietraszko','login']
head(logs)
which.max(table(logs[logs$login==log_piet,'host']))
proton(action = "server", host="194.29.178.16")
