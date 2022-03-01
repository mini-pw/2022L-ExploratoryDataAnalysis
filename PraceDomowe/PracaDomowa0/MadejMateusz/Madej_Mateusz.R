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
head(mtcars, 10)
tail(mtcars)
mtcars[3,]
mtcars[,4]
mtcars[1,1]
rownames(mtcars) #sprawdzenie nazw wierszy
colnames(mtcars)




?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

## Pierwszy wiersz, pierwsza kolumna?

## 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,2:3],10)
mtcars[1:10,2:3]
mtcars[1:10, c(2,5)]

## Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am", "wt", "mpg")]


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
str(mtcars)         #najlepsze?
summary(mtcars)


## 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)


## 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4,"drat"])


## Prosty wykres



## Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)



## Zmienna "cyl" - barplot
table(mtcars$cyl)
barplot(table(mtcars$cyl))


# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()

data(employees)
log_insecure <-employees[employees$surname == "Insecure"& employees$name == "John","login"]
proton(action = "login", login=log_insecure)
data("top1000passwords")
head(top1000passwords)
head(top1000passwords)[6]
for (i in 1:length(top1000passwords)){
  response <- proton(action = "login", login=log_insecure, password=top1000passwords[i])
  print(i)
  if (response == "Success! User is logged in!"){
    print(top1000passwords[i])
    break
  }
}
data(logs)
log_piet <- employees[employees$surname == "Pietraszko","login"]
which.max(table(logs[logs$login == log_piet, "host"]))
table(logs[logs$login == log_piet, "host"])[134]
proton(action = "server", host="194.29.178.16")

# 5) Umieszczamy na repozytorium rozwiązanie.