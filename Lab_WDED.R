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
tail(mtcars)
str(mtcars)
?mtcars


## Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[2:3, 4:5]
mtcars[, 1:4]

## Pierwszy wiersz, pierwsza kolumna?

mtcars[1, 1]

## 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]
head(mtcars[, 2:3], 10)

## Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c("am", "wt", "pmg")]

## Jak wybierać jedną kolumnę?

mtcars[["mpg"]]
mtcars$mpg

## Pytania

## 1. Wymiar ramki danych

dim(mtcars)

## 2. Jakie są typy zmiennych?

str(mtcars)

## 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars[["cyl"]])

## 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cy == 4, c("drat")])

## Prosty wykres

## Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

## Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

data(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]

for(i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

head(logs)
data.frame(table(logs$host))
table(logs$login)


 for(i in 1:length(data.frame(table(logs$host))[, 1])){
   if(data.frame(table(logs$host))[i, 2] == max(data.frame(table(logs$host))[, "Freq"])){
     host = data.frame(table(logs$host))[i, 1]
   }
 }

proton(action = "server", host="XYZ")

data.frame(table(logs$login))[data.frame(table(logs$login))$Var1 == "slap", ]
logs[logs$login == "slap", ]

194.29.178.91

# 5) Umieszczamy na repozytorium rozwiązanie.