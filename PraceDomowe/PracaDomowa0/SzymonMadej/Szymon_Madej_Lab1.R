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

mtcars

head(mtcars, 10)
tail(mtcars, 6)

?mtcars
??mtcars
dim(mtcars)

str(mtcars)
mtcars[2:3,4:5]
mtcars[,1:5]
mtcars[1,1]
mtcars[1:10,2:3]

mtcars[,c('am','wt','mpg')]
mtcars[['mpg']]
mtcars$mpg
#1
dim(mtcars)
#2
str(mtcars)
#3
unique(mtcars$cyl)
#4
mean(mtcars[mtcars$cyl == 4,'drat'])
median(mtcars[mtcars$cyl == 4,'drat'])
  


plot(mtcars$mpg, mtcars$hp)

barplot((mtcars$cyl))
table(mtcars$cyl)
barplot(table(mtcars$cyl))


install.packages("proton")
library(proton)
proton()
df = employees
head(df)
dim(df)[1]
df[df$name == 'John' & df$surname == 'Insecure', ]
  
proton(action = "login", login=df[df$name == 'John' & df$surname == 'Insecure', ][3])


pass = top1000passwords
head(pass)

pass[2]
for(p in pass){
  proton(action = "login", login=df[df$name == 'John' & df$surname == 'Insecure', ][3], password=p)
}

ji = logs[logs$login == 'johnins',]

(table(ji$host))
df
df[df$surname == 'Pietraszko', ]
pass

table(logs[logs$login == 'slap',]$host)
log = as.data.frame(table(logs[logs$login == 'slap',]$host))
logs

log[order(log$Freq),]

proton(action = "server", host="194.29.178.16")



bash_history[1]
strsplit(bash_history, ' +')
