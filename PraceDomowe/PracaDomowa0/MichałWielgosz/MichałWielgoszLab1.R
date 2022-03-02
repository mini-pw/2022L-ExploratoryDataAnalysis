#data("mtcars")
#head(mtcars)
#head(mtcars, 10)
# dim(mtcars)
# #str(mtcars)
# mtcars[2:3, 4:5]
# mtcars[1:1]
# mtcars[1:10, 2:3]
# head(mtcars[, 2:3])
# mtcars[, c("mpg", "cyl")]
# mtcars[, c("am","wt","mpg")]
# 1. Wymiary ramki danych
#dim(mtcars)
#2. Jakie typy zmiennych?
#typeof(mtcars)
#str(mtcars)
#3.
#length(unique(mtcars$cyl))
#4.
#mean(mtcars[mtcars$cyl ==4, c("drat")])
#for(i in 1:4){
#  [i]
#}
#plot(mtcars$mpg, mtcars$hp)
#barplot(mtcars$cyl)
#barplot(table(mtcars$cyl))

##########################################################################################################
#install.packages("proton")
library(proton)

#proton()
#johnins employees[employees$surname == "johnins", c("login")]
#slap employees[employees$surname == "Pietraszko", c("login")]
         
#proton(action = "login", login="johnins")
#for(i in 1:1000){
#proton(action = "login", login="johnins", password=top1000passwords[i])   }

#table(logs[logs$login == "slap", c("host")])
# 194.29.178.16 
# proton(action = "server", host="194.29.178.16")
 # for(i in 1:19913){
 #  proton(action = "login", login="slap", password=(strsplit(bash_history[i]," "))[[1]][[1]])
 # }

