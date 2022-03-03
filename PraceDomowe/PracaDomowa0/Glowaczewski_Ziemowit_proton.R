install.packages("proton")
library(proton)
proton()
#Cz.1
head(employees)
employees[employees$name == "John"& employees$surname == "Insecure",]
proton(action = "login", login="johnins")
#Cz.2
for (i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins",password=top1000passwords[i])
}
#Cz.3
head(logs)
employees[employees$surname=="Pietraszko",]
which.max(table(logs[logs$login=="slap","host"]))
table(logs[logs$login=="slap","host"])[134]
proton(action="server",host="194.29.178.16")
#Cz.4
head(bash_history,10)
pom=gsub(" .*$","",bash_history)
temp=unique(pom)
temp
proton(action="login",login="slap", password="DHbb7QXppuHnaXGN")
