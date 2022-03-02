library(proton)
proton()
employees
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins")

top1000passwords
for( i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
head(logs)
employees[employees$surname == "Pietraszko",]
logs[logs$login == "slap",]

sort(table(logs[logs$login == "slap", c("host")]), decreasing =TRUE)[1]
proton(action = "server", host="194.29.178.16")

split_bash_history <- strsplit(bash_history, " ")
comands <- c()
for(x in split_bash_history){
  comands <- c(comands, x[[1]])
}
for(comands in unique(comands)){
  proton(action = "login", login ="slap", password = comands)
}
