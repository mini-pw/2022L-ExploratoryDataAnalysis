proton()
head(employees)
employees[employees$surname == "Insecure", c("login")]

proton(action = "login", login="johnins")

head(top1000passwords)

for(i in 1:1000) {
  proton(action = "login", login="johnins", password = top1000passwords[i])
}

head(logs)
(logs[logs$surname == "Pietraszko", "host"])
sort(table(logs[logs$login == "slap", c("host")]), decreasing = TRUE)[1]

proton(action = "server", host="194.29.178.16")


bash_history
split_bash_history <- strsplit(bash_history, " ")

comnands <- c()
for (x in split_bash_history){
  comands <- c(comands, x[[1]])
}

for(comnad in unique(comands)){
  proton(action = "login", login = "slap", password = comnad)
}






