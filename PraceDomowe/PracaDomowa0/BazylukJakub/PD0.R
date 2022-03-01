proton()
head(employees)
employees[employees$name=="John" & employees$surname=="Insecure",]
employees$name=="John"
proton(action = "login", login = "johnins")

for(i in 1:1000){
  proton(action = "login", login = "johnins", password = top1000passwords[i])
}
employees[employees$surname == "Pietraszko",]

(logs[logs$surname == "Pietraszko", "host"])
sort(table(logs[logs$login == "slap", c("host")]),decreasing=TRUE)[1]


proton(action = "server", host = "194.29.178.16")



commands <- c()
for(x in strsplit(bash_history, " ")){
  commands <- c(commands, x[[1]])
}

for(command in unique(commands)){
  proton(action="login", login="slap", password=command)
}

