library(proton)
proton()
data(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins")

top1000passwords
for(i in 1:1000){
  x = proton(action = "login", login="johnins",password = top1000passwords[i])
}

employees[employees$surname == "Pietraszko",]
y = logs[logs$login == "slap",]
which.max(unique(table(y$host)))
proton(action = "server", host= "194.29.178.16")

bash_history
for(i in 1:length(bash_history)){
  
}
strsplit()
