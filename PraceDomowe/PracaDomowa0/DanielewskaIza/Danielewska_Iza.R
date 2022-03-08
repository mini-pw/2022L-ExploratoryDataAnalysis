install.packages("proton")
library(proton)
proton()
data("employees")
log_insecure <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login=log_insecure)
for (i in 1:length(top1000passwords)) {
  print(i)
  if (proton(action = "login", login=log_insecure, password=top1000passwords[i])=="Success! User is logged in!") {
    print(top1000passwords[i])
    break
  }
} 

log_piet <- employees[employees$surname == "Pietraszko","login"]
head(logs)
which.max(table(logs[logs$login ==log_piet,"host"]))
table(logs[logs$login ==log_piet,"host"])[134]
proton(action = "server", host="194.29.178.16")

com <- c()
for(x in strsplit(bash_history, " ")){
  com <- c(com, x[[1]])
}

for(com in unique(com)){
  proton(action="login", login="slap", password=com)
}

