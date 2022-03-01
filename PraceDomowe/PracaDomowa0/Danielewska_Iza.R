library(proton)
proton()
data("employees")
login <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login=login)
table(top1000passwords)
for (i in 1:length(top1000passwords)) {
  print(i)
  if (proton(action = "login", login=login, password=top1000passwords[i])=="Success! User is logged in!") {
    print(top1000passwords[i])
    break
  }
} 

log_piet <- employees[employees$surname == "Pietraszko","login"]
head(logs)
which.max(table(logs[logs$login ==log_piet,"host"]))
table(logs[logs$login ==log_piet,"host"])[134]
proton(action = "server", host="194.29.178.16")

bash_history
