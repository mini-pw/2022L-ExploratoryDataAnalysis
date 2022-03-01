library(proton)
#1
proton()
head(employees)
employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login="johnins")
#2
head(top1000passwords)
top1000passwords

for (i in 1:length(top1000passwords))
  {
  response <- proton(action = "login", login="johnins", password = top1000passwords[i]) 
  if(response == "Success! User is logged in!"){
    print(top1000passwords[i])
    break
  }
}
#3
log_piet<- employees[employees$surname == "Pietraszko", "login"]
head(logs)

max(table(logs[logs$login == log_piet, "host"]))

table(logs[logs$login == log_piet, "host"])[134]

proton(action = "server", host="194.29.178.16")

