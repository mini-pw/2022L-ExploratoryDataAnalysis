proton()
data(employees)
colnames(employees)
login_ins <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login= login_ins)

for(i in 1:length(top1000passwords)){
  response <- proton(action = "login", login = login_ins, password = top1000passwords[i])
  if(response == 'Success! User is logged in!'){
    print(top1000passwords[i])
    break
  }
}

log_piet <- employees[employees$surname == "Pietraszko", "login"]
head(logs)
which.max(table(logs[logs$login == log_piet, "host"]))

proton(action = "server", host = "194.29.178.16")