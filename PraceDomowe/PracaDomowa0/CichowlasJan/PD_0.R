# Przygotowanie
library(proton)
proton()

# Problem 1
log_ins <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login = log_ins)

# Problem 2
for(i in 1:length(top1000passwords)){
  response <- proton(action = "login", login = log_ins, password = top1000passwords[i])
  if(response == "Success! User is logged in!"){
    print(top1000passwords[i])
    break
  }
}

# Problem 3
log_piet <- employees[employees$surname == "Pietraszko", "login"]
host_piet <- names(which.max(table(logs[logs$login == log_piet, "host"])))
proton(action = "server", host = host_piet)

# Problem 4
commands <- strsplit(bash_history, " .+")
unique(commands)
## "DHbb7QXppuHnaXGN" wygląda na hasło
proton(action = "login", login = log_piet, password = "DHbb7QXppuHnaXGN")
