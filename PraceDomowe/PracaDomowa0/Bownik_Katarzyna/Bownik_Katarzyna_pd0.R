library(proton)

proton()
data("employees")
login_john <- employees[(employees$name == "John" & employees$surname == "Insecure"), "login"]
proton(action = "login", login = login_john)

data("top1000passwords")
for(i in 1:length(top1000passwords)){
  proton(action = "login", login=login_john, password=top1000passwords[i])
}

data("logs")
login_pietraszko <- employees[employees$surname == "Pietraszko","login"]
a <- table(logs[logs$login == login_pietraszko, "host"])
as.character(data.frame(a)[which.max(data.frame(a)[, "Freq"]), 1])
proton(action = "server", host= as.character(data.frame(a)[which.max(data.frame(a)[, "Freq"]), 1]) )

data("bash_history")
bash_history_unique <- unique(bash_history)
a <- strsplit(bash_history_unique, " ")
for(i in 1:length(bash_history_unique)){
  if(length(a[[i]]) == 1){
    proton(action = "login", login=login_pietraszko, password=bash_history_unique[i])
  }else{
    next
  }
}
