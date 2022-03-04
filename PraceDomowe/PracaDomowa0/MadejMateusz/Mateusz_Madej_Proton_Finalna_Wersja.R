install.packages("proton")
library(proton)
proton()

data("employees")
data("top1000passwords")
data("logs")
data("bash_history")

# Task 1
log_ins <- employees[employees$surname == "Insecure" & employees$name == "John", "login"]
proton(action = "login", login=log_ins)

# Task 2
for (i in 1:length(top1000passwords)) {
  response <- proton(action = "login", login=log_ins, password=top1000passwords[i])
  if (response == "Success! User is logged in!"){
    pass_ins <- top1000passwords[i]
    print(top1000passwords[i])
    break
  }
}

# Task 3
log_piet <- employees[employees$surname == "Pietraszko", "login"]
which.max(table(logs[logs$login == log_piet, "host"]))
table(logs[logs$login == log_piet, "host"])[134]
proton(action = "server", host="194.29.178.16")

# Task 4
bash_sus <- unique(grep(" ", bash_history, value = TRUE, invert = TRUE))
for (i in 1:length(bash_sus)){
  response <- proton(action = "login", login=log_piet, password=bash_sus[i])
  if (response == "Success! User is logged in!"){
    pass_piet <- bash_sus[i]
    print(pass_piet)
    break
  }
}


