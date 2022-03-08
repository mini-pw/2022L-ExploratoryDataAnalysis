install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$surname=='Insecure' & employees$name=="John",'login']
proton(action = "login", login = "johnins")
head(top1000passwords)
for (i in 1:length(top1000passwords)) {
  response <-proton(action = "login", login="johnins",
                    password=top1000passwords[i])
  if(response=="Success! User is logged in!")
    {print(top1000passwords[i])
    break}
}  
head(logs)
log_piet <- employees[employees$surname== 'Pietraszko', "login"]
log_piet
head(logs)
which.max(table(logs[logs$login== log_piet,'host']))
table(logs[logs$login== log_piet,'host'])[134]
proton(action= 'server', host="194.29.178.16")
head(bash_history)
unique(grep(pattern = " ", x = bash_history, value = TRUE, invert = TRUE))
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
