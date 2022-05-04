install.packages("proton")
library("proton")
proton()
data("employees")
employees[employees$surname== c("Insecure"),]
test <-proton(action = "login", login="johnins")
data("top1000passwords")
pass <- c()
for (i in 1:length(top1000passwords)){
   proton(action = "login", login="johnins", password=top1000passwords[i])
}
data("logs")       
employees[employees$surname== c("Pietraszko"),]
library("dplyr")
logs %>% filter(login == "slap")%>%group_by(host) %>% summarise(count=n()) %>% arrange(desc(count))
proton(action = "server", host="194.29.178.16")
data("bash_history")
bash_history %>% 
  mutate(bash_history = str_replace(name, "^\\S* ", ""))
install.packages("stringr")
library(stringr)
str_changed <- gsub( " .*$", "", bash_history )
str_uniq <- unique(str_changed)
str_uniq
for (i in 1:length(str_uniq)){
  proton(action = "login", login="slap", password=str_uniq[i])
}
