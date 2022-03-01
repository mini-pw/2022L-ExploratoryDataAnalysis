# 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()

JI_Login = employees[(employees$name == "John") & (employees$surname == "Insecure"),c("login")]
top1000passwords
for(i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins",password=top1000passwords[i])
}