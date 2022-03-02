table("employees")
proton(action = "login", login="johnins")
data("top1000passwords")

for(i in 1:1000){
  a = proton(action = "login", login="johnins", password=top1000passwords[i])
  }
data("logs")
table("logs")
proton(action = "server", host="194.29.178.13")
