library(proton)
proton()
zad1 = employees
zad1= zad1[zad1$name =='John',]
zad1x = zad1x[zad1x$surname =='Pietraszko',]

zad2 = top1000passwords
for (password in zad2){
  pass = password
  proton(action = 'login', login = 'johnins', password= pass)
}

zad3 = logs
zad3 = zad3[zad3$login == 'slap',]
hosts = table(zad3$host)
sort(hosts)
tail(hosts)
proton(action = "server", host="194.29.178.16")
commands = bash_history


commands2 = strsplit(commands," ")
commands2
commands2[[1]][1]
for(line in commands2){
  
  proton(action = 'login', login = 'slap', password = commands2[[1]][line])
}
