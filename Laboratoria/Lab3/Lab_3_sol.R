#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 3            ###
#########################################

library(dplyr)
install.packages("tidyr")
install.packages("nycflights13")
library(nycflights13)
# Dane - nycflights13 - połączenia tabel
# https://cran.r-project.org/web/packages/dittodb/vignettes/nycflights.html

?flights
dim(flights)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

View(flights2)  

# 1. Mutating joins

# Kontrolowanie sposobu łączenia tabel

# 1)

left_join(x, y, by = NULL)

flights2 %>% 
  left_join(weather)

# 2) 

flights2 %>% 
  left_join(planes, by = "tailnum")

# 3)

# by = c("x" = "a")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

# Typy łączenia tabel

df1 <- tibble(x = c(1, 2), y = 1:2)
df2 <- tibble(x = c(3, 1), a = 10, b = "a")


# a) inner_join() 

df1 %>% 
  inner_join(df2)

# b) left_join()

df1 %>% 
  left_join(df2)

# c) right_join()

df1 %>% 
  right_join(df2)

# d) full_join()

df1 %>% 
  full_join(df2)


## Zadanie 1
# Ile lotów w poszczególnych miesiącach obsługiwała firma "JetBlue Airways"?

flights
airlines

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>% 
  select(month) %>% 
  table()

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>%
  group_by(month) %>% 
  summarise(n = n())

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>% 
  count(month)

# W którym miesiącu jest najwięcej?

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>% 
  count(month) %>% 
  top_n(1)

flights %>% 
  left_join(airlines, by = "carrier") %>% 
  filter(name == "JetBlue Airways") %>% 
  group_by(month) %>% 
  count() %>% 
  arrange(-n)


# Jaka jest średnia liczba odlotów?

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>% 
  count(month) %>% 
  summarise(średnia = mean(n))

flights %>% 
  left_join(airlines) %>% 
  filter(name == "JetBlue Airways") %>%
  group_by(month) %>% 
  summarise(n = n()) %>% 
  summarise(średnia = mean(n))


## Zadanie 2
# W jakim miesiącu było najwięcej odlotów z lotniska "John F Kennedy Intl"?

flights
airports

flights %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  filter(name == "John F Kennedy Intl") %>% 
  count(month) %>% 
  top_n(1)

# 2. Filtering joins

semi_join(x, y)
anti_join()

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

(df1 <- tibble(x = c(1, 1, 3, 4), y = 1:4))
(df2 <- tibble(x = c(1, 1, 2), z = c("a", "b", "a")))

df1 %>% 
  inner_join(df2, by = "x")
df1 %>% 
  semi_join(df2, by = "x")


# 3. Set operations

intersect(x, y) 
union(x, y)
setdiff(x, y)

(df1 <- tibble(x = 1:2, y = c(1L, 1L)))
(df2 <- tibble(x = 1:2, y = 1:2))

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

# 4. Pivoting

library(tidyr)

# a) pivot_longer()
?relig_income

relig_income %>% 
  pivot_longer(!religion, names_to = "income", values_to = "count")


# b) pivot_wider()

?fish_encounters

fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen)

fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)
