#otwieranie bazy danych
library(haven)

dataset = read_sav(path)
path = file.path("C:/Users/Darek/Documents", "R", "ROSES.sav")
dataset <- as.data.frame(dataset)

#SPORTOWCY##
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)

#szukam sportowców na podstawie odpowiedzi na pytanie o tym kim uczeñ chce zostaæ w przysz³osci

#wektor z s³owami zwi¹zanymi ze sportem
sports <- c("SPORT","PI£KA", "SIATK","SZACH","BOKS","ÓWKA","P£YW")

sportsmen <- dataset %>%
  mutate(Upperstr = toupper(Q18_1)) %>% 
  filter((str_detect(Upperstr, sports[1])|(str_detect(Upperstr, sports[2]))
          |str_detect(Upperstr, sports[3])|str_detect(Upperstr, sports[4])
          |str_detect(Upperstr, sports[5])|str_detect(Upperstr, sports[6])
          |str_detect(Upperstr, sports[7]))
         & !(str_detect(Upperstr, "TRANSPORT")) & !(str_detect(Upperstr, "MEDYCYNY"))
         & !(str_detect(Upperstr, "DZIENNIK"))
  ) 


#mamy 58 sportowców

sportsmen <- sportsmen %>% 
  t() %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame()

#sumuje ilu sportowców ma dany stopieñ zaufania wobec naukowców
#z uwzglêdnieniem podzia³u na p³eæ

science_ds_sportsmen <- sportsmen %>% 
  select(c(19,60,61,68:71,77:82,90,92:97,123,131,146:153,155,157)) %>% 
  mutate(P³eæ = ifelse(Q2==1, 'Dziewczynka', 'Ch³opiec')) %>% 
  relocate(where(is.numeric), .after = where(is.character)) %>% 
  select(-2) %>% 
  na.omit(Q11_12) %>%
  t() %>% 
  t() %>% 
  as.data.frame() %>%
  group_by(P³eæ, Q11_12) %>%
  summarise(Count = n()) %>% 
  ungroup() 

## uzupe³nienie ramki danych, dodanie informacji o tym ¿e
#¿adna z dziewczynek-sportowców zaznaczy³a 3 i 4 w pytaniu o naukowców

additional_rows <-data.frame(
  "P³eæ" = c("Dziewczynka","Dziewczynka"),
  "Q11_12" = c(as.character(" 3"),as.character(" 4")),
  "Count"=c(as.numeric(0.1),as.numeric(0.1)))

science_ds_sportsmen<- rbind(science_ds_sportsmen,additional_rows)
science_ds_sportsmen<- as.data.frame(science_ds_sportsmen)
colnames(science_ds_sportsmen) <- c('P³eæ', 'Zaufanie', 'Zliczone' )


#     WYKRESY   #
# Wykres przedstawiajacy zaufanie sportowców w zale¿noœci od p³ci
# gdy s³upek przedstawia wartoœæ 0.1, to oznacza,¿e rzeczywista
# wartoœæ wynosi 0, dziêki temu wartoœci 0 s¹ dobrze widoczne na wykresie

#wykres z podzia³em na p³cie

ggplot(science_ds_sportsmen, aes(x = Zaufanie, y = Zliczone, fill = P³eæ)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = ' Zaufanie do naukowców', y = 'Iloœæ osób', fill = 'P³eæ',
       title = 'Zaufanie sportowców do naukowców')+
  theme_bw()+
  scale_x_discrete(labels = c('Brak', 'Niewielkie', 'Œrednie', 'Wysokie'))+
  scale_y_continuous(expand = expansion(mult =c(0,.02)),
                     limits = c(0, 16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  