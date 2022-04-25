install.packages("ggmosaic")
install.packages("showtext")
library("ggmosaic")
library("dplyr")
library("readxl")
library(ggplot2)
library(tidyr)
library(ggplot2)
library(reshape2)
library(naniar)
library(forcats)
df <- readxl::read_excel("Naukobus.xlsx")
library(showtext)
font_add_google("Roboto", "Roboto")


### Wykres oceny od pogladow

## df %>% 
#select(matka_studia = P4, ojciec_studia = P5, P10_M:P10F, szczepienia = D9, 
#      segregacja = D10, woda = D12, globalne_ocieplenie = D15) %>% 
#replace_with_na_all(~.x %in% c(0,77,88,99)) %>% 
#mutate(srednia_ocen = rowMeans(df_poglady[3:8], na.rm = TRUE)) -> df_poglady

df %>% 
  select(matka_studia = P4, ojciec_studia = P5, P10_M:P10F, szczepienia = D9, 
         segregacja = D10, woda = D12, globalne_ocieplenie = D15) %>% 
  mutate(across(matka_studia:globalne_ocieplenie, ~replace(., . > 10 | . < 1, NA))) -> df_poglady

temp2 <- df_poglady %>%
  mutate(srednia_ocen = rowMeans(df_poglady[3:8], na.rm = TRUE)) %>% 
  mutate(dobre_oceny = srednia_ocen >= 4.75) %>% 
  group_by(dobre_oceny) %>% 
  summarise(szczepienia = sum(szczepienia == 1, na.rm = TRUE)/n(),
            segregacja = sum(segregacja == 2, na.rm = TRUE)/n(),
            woda = sum(woda == 1, na.rm = TRUE)/n(),
            globalne_ocieplenie = sum(globalne_ocieplenie == 1, na.rm = TRUE)/n() )

x <-   temp2 %>%
  pivot_longer(!dobre_oceny)

ggplot(x, aes(y=value, x=name, fill = dobre_oceny, width=0.5)) + 
  geom_col(stat='identity',  position = position_dodge(width=0.55)) +
  scale_y_continuous(labels=scales::percent, expand = c(0,0)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Roboto", size = 19), axis.text.x = element_text(size = 19), legend.text = element_text(size = 19)) +
  labs(title = element_blank(), x = "", y = "odsetek dzieci") +
  scale_fill_manual(na.value="#281E15",values = c("#928477","#A49FA3", "red"), name = "pasek na świadectwie", labels = c("tak", "nie", "brak odpowiedzi")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2), labels = c("globalne ocieplenie będzie zagrażać życiu na ziemi", "segregacja odpadów to słuszne działanie",
                              "szczepienia powinny być obowiązkowe dla wszystkich", "woda w bidonie, nie w butelce"))


###### mosaic plot

df %>%
  select(obecnosc_obrazka = 'P 45', P10_M:P10F, studia_mama = P4, studia_tata = P5, mama_praca = P6, tata_praca = P8) %>%
  mutate(across(P10_M:P10F, ~replace(., . > 6 | . < 1, NA)))  -> with_na_2

with_na_2 %>%
  mutate(mean = rowMeans(with_na_2[2:6], na.rm = TRUE), 
         pasek = case_when(mean >= 4.75 ~ "tak",
                           mean < 4.75 ~ "nie",
                           TRUE ~ "brak odpowiedzi")) %>%
  filter(mama_praca < 3, tata_praca < 3) %>%
  mutate(praca = case_when(mama_praca == 1 & tata_praca == 1 ~ "oboje", 
                           mama_praca == 1 & tata_praca == 2 ~ "mama",
                           mama_praca == 2 & tata_praca == 1 ~ "tata",
                           mama_praca == 2 & tata_praca == 2 ~ "nikt")) -> praca_a_srednia

praca_a_srednia$praca <- factor(praca_a_srednia$praca, levels = c("nikt", "mama", "tata", "oboje"))
praca_a_srednia$pasek <- factor(praca_a_srednia$pasek, levels = c("nie", "tak", "brak odpowiedzi"))
#ggplot(data = praca_a_srednia) +
 # geom_mosaic(aes(x = product(praca, pasek), fill = praca)) +
  #theme(legend.position = "NONE") +
  #labs(title = "Pasek na świadectwie w zależności od pracy rodziców", y = "kto pracuje?", x = "pasek na świadectwie") +
  #scale_fill_manual(values = c("#cd9b5a","#cd5abe", "#6a5acd", "#becd5a"))


ggplot(data = praca_a_srednia) +
  geom_mosaic(aes(x = product(praca, pasek), fill = praca)) +
  #scale_fill_brewer(palette = "Oranges") +\\
  labs(title = element_blank(), y = "Kto pracuje?", x = "pasek na świadectwie") +
  scale_fill_manual(values = c("#281E15", "#47423E", "#928477", "#A49FA3")) + 
  theme_bw() +
  theme( legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(family="Roboto", size = 15), axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), legend.title = element_text(size = 15))

##############
### violin plots

df %>%
  select('P14', P10_M:P10F) %>%
  mutate(across(P10_M:P10F, ~replace(., . > 6 | . < 1, NA)))  -> with_na_5

with_na_5 %>%
  mutate(mean = rowMeans(with_na_5[2:7], na.rm = TRUE)) %>%
  filter(P14 < 6) %>%
  mutate(ksiazki = case_when(P14 == 1 ~ "nie ma ani jednej",
                             P14 == 2 ~ "kilka (mniej niż 20)",
                             P14 == 3 ~ "dużo (od 20 do 50)",
                             P14 == 4 ~ "bardzo dużo (od 50 do 100)",
                             P14 == 5 ~ "całe mnóstwo (powyżej 100)")) %>%
  mutate(ksiazki = fct_reorder(ksiazki, P14))-> df2

#ggplot(df2, aes(x = factor(ksiazki), y = mean)) +
 # geom_violin(color = "slateblue3", fill="slateblue1") +
  #geom_boxplot(width=0.1, fill = "gray80") +
  #labs(title = "Średnia ocen w zależności od ilości książek w domach dzieci",
   #    x = "Ile książek znajduje się w twoim domu?",
  #   y = "średnia ocen") +
  #scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6))


ggplot(df2, aes(x = factor(ksiazki), y = mean)) +
  geom_violin(color = "#B7B2AC", fill="#B7B2AC") +
  geom_boxplot(width=0.2, fill = "#928477", outlier.colour="#928477") +
  labs(title = element_blank(),
       x = "Ile książek znajduje się w twoim domu?",
       y = "średnia ocen") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="Roboto", size = 17))

