library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
df <- read_excel("Naukobus/Naukobus.xlsx", sheet = 1)

# Tradycyjne miejsca nauki (dom, wlasny pokoj, szkola):

trad <- df %>% 
  select(c("P 45a", "P 45e", P10_M, P10_JP, P10B)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,2:4], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"Wysoka",
                                srednia > quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "Przeciętna",
                                srednia <= quantile(srednia, 0.33) ~"Niska")) %>% 
  filter(`P 45a` >= 1 | `P 45e` >= 1) %>% 
  count(przedzialy) %>% 
  mutate(Metoda = 'Tradycyjna')
trad


# Nietradycyjne miejsca nauki (poza domem, muzea, domy kultury, natura):

nietrad <- df %>% 
  select(c("P 45b","P 45c","P 45f", "P 45g" , P10_M, P10_JP, P10B)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,5:7], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"Wysoka",
                                srednia > quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "Przeciętna",
                                srednia <= quantile(srednia, 0.33) ~"Niska")) %>% 
  filter(`P 45b` >= 1 | `P 45c` >= 1 | `P 45f` >= 1 | `P 45g` >= 1) %>% 
  count(przedzialy) %>% 
  mutate(Metoda = 'Nietradycyjna')

# Obie metody:

obie <- df %>% 
  select(c("P 45a", "P 45b","P 45c", "P 45e", "P 45f", "P 45g", P10_M, P10_JP, P10B)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,7:9], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"Wysoka",
                                srednia > quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "Przeciętna",
                                srednia <= quantile(srednia, 0.33) ~"Niska")) %>% 
  filter((`P 45a` >= 1 | `P 45e` >= 1) & (`P 45b` >= 1 | `P 45c` >= 1 | `P 45f` >= 1 | `P 45g` >= 1)) %>% 
  count(przedzialy) %>% 
  mutate(Metoda = 'Obie')

# Porownanie:

x <- rbind(nietrad,trad, obie)

ggplot(x, aes(x = przedzialy, y =n, fill= Metoda)) +
  geom_bar(position="stack", stat="identity") + 
  scale_y_continuous(expand = c(0,0)) +               
  labs(title = "Metody uczenia się:",
       x = "Podział średniej",
       y = "Liczba uczniów",
       caption = "Podział średniej:
                  Wysoka: powyżej 4.33 
                  Przeciętna: 3.33 - 4.33
                  Niska: poniżej 3.33") +
  scale_fill_manual("Metoda", values = c("Nietradycyjna" = "gray80", 
                                         "Obie" = "grey50", 
                                         "Tradycyjna" = "grey20")) +
  theme(plot.background = element_rect(fill = "#306457"), 
        plot.title = element_text(color = "white"), 
        plot.caption = element_text(color = "white"), 
        axis.title.x = element_text(color = "white"), 
        axis.title.y = element_text(color = "white"),
        axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"), 
        legend.background = element_rect(fill = "#306457"), 
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"), 
        panel.background = element_rect(fill = "#306457"))
