library(readxl)
library(dplyr)
library(ggplot2)
df <- read_excel("Naukobus/Naukobus.xlsx")

oceny <- df %>%
  select(c(id_ucznia,P10_M, P10_JP, P10B, D20:D22)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,2:4], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"Wysoka",
                                srednia > quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "Przeciętna",
                                srednia <= quantile(srednia, 0.33) ~"Niska")) %>%
  select(id_ucznia, D22, przedzialy) %>% 
  filter(D22 != 88, D22 != 99)


# Chcemy jakoś powiązać nastawienie ucznia i jego oceny z innymi jego cechami
# Może z grupy pytań o zainteresowanie podczas wystawy?
# Może pytania W2_3 i W2_4 się nadadzą?

zainteresowanie <- df %>% 
  select(id_ucznia, W2_3, W2_4) %>%  # potrzebujemy oczywiście id_ucznia
  filter(W2_3 != 88, W2_3 != 99, W2_4 != 88, W2_4 != 99) %>%  # usuwamy braki danych
  transmute(id_ucznia = id_ucznia, zainteresowanie = (W2_3 + W2_4) / 2)

# Teraz spróbujemy dodać zainteresowanie do low/medium/high pesimist/optimist

do_wykresu <- oceny %>% 
  inner_join(zainteresowanie, by = "id_ucznia") %>% 
  mutate(Nastawienie = case_when(D22 == 1 ~ "Optymista",
                                D22 == 2 ~ "Pesymista")) %>% 
  select(przedzialy, Nastawienie, zainteresowanie) %>% 
  group_by(przedzialy, Nastawienie) %>% 
  summarise(sr_zainteresowanie = mean(zainteresowanie))

ggplot(do_wykresu, aes(x = przedzialy, y = sr_zainteresowanie, 
                       colour = Nastawienie)) +
  geom_segment(aes(x = przedzialy, xend = przedzialy, y = sr_zainteresowanie, yend = 0),
               position = position_dodge(width = 1)) +
  geom_point(size = 5, position = position_dodge(width = 1)) +
  labs(title = "Zaangażowanie uczniów w wystawę:", 
       x = "Średnia ucznia", 
       y = "Zainteresowanie ucznia",
       caption = "Podział średniej:
                  Wysoka: powyżej 4.33 
                  Przeciętna: 3.33-4.33
                  Niska: poniżej 3.33") +
  scale_y_continuous(expand = c(0,0), limits = c(0,4.5))+
  scale_color_manual(values = c("gray80", "gray20"))+
  coord_flip() +
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
        panel.background = element_rect(fill = "#306457"), 
        legend.key = element_rect(fill = "#306457"))

