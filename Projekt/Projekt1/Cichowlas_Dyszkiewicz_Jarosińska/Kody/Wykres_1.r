library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
df <- read_excel("Naukobus/Naukobus.xlsx")

D22_low <- df %>%
  select(c(id_ucznia,P10_M, P10_JP, P10B, D20:D22)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,2:4], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"high",
                                srednia> quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "medium",
                                srednia <= quantile(srednia, 0.33) ~"low")) %>%
  filter(przedzialy== 'low') %>%
  filter(D22==1 | D22==2 | D22 == 99) %>%
  count(D22)

D22_low$D22 <- c("1.","2.", "3.")

D22_medium <- df %>%
  select(c(id_ucznia,P10_M, P10_JP, P10B, D20:D22)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,2:4], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"high",
                                srednia> quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "medium",
                                srednia <= quantile(srednia, 0.33) ~"low")) %>%
  filter(przedzialy== 'medium') %>%
  filter(D22==1 | D22==2 | D22 == 99) %>%
  count(D22)

D22_medium$D22 <- c("1.","2.", "3.")

D22_high <- df %>%
  select(c(id_ucznia,P10_M, P10_JP, P10B, D20:D22)) %>%
  filter(P10_M > 0 & P10_M<7 & P10_JP > 0 & P10_JP<7 &
           P10B > 0 & P10B<7) %>%
  mutate(srednia = rowMeans(.[,2:4], na.rm = TRUE)) %>%
  mutate(przedzialy = case_when(srednia > quantile(srednia, 0.66) ~"high",
                                srednia> quantile(srednia, 0.33) & srednia <= quantile(srednia, 0.66)  ~ "medium",
                                srednia <= quantile(srednia, 0.33) ~"low")) %>%
  filter(przedzialy == 'high') %>%
  filter(D22==1 | D22==2 | D22 == 99) %>%
  count(D22)

D22_high$D22 <- c("1.","2.", "3.")

p3_low <- ggplot(D22_low, aes(x= D22, y =n)) +
  geom_col(fill= 'gray20') +
  labs(title = "Osoby z niską ?rednią ocen:", 
       x = "Możliwe odpowiedzi", 
       y = "Liczba odpowiedzi") +
  scale_y_continuous(expand = c(0, 0))  +
  theme(plot.background = element_rect(fill = "#306457"), 
        plot.title = element_text(color = "white"), 
        plot.caption = element_text(color = "white"), 
        axis.title.x = element_text(color = "white"), 
        axis.title.y = element_text(color = "white"),
        axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"), 
        panel.background = element_rect(fill = "#306457"))

p3_medium <- ggplot(D22_medium, aes(x = D22, y = n)) +
  geom_col(fill= 'gray50') +
  labs(title = "Osoby z przeciętną średnią ocen:", 
       x = "Możliwe odpowiedzi", 
       y = "Liczba odpowiedzi") +
  scale_y_continuous(expand = c(0, 0))  +
  theme(plot.background = element_rect(fill = "#306457"), 
        plot.title = element_text(color = "white"), 
        plot.caption = element_text(color = "white"), 
        axis.title.x = element_text(color = "white"), 
        axis.title.y = element_text(color = "white"),
        axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"), 
        panel.background = element_rect(fill = "#306457"))

p3_high <- ggplot(D22_high, aes(x= D22, y =n)) +
  geom_col(fill= 'gray80') +
  labs(title = "Osoby z wysoką średnią ocen:", 
       x = "Możliwe odpowiedzi", 
       y = "Liczba odpowiedzi") +
  scale_y_continuous(expand = c(0, 0))  +
  theme(plot.background = element_rect(fill = "#306457"), 
        plot.title = element_text(color = "white"), 
        plot.caption = element_text(color = "white"), 
        axis.title.x = element_text(color = "white"), 
        axis.title.y = element_text(color = "white"),
        axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"), 
        panel.background = element_rect(fill = "#306457"))

legend <-  grobTree(rectGrob(gp = gpar(fill = "#306457")), 
textGrob("Moje zarobki: 
1) będą wyższe niż moich rówieśników;
2) będą niższe niż moich rówieśników;
3) brak odp.

Podział średniej:
Wysoka: powyżej 4.33 
Przeciętna: 3.33-4.33
Niska: poniżej 3.33", gp = gpar(col = "white")))

plot <- grid.arrange(p3_low,p3_medium,p3_high, legend, widths = c(1,1), 
             top = grobTree(rectGrob(gp = gpar(fill = "#306457")), 
                            textGrob("Czy oceny maja wpływ na poczucie własnej wartości?", 
                                     gp = gpar(col = "white", fontsize = 15))))



