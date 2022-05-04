library(dplyr)
library(haven)
library(ggplot2)
library(fmsb)
df <- read_sav("ROSES.sav")

# wybranie potrzebnych danych dla grupy naukowców
df %>% 
  filter(df[, 93] >= 3, df[, 96] >= 3, df[, 60] <= 2, df[, 84] >= 3) %>% 
  select(c(26, 30, 34, 28, 89)) %>% 
  sapply(as.integer) %>% 
  colMeans(na.rm = TRUE) %>% 
  as.data.frame %>% 
  t() -> df4
colnames(df4) <- c("chmury, deszcz, pogoda", "atomy i cząsteczki", "czarne dziury, supernowe", "dziedziczenie i geny", "wpływ internetu na społeczeństwo")
df4 <- as.data.frame(rbind(rep(4, 5), rep(0, 5), df4))

# utworzenie radarchartu
radarchart( df4  , axistype=1 , 
            
            #custom polygon
            pcol=adjustcolor( "#3a754e", alpha.f = 0.8) , pfcol=adjustcolor( "#3a754e", alpha.f = 0.2) , plwd=3 , 
            
            #custom the grid
            cglcol=adjustcolor( "#205c37", alpha.f = 0.7), cglty=1, axislabcol=adjustcolor( "#205c37", alpha.f = 0.7), caxislabels=seq(0,4,1), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)