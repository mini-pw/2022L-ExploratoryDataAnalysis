library(dplyr)
library(haven)
library(ggplot2)
library(fmsb)
df <- read_sav("ROSES.sav")

# wybranie potrzebnych danych dla grupy metafizyków
df %>% 
  filter(df[, 57] == 4, df[, 58] == 4, df[, 60] == 4, df[, 61] == 4, 
         df[, 84] <= 2, df[, 93] <= 2) %>% 
  select(c(26, 30, 34, 28, 89)) %>% 
  sapply(as.integer) %>% 
  colMeans(na.rm = TRUE) %>% 
  as.data.frame %>% 
  t() -> df3
colnames(df3) <- c("chmury, deszcz, pogoda", "atomy i cząsteczki", "czarne dziury, supernowe", "dziedziczenie i geny", "wpływ internetu na społeczeństwo")
df3 <- as.data.frame(rbind(rep(4, 5), rep(0, 5), df3))

# utworzenie radarchartu
radarchart( df3  , axistype=1 , 
            
            #custom polygon
            pcol=adjustcolor( "blue", alpha.f = 0.8) , pfcol=adjustcolor( "blue", alpha.f = 0.2) , plwd=3 , 
            
            #custom the grid
            cglcol=adjustcolor( "blue4", alpha.f = 0.7), cglty=1, axislabcol=adjustcolor( "blue4", alpha.f = 0.7), caxislabels=seq(0,4,1), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)
