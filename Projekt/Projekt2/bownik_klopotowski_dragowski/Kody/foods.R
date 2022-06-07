# library(dplyr)
# library(plotly)
# library(tidyr)

emis_kcal <- read.csv("dane/eutrophying-emissions_kcal.csv")
emis_kg <- read.csv("dane/eutrophying-emissions_kg.csv")
emis_protein <- read.csv("dane/eutrophying-emissions_protein.csv")
land_kcal <- read.csv("dane/land_kcal.csv")
land_kg <- read.csv("dane/land_kg.csv")
land_protein <- read.csv("dane/land_protein.csv")
water_kcal <- read.csv("dane/water_kcal.csv")
water_kg <- read.csv("dane/water_kg.csv")
water_protein <- read.csv("dane/water_protein.csv")

colnames(emis_kg)[4] <- "emis_per_kg"
colnames(emis_kcal)[4] <- "emis_per_kcal"
colnames(emis_protein)[4] <- "emis_per_protein"
colnames(land_kcal)[4] <- "land_per_kcal"
colnames(land_protein)[4] <- "land_per_protein"
colnames(land_kg)[4] <- "land_per_kg"
colnames(water_kcal)[4] <- "water_per_kcal"
colnames(water_protein)[4] <- "water_per_protein"
colnames(water_kg)[4] <- "water_per_kg"

items <- c(emis_kcal$Entity, emis_kg$Entity, emis_protein$Entity,
         land_kcal$Entity, land_kg$Entity, land_protein$Entity,
         water_kcal$Entity, water_kg$Entity, water_protein$Entity)
items <- data.frame(table(items))

items <- items %>% 
  filter(Freq == 9) %>% 
  select(items)

emis_kcal <- emis_kcal %>% 
  filter(Entity %in% items$items)
emis_kg <- emis_kg %>% 
  filter(Entity %in% items$items)
emis_protein <- emis_protein %>% 
  filter(Entity %in% items$items)
land_kcal <- land_kcal %>% 
  filter(Entity %in% items$items)
land_kg <- land_kg %>% 
  filter(Entity %in% items$items)
land_protein <- land_protein %>% 
  filter(Entity %in% items$items)
water_kcal <- water_kcal %>% 
  filter(Entity %in% items$items)
water_kg <- water_kg %>% 
  filter(Entity %in% items$items)
water_protein <- water_protein %>% 
  filter(Entity %in% items$items)
  
#emisje

emis <- data.frame(emis_kcal$Entity, emis_kcal$emis_per_kcal, 
                   emis_kg$emis_per_kg, emis_protein$emis_per_protein)
colnames(emis) <- c("Entity", "1000 kcal", "1 kg", "100g protein")

emis <- emis %>%
  pivot_longer(!Entity, names_to = "per", values_to = "emis") %>% 
  mutate( emis = round(emis, 2) )


fig_emis <- plot_ly(data = emis %>% 
          filter(Entity == "Apples"),
        x = ~emis,
        y = ~per,
        name = "Emissions",
        type = "bar",
        marker = list(color = "#46b871"))  

for (i in items$items[2:28]){
  fig_emis <- fig_emis %>% 
    add_trace(data = emis %>% 
                filter(Entity == i),
              x = ~emis,
              y = ~per,
              name = "Emissions",
              type = "bar",
              marker = list(color = "#46b871"),
              visible = FALSE)
}

fig_emis <- fig_emis %>% 
  layout(xaxis = list(range = c(0, 370), gridcolor = "black"))

#land

land <- data.frame(land_kcal$Entity, land_kcal$land_per_kcal, 
                   land_kg$land_per_kg, land_protein$land_per_protein)
colnames(land) <- c("Entity", "1000 kcal", "1 kg", "100g protein")

land <- land %>%
  pivot_longer(!Entity, names_to = "per", values_to = "land") %>% 
  mutate( land = round(land, 2) )


fig_land <- plot_ly(data = land %>% 
                      filter(Entity == "Apples"),
                    x = ~land,
                    y = ~per,
                    name = "Land use",
                    type = "bar",
                    marker = list(color = "#453737"))  

for (i in items$items[2:28]){
  fig_land <- fig_land %>% 
    add_trace(data = land %>% 
                filter(Entity == i),
              x = ~land,
              y = ~per,
              name = "Land use",
              type = "bar",
              marker = list(color = "#453737"),
              visible = FALSE)
}

fig_land <- fig_land %>% 
  layout(xaxis = list(range = c(0, 370), gridcolor = "black"))

#woda

water <- data.frame(water_kcal$Entity, water_kcal$water_per_kcal, 
                    water_kg$water_per_kg, water_protein$water_per_protein)
colnames(water) <- c("Entity", "1000 kcal", "1 kg", "100g protein")

water <- water %>% 
  pivot_longer(!Entity, names_to = "per", values_to = "water") %>% 
  mutate( water = round(water, 2) )

fig_water <- plot_ly(data = water %>% 
                      filter(Entity == "Apples"),
                    x = ~water,
                    y = ~per,
                    name = "Water use",
                    type = "bar",
                    marker = list(color = "#66c9d4"))  

for (i in items$items[2:28]){
  fig_water <- fig_water %>% 
    add_trace(data = water %>% 
                filter(Entity == i),
              x = ~water,
              y = ~per,
              name = "Water use",
              type = "bar",
              marker = list(color = "#66c9d4"),
              visible = FALSE)
}

fig_water <- fig_water %>% 
  layout(xaxis = list(range = c(0, 6050), gridcolor = "black"))

# połączenie i podpisy

annotations = list(
  list( 
    x = 0.5,  
    y = 1.0,  
    text = "Grams of phosphate-equivalents per:",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  
  list( 
    x = 0.5,  
    y = 0.64,  
    text = "Square meters of land use per:",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  
  list( 
    x = 0.5,  
    y = 0.3,  
    text = "Liters of water use per:",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  )
)

figcol <- subplot(fig_emis, fig_land, fig_water, nrows = 3, margin = 0.05) %>% 
  layout(annotations = annotations)

t_or_f <- rep(FALSE, 28)
t_or_f[1] <- TRUE

p <- list(
  list(method = "restyle",
       args = list(list(visible = t_or_f)),
       label = "Apples"))

for (i in 2:28){
  t_or_f <- rep(FALSE, 28)
  t_or_f[i] <- TRUE
  
  p <- append(p, list(list(method = "restyle",
                      args = list(list(visible = t_or_f)),
                      label = items$items[i])))
}


figcol <- figcol %>% 
  layout( showlegend = FALSE,
    updatemenus = list(
      list(
        x = 1,
        y = 1.1,
        buttons = p)
    ))

figcol

   