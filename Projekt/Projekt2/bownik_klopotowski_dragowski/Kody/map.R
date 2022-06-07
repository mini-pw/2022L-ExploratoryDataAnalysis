# library(dplyr)
# library(ggplot2)
# library(plotly)

df <- read.csv("dane/share-global-food-emissions.csv")

colnames(df)[4] <- "Value"

df <- df %>% 
  filter(Entity != "Developing", Entity != "Industrialized")

figmap <- plot_geo(data = df, 
         locationmode = "country names",
         frame = ~Year) %>% 
  add_trace(locations = ~Entity,
            z = ~Value,
            zmin = 0, # min(df$Value) + margines
            zmax = 15, # max(df$Value) + margines
            color = ~Value,
            colorscale = "agsunset",
            colorbar = list(x = 1.01, y = 0.75, title = "Percent of\nglobal emissions",
                     ticksuffix = "%")) %>%
  animation_slider(y = 0.25) %>% animation_button(y = 0.25) %>% 
  layout( title = list(y = 0.85, font = list(size = 20),
    text = "Share of global CO2 equivalents emisions from food production"))

figmap
