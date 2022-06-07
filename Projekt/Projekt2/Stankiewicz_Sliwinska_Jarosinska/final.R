library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(tidyr)
library(dplyr)
library(shinydashboard)
library(readxl)
library(plotly)

df <- read_excel("C:/Users/monik/OneDrive/Dokumenty/Eksploracja/Projekt 2/emdat_dane.xlsx")

types <- df %>% 
  filter(`Disaster Type` %in% c("Drought","Flood","Earthquake","Wildfire","Volcanic activity"))

df6 <- df %>% 
  filter(Year >= 1960) %>% 
  select(rok = Year, Disaster_Subgroup =`Disaster Subgroup`) %>%
  filter(Disaster_Subgroup != "Complex Disasters" & Disaster_Subgroup != "Extra-terrestrial") %>%
  group_by(rok, Disaster_Subgroup) %>% 
  count() 

introduction <- tabItem(tabName = "introduction", fluidRow(box("Many people at least have heard about wildfires in Australia, hurricanes in USA, tsunamis in Japan. Intuition suggests that number of disasters is increasing year by year. The question is: what numbers say about our intuition? With increasing global temperatures natural disasters such as droughts, floods and other are more likely to occur. Moreover, the intensity of them also increases. The application is designed to show the climate change over the years, focusing on increase in the number of natural disasters.
", width = 12, background = "purple", style = 'font-size:20px;',img(src = "natural-disasters.png", height = 400, width = 1230))
))
main <- tabItem(tabName = "main",
                fluidRow(
                ),
                
  
    fluidRow(
        box(width = 4, height = 461, background = "purple",style = 'font-size:18px;', selectInput("Disaster_Type", "Choose disaster type:", unique(types$`Disaster Type`)), textOutput("text2"),
  
    ),
        box(width = 8, title = "Disasters all over the world", plotlyOutput("map"), background = "purple")
    ),
    fluidRow(
        box(width = 8, title = "Disasters all over the world", plotlyOutput("wykres1"), background = "purple"),
        box(width = 4, height = 461,style = 'font-size:18px;', textOutput("text3"), background = "purple"),
    ),
    
    fluidRow(
        box(width = 6, title = 'Natural disasters between 1950-1960', plotlyOutput("pointmap5060"), background = "purple"),
        box(width = 6, title = 'Natural disasters between 2010-2020', plotlyOutput("pointmap1020"), background = "purple")
    ),
    fluidRow(
        box(width = 12, background = "purple", style = 'font-size:18px;',  textOutput("text6")),
    ),
    fluidRow(
        box(width = 4,height = 461, background = "purple", style = 'font-size:18px;', sliderInput("rok",
                                   "Which year:",
                                   min = as.numeric(min(df6$rok)),
                                   max = as.numeric(max(df6$rok)),
                                   value = c(as.numeric(min(df6$rok)),as.numeric(max(df6$rok)))), textOutput("text4")
    ), 
    
        box(width = 8, title = textOutput("tytul"), plotOutput("wykres2"), background = "purple")
    ),
    fluidRow(
        box(width = 12, background = "purple",style = 'font-size:18px;', textOutput("text5"))
            
        )
    )
    
body <- dashboardBody(tabItems(introduction, main))
sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Introduction", tabName = "introduction"),
              menuItem("Main", tabName = "main")
  ))

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Natural disasters"),
                    sidebar, body
)


server <- function(input, output) {
    
    output$map <- renderPlotly({
        scl = list(c(0, "yellow"), list(1, "red"))
        
        df2 <- df %>% 
            select("Year", "Disaster Group":"Disaster Subtype", "Country":"Continent", "Latitude", "Longitude")
        
        df2$`Disaster Subtype`[is.na(df2$`Disaster Subtype`)] <- "" 
        
        df2 <- df2 %>% 
            group_by(Year, ISO, `Disaster Type`) %>%
            summarise(n = n()) %>% 
            group_by(`Disaster Type`) 
        
        df3<-df2[df2$`Disaster Type`== input$Disaster_Type,]
        
        p <- plot_geo(df3, locationmode = 'world') %>%
            add_trace(z = ~df3$n, locations = df3$ISO, frame = ~df3$Year,
                      color = ~df3$n, zmin = 0, zmax=max(df2$n) - 3, colorscale = scl, colorbar = list(title = "Number of disasters")) %>% 
            animation_slider(
                currentvalue = list(prefix = "Year: ", font = list(color="darkblue"))) 
        
    })
    
    output$text2 <- renderText({"The map shows the number of particular disasters in different places in the selected year from 1950 to 2022. It can clearly be seen that numbers increase over time.
        Worth noticing is fact that not only does number of disasters increase, but also the number of different places where disasters appear increases."
        
    })
    
    output$text3 <- renderText({"This plot shows total numbers of catastrophies in each year, grouped by type of disaster. Dates nad catastrophies types that are points of interest can be chosen and compared. 
      The obvious conlusion is significant rise in number of catastrophies."
      
    })
    
    output$text4 <- renderText({"On the last chart, there is shown total number of disasters grouped by their subtype in selected range of years. It allows to compare the frequency of appereance of certain types of disasters. 
      We can see on which disaster subtypes global warming and climate changes might have had the biggest impact. "
      })
    
    output$text5 <- renderText({"To conclude, all of the above presented plots indicate significant rise in number of natural disasters occuring in the world.
            Most of the noticed increase is a result of climate change, we are currently struggling with. 
            Due to the anthropogenic impact on the environment and greenhouse effect, the climate is rapidly changing, thus nowadays we can observe its consequences in the form of natural disasters.
            The results of our application confirm that climate change is an issue of great importance, that requires to be attended."
      })
    output$text6 <- renderText({
      "The maps above show the locations of various natural disasters, in years 1960-1970 and 2010-2020. 
    Additionally, there is a division of disasters into groups and types.
    Comparing both maps, the increase in the number of natural catastrophes over the years is clearly visible. 
        Furthermore, natural disasters can also be noticed in places where they did not occur previously."
    })
    output$pointmap5060 <- renderPlotly({
      df4 <- df %>% 
        filter(Year >= 1960 & Year <= 1970) %>% 
        filter(`Disaster Type` %in% c("Earthquake","Flood","Wildfire","Landslide", "Storm", "Extreme temperatures",
                                      "Drought","Glacial lake out burst", "Mass movement (dry))")) %>% 
        select("Year", "Disaster Group":"Disaster Subtype", "Country":"Continent", "Latitude", "Longitude")
      
      df4$`Disaster Subtype`[is.na(df4$`Disaster Subtype`)] <- "" 
      g <- list(
        scope = 'world',
        visible = F,
        showcountries = T,
        countrycolor = toRGB("Black"),
        resolution = 50,
        showland = TRUE,
        landcolor = toRGB("#e5ecf6")
      )
      p <- plot_geo(df4, colors = "red")
      p <- p %>% 
        add_markers(
          x = ~Longitude, y = ~Latitude, hoverinfo = "text", marker = list(color = "purple"),
          text = ~paste(df4$`Disaster Subgroup`, ", ", df4$`Disaster Subtype`)
        ) %>% 
        layout(geo = g)
      p
      
    })
    
    
    output$pointmap1020 <- renderPlotly({
      df4 <- df %>% 
        filter(Year >= 2010 & Year <= 2020) %>% 
        filter(`Disaster Type` %in% c("Earthquake","Flood","Wildfire","Landslide", "Storm", "Extreme temperatures",
                                      "Drought","Glacial lake out burst", "Mass movement (dry))")) %>% 
        select("Year", "Disaster Group":"Disaster Subtype", "Country":"Continent", "Latitude", "Longitude")
      
      df4$`Disaster Subtype`[is.na(df4$`Disaster Subtype`)] <- "" 
      g <- list(
        scope = 'world',
        visible = F,
        showcountries = T,
        countrycolor = toRGB("Black"),
        resolution = 50,
        showland = TRUE,
        landcolor = toRGB("#e5ecf6")
      )
      p <- plot_geo(df4, colors = "Set1")
      p <- p %>% 
        add_markers(
          x = ~Longitude, y = ~Latitude, hoverinfo = "text", marker = list(color = "blue"),
          text = ~paste(df4$`Disaster Subgroup`, ", ", df4$`Disaster Subtype`)
        ) %>% 
        layout(geo = g)
      p
      
    })
    
    output$wykres1 <- renderPlotly({
      df5 <- df %>%
        select(Year, `Disaster Type`, Country, ISO) %>%
        filter(`Disaster Type` %in% c("Earthquake","Flood","Wildfire","Landslide", "Storm", "Extreme temperatures",
                                      "Drought","Glacial lake out burst", "Mass movement (dry))")) %>%
        group_by(Year, `Disaster Type`) %>%
        summarise(count = n()) %>%
        pivot_wider(names_from = "Disaster Type", values_from = count)
      
      fig <- plot_ly(df5, x = ~df5$Year, y = ~Earthquake, type = 'bar', name = 'Earthquake', marker = list(color = c("009900"))) %>% 
        add_trace(y = ~Flood, name = 'Flood', marker = list(color = c("3399FF"))) %>%
        add_trace(y = ~Storm, name = 'Storm', marker = list(color = c("666666"))) %>%
        add_trace(y = ~Wildfire, name = 'Wildfire', marker = list(color = c("FF0000"))) %>%
        add_trace(y = ~Drought, name = 'Drought', marker = list(color = c("FFCC33"))) %>%
        add_trace(y = ~Landslide, name = 'Landslide', marker = list(color = c("CC6600"))) %>%
        add_trace(y = ~'Extreme temperatures', name = 'Extreme temperatures',marker = list(color = c("CC0099"))) %>%
        add_trace(y = ~'Glacial lake out burst', name = 'Glacial lake out burst', marker = list(color = c("33FFFF"))) %>%
        add_trace(y = ~'Mass movement (dry))', name = 'Mass movement', marker = list(color = c("000000"))) %>%
        layout(yaxis = list(title = 'Count'), xaxis = list(title = "Choose range:"),
               barmode = 'stack') %>%
        layout(xaxis = list(rangeslider = list(type = "date")))
      fig
    })
    
    output$wykres2 <- renderPlot({
        
        
        wykres_oki <- ggplot(df6[df6$rok >= min(input$rok) & df6$rok <= max(input$rok),], 
                             aes(x = Disaster_Subgroup, y = n)) +
            geom_col(fill = "navy") +
            labs(x = "disaster subgroups", y = "number") +
            theme(legend.position = "None", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black")) +
            theme_bw() +
            scale_y_continuous(limits = c(0, 2500))
          
        wykres_oki
        
        
    })
    output$tytul <- renderText({
      paste("Change in the quantity of natural disasters through years", input$rok[1], "-", input$rok[2])
    })
    
}
# Preview the UI in the console
shinyApp(ui = ui, server = server)