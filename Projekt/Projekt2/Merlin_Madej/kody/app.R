library(shiny)
library(plotly)
library(dplyr)
animals_slaughtered_for_meat <- read.csv('animals-slaughtered-for-meat.csv')
meat<-read.csv('per-capita-meat-consumption-by-type-kilograms-per-year.csv',sep = ',' )
colnames(animals_slaughtered_for_meat) <- c("Entity","Code", "Year", "Cattle", "Goats", "Chicken", "Turkey", "Pigs", "Sheep")
animals_slaughtered_for_meat <- animals_slaughtered_for_meat[!is.na(animals_slaughtered_for_meat$Code),]
meat <- meat[!is.na(meat$Code),]

colnames(meat)<-c("Entity","Code","Year","Mutton & Goat","Other meat","Poultry","Pigmeat","Cattle")


ui<-fluidPage(
  h4("The global production of food is responsible for a third of all planet-heating gases emitted by human activity, with the use of animals for meat causing twice the pollution of producing plant-based foods. The entire system of food production, such as the use of farming machinery, spraying of fertilizer and transportation of products, causes 17.3bn metric tonnes of greenhouse gases a year. This enormous release of gases that fuel the climate crisis is more than double the entire emissions of the US and represents 35% of all global emissions"),
  fluidRow(
    column(12,
           "Meat consumption in the world",
           fluidRow(
             column(6,
                    plotlyOutput("glob"),
                    fluidRow(
                      column(6, 
                             selectInput("gatunek",
                                         "Species:",
                                         choices =  c("Cattle","Goats", "Chicken", "Turkey","Pigs", "Sheep" ),
                                         selected = "Cattle") ),
                      column(6,
                             sliderInput("zakres",
                                         "Year:",
                                         value =  min(animals_slaughtered_for_meat$Year),
                                         min = min(animals_slaughtered_for_meat$Year),
                                         max = max(animals_slaughtered_for_meat$Year),
                                         step = 1,
                                         animate = animationOptions(interval = 800)))
                    ),
                    h4("Map above represents number of slaughtered animals of chosen species by country"),
                    
             ),
             
             column(6,
                    plotlyOutput("glob1"),
                    fluidRow(
                      column(6, 
                             selectInput("typ",
                                         "Species:",
                                         choices =  c("Mutton & Goat", "Other meat" ,"Poultry","Pigs","Cattle"),
                                         selected = "Cattle")),
                      column(6,
                             sliderInput("zakres1",
                                         "Year:",
                                         value = min(meat$Year),
                                         min = min(meat$Year),
                                         max = max(meat$Year),
                                         step = 1,
                                         animate = animationOptions(interval = 800)))
                    )
             ),
             h4("Map above represents consumption per capita of chosen species by country")
            
           )
    )
    
  ),
  h2("   "),
  h4("Table represents summary of the meat consumption in countries per capita in the chosen time period"),
  sliderInput("przed",
              "Time period:",
              value = c(min(meat$Year), max(meat$Year)),
              min = min(meat$Year),
              max = max(meat$Year),
              step = 1),
  DT::dataTableOutput("mytable"),
  

  h4("Australia, USA and New Zeland consumed the most meat throughout the years. These countries has had the greatest impact on the enviroment. Only the USA produced 698 million metric tons of CO2 in 2018. In recent years Hong kong became the biggest meat consumer.Also if every person in the U.S. cut their meat consumption by 25 percent, it would reduce annual greenhouse gas emissions by 1 percent. That might not sound like a lot, but it would help protect the rain forest, so the positive effects—including reduced water and fertilizer use, improved biodiversity and safeguarded rights of Indigenous peoples—would be amplified.")
)









server <- function(input, output) {
  output$glob1 <- renderPlotly({
    meat1<-meat %>% 
      filter(Year == input$zakres1)
    if (input$typ == "Mutton & Goat"){
      plot_ly(data = meat1,
              type = 'choropleth',
              locations = ~Code,
              z = meat1$`Mutton & Goat`,
              zmin = 0,
              zmax = 61.34,
              text = ~Entity,
              colorscale = "Reds") %>% 
        layout(title = "Mutton and goat meat consumption per capita per country in kgs",
               geo = list(projection = list(type = 'natural earth')))
    } 
    else if (input$typ == "Other meat"){
      plot_ly(data = meat1,
              type = 'choropleth',
              locations = ~Code,
              z = meat1$`Other meat`,
              zmin = 0,
              zmax = 65.18,
              text = ~Entity,
              colorscale = "Reds") %>% 
        layout(title = "Other meat consumption per capita per country in kgs",
               geo = list(projection = list(type = 'natural earth')))
    }else if (input$typ == "Poultry"){
      plot_ly(data = meat1,
              type = 'choropleth',
              locations = ~Code,
              z = meat1$Poultry,
              zmin = 0,
              zmax = 87.82,
              text = ~Entity,
              colorscale = "Reds") %>% 
        layout(title = "Poultry consumption per capita per country in kg",
               geo = list(projection = list(type = 'natural earth')))
    }else if (input$typ == "Pigs"){
      plot_ly(data = meat1,
              type = 'choropleth',
              locations = ~Code,
              z = meat1$Pigmeat,
              zmin = 0,
              zmax = 77.93,
              text = ~Entity,
              colorscale = "Reds") %>% 
        layout(title = "Pigmeat consumption per capita per country in kg",
               geo = list(projection = list(type = 'natural earth')))
    }else if (input$typ == "Cattle"){
      plot_ly(data = meat1,
              type = 'choropleth',
              locations = ~Code,
              z = meat1$Cattle,
              zmin = 0,
              zmax = 93.25,
              text = ~Entity,
              colorscale = "Reds") %>% 
        layout(title = "Cattle consumption per capita per country in kg",
               geo = list(projection = list(type = 'natural earth')))}
  })
  
  output$glob <- renderPlotly({
    df <- animals_slaughtered_for_meat %>% 
      filter(Year == input$zakres)
    if (input$gatunek == "Cattle"){
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Cattle,
              zmin = 0,
              zmax = 48726000,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of cattle slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
    }
    else if (input$gatunek == "Goats") {
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Goats,
              zmin = 0,
              zmax = 162761966,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of goats slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
    }
    else if (input$gatunek == "Chicken") {
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Chicken,
              zmin = 0,
              zmax = 10510737000,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of chickens slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
    }
    else if (input$gatunek == "Turkey") {
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Turkey,
              zmin = 0,
              zmax = 293290000,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of turkeys slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
    }
    else if (input$gatunek == "Pigs") {
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Pigs,
              zmin = 744899496,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of pigs slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
    }
    else if (input$gatunek == "Sheep") {
      plot_ly(data = df,
              type = 'choropleth',
              locations = ~Code,
              z = ~Sheep,
              zmin = 0,
              zmax = 144265012,
              text = ~Entity,
              colors = "Reds") %>% 
        layout(title = "Number of sheep slaughtered per country",
               geo = list(projection = list(type = 'natural earth')))
      
      # plotproxy <- plotlyProxy("glob", session, deferUntilFlush = FALSE)
      
    }
    
  })

  
  output$mytable = DT::renderDataTable({
    jak <- meat
    jak$Suma <-rowSums(jak[, c(4:8)], na.rm = TRUE)
    
    g <- jak %>% 
      select(Entity, Year, Suma) %>%
      filter(Year >= input$przed[1],
             Year <= input$przed[2]) %>% 
      group_by(Entity) %>% 
      summarise(Sum_avg = round(mean(Suma)),
                Sum_min = min(Suma),
                Sum_max = max(Suma))
    
    
    g
  })
  
  
}


shinyApp(ui = ui, server = server)