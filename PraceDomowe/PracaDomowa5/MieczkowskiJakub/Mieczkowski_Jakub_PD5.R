#
# library(shiny)
library(ggplot2)
library(dplyr)
ui <- shinyUI(fluidPage(
  titlePanel("Mieszkania Seattle"),
  sidebarLayout(
    sidebarPanel(
      selectInput("zipcode", "Wybierz kod pocztowy", unique(house_data$zipcode)),
      checkboxInput("liniaTrendu", "Linia trendu", FALSE)
    ),
    mainPanel(
      plotOutput("pointPlot"),
      plotlyOutput("barp")
     
      
    )
  )
))

server <- shinyServer(function(input, output) {
  
  output$pointPlot <- renderPlot({
    
    p <- ggplot(house_data[house_data$zipcode == input$zipcode, ], aes(x = sqft_living, y = price, color = bathrooms)) +
      geom_point() +
      labs(x = "Powierzchnia",
           y = "Cena",
           title = paste0("Zaleznosc ceny od powierzchni dla mieszkan o kodzie pocztowym: ", '', input$zipcode, ''))
    
    if (input$liniaTrendu){
       p <- p + geom_smooth(se = FALSE)
     }
    
    p
    
    
  })
  
  output$barp<- renderPlotly({
    
  htab <- house_data %>% 
    filter(yr_built > 1999, bedrooms < 8)
  htab2 <- htab %>% 
    group_by(yr_built, bedrooms, .add = TRUE) %>% 
    summarise(sr_cena = mean(price))
  
  plot_ly(
    data = htab2,
    x=~yr_built,
    y=~sr_cena,
    frame=~bedrooms,
    type = "bar"
  )%>% 
    layout(title="Srednia cena mieszkan z danego roku:") %>% 
    animation_opts(4000) %>%
    animation_button(x = 0.05, y = 0.15) %>%
    animation_slider(currentvalue = list(prefix = "Liczba sypialni: ", font = list(color="red")))
      
})
})

shinyApp(ui, server)
