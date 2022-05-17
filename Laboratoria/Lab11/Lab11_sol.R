library(shiny)
library(ggplot2)
library(dplyr)
library(PogromcyDanych)

ui <- shinyUI(fluidPage(
  titlePanel("Seriale IMDB"),
  sidebarLayout(
    sidebarPanel(
      selectInput("serial", "Wybierz serial", unique(serialeIMDB$serial)),
      checkboxInput("liniaTrendu", "Linia trendu", FALSE)
    ),
    mainPanel(
      plotOutput("pointPlot"),
      tableOutput("table")
    )
  )
))

server <- shinyServer(function(input, output) {
  
  output$pointPlot <- renderPlot({
    
    p <- ggplot(serialeIMDB[serialeIMDB$serial == input$serial, ], aes(x = id, y = ocena, color = sezon)) +
      geom_point() +
      labs(x = "Odcinek",
           y = "Ocena",
           color = "Sezon",
           title = paste0("Oceny dla serialu ", '"', input$serial, '"'))
    
    if (input$liniaTrendu){
      p <- p + geom_smooth(se = FALSE)
    }
    
    p
    
  })
  
  output$table <- renderTable({
    
    serialeIMDB %>% 
      filter(serial == input$serial) %>% 
      group_by(sezon) %>% 
      summarise(srednia = mean(ocena, na.rm = TRUE))
    
  })
  
})


shinyApp(ui, server)
